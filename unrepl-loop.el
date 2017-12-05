;;; unrepl-loop.el --- UNREPL EDN messages processing -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-loop.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Nov 11 20:07:16 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Processing of UNREPL EDN messages.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'arc-mode)
(require 'parseclj)
(require 'subr-x)
(require 'treepy)

(require 'unrepl-ast)
(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-repl)


(defvar-local unrepl-loop-process-type nil
  "Type of process.
This local variable is meant to be set in conn-pool processes' buffers so
that they are easily distinguishable.")

(defvar-local unrepl-loop-process-dispatcher nil
  "The EDN message dispatcher function for a process buffer.")

(defvar-local unrepl-loop-process-output-start 1
  "The start of the most recent output UNREPL message.")

(defvar-local unrepl-loop-greeted-p nil
  "Predicate that defines if the client for the current buffer has been greeted already.")


(defun unrepl-loop--announce-greeting-p (process)
  "Decide whether or not to announce this PROCESS greetings."
  (with-current-buffer (process-buffer process)
    (eql unrepl-loop-process-type :client)))


(defun unrepl-loop--send (conn-id proc-type str &optional no-line-break)
  "Send input STR to PROC-TYPE of CONN-ID.
PROC-TYPE is a keyword, either `:client', `:aux', or `:side-loader'.
By default, this function will add a new line after STR.  NO-LINE-BREAK
overrides this behavior."
  (let* ((project (unrepl-projects-get conn-id))
         (proc (unrepl-project-conn-pool-get-process project proc-type)))
    (process-send-string proc (concat str (unless no-line-break "\n")))
    str))


(defun unrepl-loop--destructure-message-ast (msg-node)
  "Traverse MSG-NODE and return its UNREPL tag, payload and group-id.
Tag is returned as a keyword.
Payload is returned as a parseclj AST node.
Group-id is returned as an integer."
  (let* ((zp (unrepl-ast-zip msg-node))
         (tag (-> zp
                  (treepy-down)
                  (treepy-node)
                  (parseclj-ast-value)))
         (payload (-> zp
                      (treepy-down)
                      (treepy-right)
                      (treepy-node)))
         (group-id (-> zp
                       (treepy-down)
                       (treepy-right)
                       (treepy-right)
                       (treepy-node)
                       (parseclj-ast-value))))
    (list tag payload group-id)))


(defun unrepl-loop-handle-proc-message (process output)
  "Decode OUTPUT's EDN messages from PROCESS, and dispatch accordingly."
  (let ((proc-buf (process-buffer process)))
    (with-current-buffer proc-buf
      (unless unrepl-loop-greeted-p
        (when-let (hello-match (string-match-p "\\[:unrepl.*/hello" output))
          (setq output (substring output hello-match))
          (setq-local unrepl-loop-greeted-p t)
          (when (unrepl-loop--announce-greeting-p process)
            (message "UNREPL says hi!"))))
      (when unrepl-loop-greeted-p
        (goto-char (point-max))
        (insert output)

        ;; There can either be several EDN messages in OUTPUT, or an incomplete
        ;; message.
        ;; If output ends in ]\n we assume it is complete, and we iterate over
        ;; all possible forms in it.
        (when (string-suffix-p "]\n" (buffer-substring-no-properties unrepl-loop-process-output-start
                                                                     (point-max)))
          (goto-char unrepl-loop-process-output-start)
          (mapc (lambda (msg-ast-node)
                  (apply unrepl-loop-process-dispatcher
                         unrepl-conn-id
                         (unrepl-loop--destructure-message-ast msg-ast-node)))
                (parseclj-ast-children (parseclj-parse-clojure)))
          (setq-local unrepl-loop-process-output-start (point-max)))))))



;; Client Process
;; =============================================================================

(defun unrepl-client-send (str &optional eval-callback stdout-callback buffer)
  "Send input STR to UNREPL client connection.
EVAL-CALLBACK is a function that takes the evaluation payload and displays
it in any given way.
STDOUT-CALLBACK is a function that takes any output payload taken from this
evaluation and process it in any given way.
If the given STR input is interactive, BUFFER gets saved into the new
pending evaluation so that it can be moved to said buffer as 'latest
evaluation' before being discarded from the pending evaluations queue.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (prog1 (unrepl-loop--send unrepl-conn-id :client str)
    (unrepl-pending-eval-add :client unrepl-conn-id
                             :status :sent
                             :input str
                             :buffer buffer
                             :eval-callback eval-callback
                             :stdout-callback stdout-callback)))


(defun unrepl-loop-client-dispatcher (conn-id tag payload &optional group-id)
  "Dispatch an UNREPL message to an `unrepl-loop--client-*' message handler.
CONN-ID is provided to client message handlers so they know which
project/repl to modify.
TAG is the UNREPL tag, and it's used to select the handler function for the
message.
PAYLOAD is a parseclj AST node of the message's payload.
GROUP-ID is a number."
  (pcase tag
    (:unrepl/hello (unrepl-loop--client-hello conn-id payload))
    (:prompt (unrepl-loop--client-prompt conn-id payload))
    (:read (unrepl-loop--client-read conn-id payload group-id))
    (:started-eval (unrepl-loop--client-started-eval conn-id payload group-id))
    (:eval (unrepl-loop--client-eval conn-id payload group-id))
    (:out (unrepl-loop--client-out conn-id payload group-id))
    (:exception (unrepl-loop--client-exception-handler conn-id payload group-id))
    (:bye (unrepl-loop--bye-handler :client conn-id payload))
    (_ (when unrepl-debug
         (error (format "[client] Unrecognized message: %S" tag))))))

;; Message Processing
;; ------------------

(defmacro unrepl-loop--unpack-payload (vars &rest body)
  "Take VARS out of `payload' and make them available in BODY scope.
This macro assumes the existence of a `payload' variable in scope, if there
isn't, an error will be raised.

VARS is a list of symbols that should have corresponding keywords in (an
already available) `payload' variable.  For each symbol in vars, a
corresponding variable will be created and it the value of =(gethash <var
as a keyword> payload)= and will be added to the local scope in which BODY
gets executed."
  (declare (indent 1))
  `(let (,@(mapcar
            (lambda (v)
              `(,v (unrepl-ast-map-elt payload ,(intern-soft (format ":%s" v)))))
            vars))
     ,@body))


(declare-function unrepl-socket-connect "unrepl-socket")
(defun unrepl-loop--client-hello (conn-id payload)
  "Handle a `:unrepl/hello' message transmitted through CONN-ID.
It processes the PAYLOAD to init the corresponding REPL and subsequent
evaluation of inputs."
  (unrepl-loop--unpack-payload
      (actions)
    (unrepl-repl-connected conn-id)                   ;; Start REPL
    (unrepl-project-set-in conn-id :actions actions)  ;; Store global actions
    ;; And start aux connections
    (let* ((host-port (unrepl-conn-host-port conn-id))
           (host (car host-port))
           (port (cdr host-port))
           (start-aux-msg (unrepl-command-template (unrepl-ast-map-elt actions :start-aux)))
           (start-sl-msg (unrepl-command-template (unrepl-ast-map-elt actions :unrepl.jvm/start-side-loader))))
      (unrepl-project-conn-pool-set-in
       conn-id
       :aux (unrepl-socket-connect :aux host port
                                   #'unrepl-loop-aux-handler
                                   (concat start-aux-msg "\n"))
       :side-loader (unrepl-socket-connect :side-loader host port
                                           #'unrepl-loop-side-loader-handler
                                           (concat start-sl-msg "\n"))))))


(defun unrepl-loop--client-prompt (conn-id payload)
  "Handle a `:prompt' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:prompt' as a AST NODE."
  (let* ((previous-ns (-> conn-id
                          (unrepl-projects-get)
                          (unrepl-project-namespace)))
         (new-ns (-> payload
                     (unrepl-ast-map-elt 'clojure.core/*ns*)  ;; tagged element
                     (parseclj-ast-children)
                     (car)                                    ;; actual ns symbol
                     (parseclj-ast-value)))
         (changed-namespace-p (not (eq previous-ns new-ns))))
    (unrepl-project-set-in conn-id :namespace new-ns)
    (if-let (pending-eval (unrepl-pending-evals-shift :client conn-id))
        (progn
          (when (or (unrepl-pending-eval-entry-history-idx pending-eval)
                    changed-namespace-p)
            (unrepl-repl-prompt conn-id))
          (when-let (buffer (unrepl-pending-eval-entry-buffer pending-eval))
            (with-current-buffer buffer
              (setq-local unrepl-latest-eval pending-eval))))
      (unrepl-repl-prompt conn-id))))


(defun unrepl-loop--client-read (conn-id _payload group-id)
  "Handle a `:read' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:read' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation."
  (let ((history-assoc (unrepl-repl-input-history-assoc conn-id group-id)))
    ;; `history-assoc' is either nil or a tuple that contains
    ;; `:repl-history-idx' as its first element and a history entry id as its
    ;; second element.  For more information, read its documentation.
    (apply #'unrepl-pending-eval-update :client conn-id
           :status :read
           :group-id group-id
           :actions nil
           history-assoc)))


(defun unrepl-loop--client-started-eval (conn-id payload group-id)
  "Handle a `:started-eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:started-eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation."
  (unrepl-loop--unpack-payload (actions)
    (unrepl-pending-eval-update :client conn-id
                                :status :started-eval
                                :group-id group-id
                                :actions actions)))


(defun unrepl-loop--client-eval (conn-id payload _group-id)
  "Handle a `:eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation.

This function will see if there's an evaluation display callback function,
and it will use it to show the result.  If not, it will try to determine
where did this evaluation come from (REPL buffer, `unrepl-eval-last-sexp'
command, etc), and will call a different function to display the result
accordingly."
  (unrepl-pending-eval-update :client conn-id
                              :status :eval
                              :payload payload)
  ;; Display the evaluation payload somewhere...
  (if-let (eval-callback (unrepl-pending-eval-callback :client conn-id))
      (funcall eval-callback payload)
    (message "%s" (parseclj-unparse-clojure-to-string payload))))


(defun unrepl-loop--client-out (conn-id payload group-id)
  "Handle a `:out' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:out' as a string or as a #unrepl/string
tagged literal.
GROUP-ID is an integer as described by UNREPL's documentation."
  (unrepl-repl-handle-out conn-id payload group-id))


(defun unrepl-loop--client-exception-handler (conn-id payload group-id)
  "Handle `:exception' messages transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:exception' as an AST node.
GROUP-ID is an integer as described by UNREPL'S documentation."
  (unrepl-pending-eval-update :client conn-id
                              :status :exception)
  (unrepl-repl-handle-exception conn-id payload group-id))



;; Aux Connection Process
;; =============================================================================

(defun unrepl-aux-send (str &optional eval-callback stdout-callback)
  "Send input STR to UNREPL aux connection.
EVAL-CALLBACK is a function that takes the evaluation payload and displays
it in any given way.
STDOUT-CALLBACK is a function that takes any possible output payload and
handles it in any given way.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (prog1 (unrepl-loop--send unrepl-conn-id :aux str)
    (unrepl-pending-eval-add :aux unrepl-conn-id
                             :status :sent
                             :eval-callback eval-callback
                             :stdout-callback stdout-callback)))


(defun unrepl-loop-aux-handler (conn-id tag payload &optional group-id)
  "Dispatch MSG to an `unrepl-loop--aux-*' message handler.
CONN-ID is provided to the handlers so they know which project/repl they
will be affecting.
TAG is the UNREPL tag, and it's used to select the handler function for the
message.
PAYLOAD is a parseclj AST node of the message's payload.
GROUP-ID is an integer as described by UNREPL's documentation."
  (pcase tag
    (:prompt (unrepl-loop--aux-prompt conn-id))
    (:read (unrepl-loop--aux-read conn-id group-id))
    (:started-eval (unrepl-loop--aux-started-eval conn-id payload group-id))
    (:eval (unrepl-loop--aux-eval conn-id payload))
    (:out (unrepl-loop--aux-out conn-id payload group-id))
    (:exception #'ignore)
    (:bye (unrepl-loop--bye-handler :aux conn-id payload))))


(defun unrepl-loop--aux-prompt (conn-id)
  "Handle a `:prompt' message transmitted through CONN-ID.
Shifts the pending evaluations queue for the `:aux' connection."
  (unrepl-pending-evals-shift :aux conn-id))


(defun unrepl-loop--aux-read (conn-id group-id)
  "Handle a `:read' message transmitted through CONN-ID.
GROUP-ID is an integer as described by UNREPL's documentation.
Updates the top of the pending evaluations queue with the `:read' status,
its group id, and its actions."
  (unrepl-pending-eval-update :aux conn-id
                              :status :read
                              :group-id group-id
                              :actions nil))

(defun unrepl-loop--aux-started-eval (conn-id payload group-id)
  "Handle a `:started-eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:started-eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation."
  (unrepl-loop--unpack-payload (actions)
    (unrepl-pending-eval-update :aux conn-id
                                :status :started-eval
                                :group-id group-id
                                :actions actions)))


(defun unrepl-loop--aux-eval (conn-id payload)
  "Handle a `:eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as an AST node.

This function will see if there's an evaluation callback function, and it
will use it to handle the PAYLOAD.  If not, it will just ignore it."
  (when-let (eval-callback (unrepl-pending-eval-callback :aux conn-id))
    (funcall eval-callback payload)))


(defun unrepl-loop--aux-out (conn-id payload group-id)
  "Handle a `:out' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:out' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation.

This function will only work if GROUP-ID matches the current pending
evaluation's, or in other words, if the output is not async.  It will look
for a STDOUT callback function in the pending evaluation's data and call it
with PAYLOAD and GROUP-ID, if any."
  (when (eql (unrepl-pending-eval-group-id :aux conn-id) group-id)
    (when-let (out-callback (unrepl-pending-eval-stdout-callback :aux conn-id))
      (funcall out-callback payload group-id))))



;; Side Loader Process
;; =============================================================================

(defun unrepl-side-loader-send (str)
  "Send input STR to UNREPL side loader connection.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (unrepl-loop--send unrepl-conn-id :side-loader str))


(defun unrepl-loop-side-loader-handler (conn-id tag payload &rest _extra)
  "Dispatch message to an `unrepl-loop--side-loader-*' message handler.
CONN-ID is provided to side-loader message handlers so they know which
project/repl to modify.
TAG is the UNREPL tag for side-loading, expected to be either `:class' or
`:resource'.
PAYLOAD is a parseclj AST node of the message's payload, which should be a
string."
  (pcase tag
    (:unrepl.jvm.side-loader/hello #'ignore)
    (:resource (unrepl-loop--side-loader-resource-handler conn-id payload))
    (:class (unrepl-loop--side-loader-class-handler conn-id payload))
    (:bye (unrepl-loop--bye-handler :side-loader conn-id payload))
    (_ (when unrepl-debug
         (error (format "[side-loader] Unrecognized message %S" tag))))))


(defun unrepl-loop--side-loader-find-file (file-path classpath)
  "Try to find FILE-PATH in CLASSPATH.
CLASSPATH should be a list of paths.  If nothing is found, return nil.
Return the file contents encoded as a base64 string."
  (when-let (path (car classpath))
    (let ((encoded-buffer (lambda ()
                            (encode-coding-region (point-min) (point-max) 'utf-8)
                            (base64-encode-region (point-min) (point-max) t)
                            (format "%S" (buffer-string)))))
      (cond
       ;; path as a directory
       ((file-directory-p path)
        (let ((file-path-complete (concat (file-name-as-directory path)
                                          file-path)))
          (if (file-exists-p file-path-complete)
              (with-temp-buffer
                (insert-file-contents file-path-complete)
                (funcall encoded-buffer))
            (unrepl-loop--side-loader-find-file file-path (cdr classpath)))))
       ;; path as a file (assumed to be jar/zip)
       (t
        (with-temp-buffer
          (condition-case err
              (progn
                (let ((message-log-max nil)
                      (inhibit-message t))
                  (archive-zip-extract path file-path))
                (if (> (buffer-size) 0)
                    (funcall encoded-buffer)
                  (unrepl-loop--side-loader-find-file file-path (cdr classpath))))
            (error
             (ding (message "%S" err))
             (unrepl-loop--side-loader-find-file file-path (cdr classpath))))))))))


(defun unrepl-loop--side-loader-load (conn-id file-path)
  "Find a FILE-PATH in classpath and load it through the side-loader conn.
Classpath is taken from CONN-ID'S project.
The actual file is then sent back to the side-loader as a base64 string.
If FILE-PATH cannot be found, send nil to side-loader."
  (let ((classpath (-> conn-id
                       (unrepl-projects-get)
                       (unrepl-project-classpath))))
    (let ((base64-contents (unrepl-loop--side-loader-find-file file-path classpath)))
      (unrepl-side-loader-send (or base64-contents "nil")))))


(defun unrepl-loop--side-loader-resource-handler (conn-id payload)
  "Handle a `:resource' message from CONN-ID's side-loader.
PAYLOAD is a string pointing to the file-path, as an AST node."
  (unrepl-loop--side-loader-load conn-id (parseclj-ast-value payload)))


(defun unrepl-loop--side-loader-class-handler (conn-id payload)
  "Handle a `:class' message from CONN-ID's side-loader.
PAYLOAD is a string pointing to the file-path, as an AST node."
  (let* ((payload-val (parseclj-ast-value payload))
         (file-path (format "%s.class"
                            (replace-regexp-in-string "\\." "/" payload-val))))
    (unrepl-loop--side-loader-load conn-id file-path)))



;; Generic handlers
;; -------------------------------------------------------------------

(defun unrepl-loop--bye-handler (type conn-id payload)
  "Handle UNREPL `:bye' message for TYPE connection to CONN-ID.
PAYLOAD is the `:bye' message payload as an AST node.
Announce disconnection to the user and quit the project."
  (let ((msg (format "UNREPL %s connection disconnected, shutting down." type))
        (payload (unrepl-ast-unparse-to-string payload)))
    (unrepl-project-quit conn-id)
    (error (concat msg "%s") payload)))


(provide 'unrepl-loop)

;;; unrepl-loop.el ends here
