;;; spiral-loop.el --- UNREPL EDN messages processing -*- lexical-binding: t; -*-
;;
;; Filename: spiral-loop.el
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

(require 'spiral-ast)
(require 'spiral-mode)
(require 'spiral-project)
(require 'spiral-repl)


(defcustom spiral-sync-request-timeout 10
  "The number of seconds to wait for a sync response.
Setting this to nil disables the timeout functionality."
  :type 'integer
  :group 'spiral)

(defvar-local spiral-loop-process-type nil
  "Type of process.
This local variable is meant to be set in conn-pool processes' buffers so
that they are easily distinguishable.")

(defvar-local spiral-loop-process-dispatcher nil
  "The EDN message dispatcher function for a process buffer.")

(defvar-local spiral-loop-process-output-start 1
  "The start of the most recent output UNREPL message.")

(defvar-local spiral-loop-greeted-p nil
  "Predicate that defines if the client for the current buffer has been greeted already.")


(defun spiral-loop--announce-greeting-p (process)
  "Decide whether or not to announce this PROCESS greetings."
  (with-current-buffer (process-buffer process)
    (eql spiral-loop-process-type :client)))


(defun spiral-loop--send (conn-id proc-type str &optional no-line-break)
  "Send input STR to PROC-TYPE of CONN-ID.
PROC-TYPE is a keyword, either `:client', `:aux', or `:side-loader'.
By default, this function will add a new line after STR.  NO-LINE-BREAK
overrides this behavior."
  (let* ((project (spiral-projects-get conn-id))
         (proc (spiral-project-conn-pool-get-process project proc-type)))
    (process-send-string proc (concat str (unless no-line-break "\n")))
    str))


(defun spiral-loop--sync-request (conn-id proc-type str)
  "Send input STR to PROC-TYPE of CONN-ID and synchronously wait for a response.
PROC-TYPE is a keyword, either `:client', `:aux', or `:side-loader'."
  (let* ((start (current-time))
         result)
    (spiral-loop--send conn-id proc-type str)
    (spiral-pending-eval-add proc-type conn-id
                             :status :sent
                             :eval-callback (lambda (eval-payload)
                                              (setq result eval-payload)))
    (while (and (not result)
                (spiral-pending-eval proc-type conn-id)
                (not (input-pending-p))  ;; do not hang UI
                (or (not spiral-sync-request-timeout)
                    (< (cadr (time-subtract (current-time) start))
                       spiral-sync-request-timeout)))
      (accept-process-output nil 0.01))
    result))


(defun spiral-loop--destructure-message-ast (msg-node)
  "Traverse MSG-NODE and return its UNREPL tag, payload and group-id.
Tag is returned as a keyword.
Payload is returned as a parseclj AST node.
Group-id is returned as an integer."
  (let* ((zp (spiral-ast-zip msg-node))
         (tag (thread-first zp
                (treepy-down)
                (treepy-node)
                (parseclj-ast-value)))
         (payload (thread-first zp
                    (treepy-down)
                    (treepy-right)
                    (treepy-node)))
         (group-id (thread-first zp
                     (treepy-down)
                     (treepy-right)
                     (treepy-right)
                     (treepy-node)
                     (parseclj-ast-value))))
    (list tag payload group-id)))


(defun spiral-loop-handle-proc-message (process output)
  "Decode OUTPUT's EDN messages from PROCESS, and dispatch accordingly."
  (let ((proc-buf (process-buffer process)))
    (with-current-buffer proc-buf
      (unless spiral-loop-greeted-p
        (when-let (hello-match (string-match-p "\\[:unrepl.*/hello" output))
          (setq output (substring output hello-match))
          (setq-local spiral-loop-greeted-p t)
          (when (spiral-loop--announce-greeting-p process)
            (message "UNREPL says hi!"))))
      (when spiral-loop-greeted-p
        (goto-char (point-max))
        (insert output)

        ;; There can either be several EDN messages in OUTPUT, or an incomplete
        ;; message.
        ;; If output ends in ]\n we assume it is complete, and we iterate over
        ;; all possible forms in it.
        (when (string-suffix-p "]\n" (buffer-substring-no-properties spiral-loop-process-output-start
                                                                     (point-max)))
          (goto-char spiral-loop-process-output-start)
          (mapc (lambda (msg-ast-node)
                  (apply spiral-loop-process-dispatcher
                         spiral-conn-id
                         (spiral-loop--destructure-message-ast msg-ast-node)))
                (parseclj-ast-children (parseclj-parse-clojure)))
          (setq-local spiral-loop-process-output-start (point-max)))))))



;; Client Process
;; =============================================================================

(defun spiral-client-send (str &optional eval-callback stdout-callback buffer)
  "Send input STR to UNREPL using the client connection.
EVAL-CALLBACK is a function that takes the evaluation payload and displays
it in any given way.
STDOUT-CALLBACK is a function that takes any output payload taken from this
evaluation and process it in any given way.
If the given STR input is interactive, BUFFER gets saved into the new
pending evaluation so that it can be moved to said buffer as 'latest
evaluation' before being discarded from the pending evaluations queue.
Connection to sent the input to is inferred from `spiral-conn-id'."
  (prog1 (spiral-loop--send spiral-conn-id :client str)
    (spiral-pending-eval-add :client spiral-conn-id
                             :status :sent
                             :input str
                             :buffer buffer
                             :eval-callback eval-callback
                             :stdout-callback stdout-callback)))


(defun spiral-client-sync-request (str)
  "Send input STR to UNREPL client connection and waits for a response."
  (spiral-loop--sync-request spiral-conn-id :client str))


(defun spiral-loop-client-dispatcher (conn-id tag payload &optional group-id)
  "Dispatch an UNREPL message to an `spiral-loop--client-*' message handler.
CONN-ID is provided to client message handlers so they know which
project/repl to modify.
TAG is the UNREPL tag, and it's used to select the handler function for the
message.
PAYLOAD is a parseclj AST node of the message's payload.
GROUP-ID is a number."
  (pcase tag
    (:unrepl/hello (spiral-loop--client-hello conn-id payload))
    (:prompt (spiral-loop--client-prompt conn-id payload))
    (:read (spiral-loop--client-read conn-id payload group-id))
    (:started-eval (spiral-loop--client-started-eval conn-id payload group-id))
    (:eval (spiral-loop--client-eval conn-id payload group-id))
    (:out (spiral-loop--client-std-stream conn-id payload group-id :out))
    (:err (spiral-loop--client-std-stream conn-id payload group-id :err))
    (:exception (spiral-loop--client-exception-handler conn-id payload group-id))
    (:bye (spiral-loop--bye-handler :client conn-id payload))
    (_ (when spiral-debug
         (error (format "[client] Unrecognized message: %S" tag))))))

;; Message Processing
;; ------------------

(defmacro spiral-loop--unpack-payload (vars &rest body)
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
              `(,v (spiral-ast-map-elt payload ,(intern-soft (format ":%s" v)))))
            vars))
     ,@body))


(declare-function spiral-socket-connect "spiral-socket")
(defun spiral-loop--client-hello (conn-id payload)
  "Handle a `:unrepl/hello' message transmitted through CONN-ID.
It processes the PAYLOAD to init the corresponding REPL and subsequent
evaluation of inputs."
  (spiral-loop--unpack-payload (actions print-settings)
    (spiral-repl-connected conn-id)                   ;; Start REPL
    (spiral-project-set-in conn-id :actions actions)  ;; Store global actions
    (spiral-project-set-in conn-id :print-settings    ;; Store print settings
                           (spiral-ast-to-elisp print-settings))
    ;; And start aux connections
    (let* ((host-port (spiral-conn-host-port conn-id))
           (host (car host-port))
           (port (cdr host-port))
           (start-aux-msg (spiral-command-template (spiral-ast-map-elt actions :start-aux)))
           (start-sl-msg (spiral-command-template (spiral-ast-map-elt actions :unrepl.jvm/start-side-loader))))
      (spiral-project-conn-pool-set-in
       conn-id
       :aux (spiral-socket-connect :aux host port
                                   #'spiral-loop-aux-handler
                                   (concat start-aux-msg "\n"))
       :side-loader (spiral-socket-connect :side-loader host port
                                           #'spiral-loop-side-loader-handler
                                           (concat start-sl-msg "\n"))))))


(defun spiral-loop--client-prompt (conn-id payload)
  "Handle a `:prompt' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:prompt' as a AST NODE."
  (let* ((previous-ns (thread-first conn-id
                        (spiral-projects-get)
                        (spiral-project-namespace)))
         (new-ns (thread-first payload
                   (spiral-ast-map-elt 'clojure.core/*ns*)  ;; tagged element
                   (parseclj-ast-children)
                   (car)                                    ;; actual ns symbol
                   (parseclj-ast-value)))
         (changed-namespace-p (not (eq previous-ns new-ns))))
    (spiral-project-set-in conn-id :namespace new-ns)
    (if-let (pending-eval (spiral-pending-evals-shift :client conn-id))
        (progn
          (when (or (spiral-pending-eval-entry-history-idx pending-eval)
                    changed-namespace-p)
            (spiral-repl-prompt conn-id))
          (when-let (buffer (spiral-pending-eval-entry-buffer pending-eval))
            (with-current-buffer buffer
              (setq-local spiral-latest-eval pending-eval))))
      (spiral-repl-prompt conn-id))))


(defun spiral-loop--client-read (conn-id _payload group-id)
  "Handle a `:read' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:read' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation."
  (let ((history-assoc (spiral-repl-input-history-assoc conn-id group-id)))
    ;; `history-assoc' is either nil or a tuple that contains
    ;; `:repl-history-idx' as its first element and a history entry id as its
    ;; second element.  For more information, read its documentation.
    (apply #'spiral-pending-eval-update :client conn-id
           :status :read
           :group-id group-id
           :actions nil
           history-assoc)))


(defun spiral-loop--client-started-eval (conn-id payload group-id)
  "Handle a `:started-eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:started-eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation."
  (spiral-loop--unpack-payload (actions)
    (spiral-pending-eval-update :client conn-id
                                :status :started-eval
                                :group-id group-id
                                :actions actions)))


(defun spiral-loop--client-eval (conn-id payload _group-id)
  "Handle a `:eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation.

This function will see if there's an evaluation display callback function,
and it will use it to show the result.  If not, it will try to determine
where did this evaluation come from (REPL buffer, `spiral-eval-last-sexp'
command, etc), and will call a different function to display the result
accordingly."
  (spiral-pending-eval-update :client conn-id
                              :status :eval
                              :payload payload)
  ;; Display the evaluation payload somewhere...
  (if-let (eval-callback (spiral-pending-eval-callback :client conn-id))
      (funcall eval-callback payload)
    (message "%s" (parseclj-unparse-clojure-to-string payload))))


(defun spiral-loop--client-std-stream (conn-id payload group-id type)
  "Handle either std `:out' or `:err' message transmitted through CONN-ID.
TYPE should be either `:out' or `:err'.
PAYLOAD is the UNREPL payload for `:out' as a string or as a #unrepl/string
tagged literal.
GROUP-ID is an integer as described by UNREPL's documentation."
  (spiral-pending-eval-update :client conn-id
                              :status type
                              :payload payload)
  (spiral-repl-handle-std-stream type conn-id payload group-id))


(defun spiral-loop--client-exception-handler (conn-id payload group-id)
  "Handle `:exception' messages transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:exception' as an AST node.
GROUP-ID is an integer as described by UNREPL'S documentation."
  (spiral-pending-eval-update :client conn-id
                              :status :exception)
  (spiral-repl-handle-exception conn-id payload group-id))



;; Aux Connection Process
;; =============================================================================

(defun spiral-aux-send (str &optional eval-callback stdout-callback)
  "Send input STR to UNREPL aux connection.
EVAL-CALLBACK is a function that takes the evaluation payload and displays
it in any given way.
STDOUT-CALLBACK is a function that takes any possible output payload and
handles it in any given way.
Connection to sent the input to is inferred from `spiral-conn-id'."
  (prog1 (spiral-loop--send spiral-conn-id :aux str)
    (spiral-pending-eval-add :aux spiral-conn-id
                             :status :sent
                             :eval-callback eval-callback
                             :stdout-callback stdout-callback)))


(defun spiral-aux-sync-request (str)
  "Send input STR to UNREPL aux connection and waits for a response."
  (spiral-loop--sync-request spiral-conn-id :aux str))


(defun spiral-loop-aux-handler (conn-id tag payload &optional group-id)
  "Dispatch MSG to an `spiral-loop--aux-*' message handler.
CONN-ID is provided to the handlers so they know which project/repl they
will be affecting.
TAG is the UNREPL tag, and it's used to select the handler function for the
message.
PAYLOAD is a parseclj AST node of the message's payload.
GROUP-ID is an integer as described by UNREPL's documentation."
  (pcase tag
    (:unrepl/hello (spiral-loop--aux-hello conn-id))
    (:prompt (spiral-loop--aux-prompt conn-id))
    (:read (spiral-loop--aux-read conn-id group-id))
    (:started-eval (spiral-loop--aux-started-eval conn-id payload group-id))
    (:eval (spiral-loop--aux-eval conn-id payload))
    (:out (spiral-loop--aux-out conn-id payload group-id))
    (:err #'ignore)  ;; for now
    (:exception (spiral-loop--aux-exception conn-id payload group-id))
    (:bye (spiral-loop--bye-handler :aux conn-id payload))))


(defun spiral-loop--aux-hello (conn-id)
  "Handle `:unrepl/hello' messages transmitted through aux CONN-ID.
When a aux connection initializes, print settings should be set according
to spiral customs."
  (let ((project (spiral-projects-get conn-id)))
    (spiral-update-print-settings project :eval
                                  spiral-repl-print-length
                                  spiral-repl-print-level
                                  'same
                                  'async)
    (spiral-update-print-settings project :out
                                  'same
                                  'same
                                  spiral-repl-stdout-string-length
                                  'async)))


(defun spiral-loop--aux-prompt (conn-id)
  "Handle a `:prompt' message transmitted through CONN-ID.
Shifts the pending evaluations queue for the `:aux' connection."
  (spiral-pending-evals-shift :aux conn-id))


(defun spiral-loop--aux-read (conn-id group-id)
  "Handle a `:read' message transmitted through CONN-ID.
GROUP-ID is an integer as described by UNREPL's documentation.
Updates the top of the pending evaluations queue with the `:read' status,
its group id, and its actions."
  (spiral-pending-eval-update :aux conn-id
                              :status :read
                              :group-id group-id
                              :actions nil))

(defun spiral-loop--aux-started-eval (conn-id payload group-id)
  "Handle a `:started-eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:started-eval' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation."
  (spiral-loop--unpack-payload (actions)
    (spiral-pending-eval-update :aux conn-id
                                :status :started-eval
                                :group-id group-id
                                :actions actions)))


(defun spiral-loop--aux-eval (conn-id payload)
  "Handle a `:eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as an AST node.

This function will see if there's an evaluation callback function, and it
will use it to handle the PAYLOAD.  If not, it will just ignore it."
  (when-let (eval-callback (spiral-pending-eval-callback :aux conn-id))
    (funcall eval-callback payload)))


(defun spiral-loop--aux-out (conn-id payload group-id)
  "Handle a `:out' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:out' as an AST node.
GROUP-ID is an integer as described by UNREPL's documentation.

This function will only work if GROUP-ID matches the current pending
evaluation's, or in other words, if the output is not async.  It will look
for a STDOUT callback function in the pending evaluation's data and call it
with PAYLOAD and GROUP-ID, if any."
  (when (eql (spiral-pending-eval-group-id :aux conn-id) group-id)
    (spiral-pending-eval-update :aux conn-id
                                :status :out)
    (when-let (out-callback (spiral-pending-eval-stdout-callback :aux conn-id))
      (funcall out-callback payload group-id))))


(declare-function spiral-stacktrace-popup "spiral-stacktrace")
(defun spiral-loop--aux-exception (conn-id payload _group-id)
  "Handle `:exception' messages transmitted through CONN-ID in `:aux'.
PAYLOAD is the UNREPL payload for `:exception' as an AST node.
GROUP-ID is an integer as described by UNREPL'S documentation."
  (spiral-pending-eval-update :aux conn-id
                              :status :exception)
  (spiral-stacktrace-popup conn-id payload))



;; Side Loader Process
;; =============================================================================

(defun spiral-side-loader-send (str)
  "Send input STR to UNREPL side loader connection.
Connection to sent the input to is inferred from `spiral-conn-id'."
  (spiral-loop--send spiral-conn-id :side-loader str))


(defun spiral-loop-side-loader-handler (conn-id tag payload &rest _extra)
  "Dispatch message to an `spiral-loop--side-loader-*' message handler.
CONN-ID is provided to side-loader message handlers so they know which
project/repl to modify.
TAG is the UNREPL tag for side-loading, expected to be either `:class' or
`:resource'.
PAYLOAD is a parseclj AST node of the message's payload, which should be a
string."
  (pcase tag
    (:unrepl.jvm.side-loader/hello #'ignore)
    (:resource (spiral-loop--side-loader-resource-handler conn-id payload))
    (:class (spiral-loop--side-loader-class-handler conn-id payload))
    (:bye (spiral-loop--bye-handler :side-loader conn-id payload))
    (_ (when spiral-debug
         (error (format "[side-loader] Unrecognized message %S" tag))))))


(defun spiral-loop--side-loader-find-file (file-path classpath)
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
            (spiral-loop--side-loader-find-file file-path (cdr classpath)))))
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
                  (spiral-loop--side-loader-find-file file-path (cdr classpath))))
            (error
             (ding (message "%S" err))
             (spiral-loop--side-loader-find-file file-path (cdr classpath))))))))))


(defun spiral-loop--side-loader-load (conn-id file-path)
  "Find a FILE-PATH in classpath and load it through the side-loader conn.
Classpath is taken from CONN-ID'S project.
The actual file is then sent back to the side-loader as a base64 string.
If FILE-PATH cannot be found, send nil to side-loader."
  (let ((classpath (thread-first conn-id
                     (spiral-projects-get)
                     (spiral-project-classpath))))
    (let ((base64-contents (spiral-loop--side-loader-find-file file-path classpath)))
      (spiral-side-loader-send (or base64-contents "nil")))))


(defun spiral-loop--side-loader-resource-handler (conn-id payload)
  "Handle a `:resource' message from CONN-ID's side-loader.
PAYLOAD is a string pointing to the file-path, as an AST node."
  (spiral-loop--side-loader-load conn-id (parseclj-ast-value payload)))


(defun spiral-loop--side-loader-class-handler (conn-id payload)
  "Handle a `:class' message from CONN-ID's side-loader.
PAYLOAD is a string pointing to the file-path, as an AST node."
  (let* ((payload-val (parseclj-ast-value payload))
         (file-path (format "%s.class"
                            (replace-regexp-in-string "\\." "/" payload-val))))
    (spiral-loop--side-loader-load conn-id file-path)))



;; Generic handlers
;; -------------------------------------------------------------------

(defun spiral-loop--bye-handler (type conn-id payload)
  "Handle UNREPL `:bye' message for TYPE connection to CONN-ID.
PAYLOAD is the `:bye' message payload as an AST node.
Announce disconnection to the user and quit the project."
  (let ((msg (format "UNREPL %s connection disconnected, shutting down." type))
        (payload (spiral-ast-unparse-to-string payload)))
    (spiral-disconnect conn-id)
    (error (concat msg "%s") payload)))


(provide 'spiral-loop)

;;; spiral-loop.el ends here
