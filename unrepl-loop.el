;;; unrepl-loop.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-loop.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Nov 11 20:07:16 2017 (+0100)
;; Version:
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(require 'parseedn)

(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-repl)


(defvar unrepl-loop--global-edn-tag-readers
  `((unrepl/ns . ,#'symbol-name)
    (unrepl/string . ,(lambda (c) (format "%S" c)))
    (unrepl.java/class . ,(lambda (c) (format "%S" c)))
    (unrepl/object . ,(lambda (c) (format "%S" c)))
    (error . ,(lambda (c) (format "%S" c)))
    (unrepl/... . ,(lambda (_) "...")))
  "Global EDN tag readers to be used on every incoming client message.")

(defvar-local unrepl-loop-process-type nil
  "Type of process.
This local variable is meant to be set in conn-pool processes' buffers so
that they are easily distinguishable.")

(defvar-local unrepl-loop-process-dispatcher nil
  "The EDN message dispatcher function for a process buffer.")

(defvar-local unrepl-loop-greeted-p nil
  "Predicate that defines if the client for the current buffer has been greeted already.")


(defun unrepl-loop--announce-greeting-p (process)
  "Decide whether or not to announce this PROCESS greetings."
  (with-current-buffer (process-buffer process)
    (eql unrepl-loop-process-type 'client)))


(defun unrepl-loop--send (conn-id proc-type str)
  "Send input STR to PROC-TYPE of CONN-ID.
PROC-TYPE is a keyword, either `:client', `:aux', or `:side-loader'."
  (let* ((project (unrepl-projects-get conn-id))
         (proc (unrepl-project-conn-pool-get-process project proc-type)))
    (process-send-string proc (concat str "\n"))
    str))


(declare-function unrepl-process-conn-id "unrepl")
(defun unrepl-loop-handle-proc-message (process output)
  "Decode OUTPUT's EDN messages from PROCESS, and dispatch accordingly."
  (let ((proc-buf (process-buffer process)))
    (with-current-buffer proc-buf
      (unless unrepl-loop-greeted-p
        (when-let (hello-match (string-match-p (regexp-quote "[:unrepl/hello")  ;; TODO: check for sideloader
                                               output))
          (setq output (substring output hello-match))
          (setq-local unrepl-loop-greeted-p t)
          (when (unrepl-loop--announce-greeting-p process)
            (message "UNREPL says hi!"))))
      (when unrepl-loop-greeted-p
        (goto-char (point-max))
        (save-excursion (insert output))

        ;; There can be several EDN messages in OUTPUT, so we iterate over them.
        (mapcar (lambda (edn-msg)
                  (funcall unrepl-loop-process-dispatcher
                           edn-msg
                           (unrepl-process-conn-id process)))
                (parseedn-read unrepl-loop--global-edn-tag-readers))))))



;; Client Process
;; =============================================================================

(defun unrepl-client-send (str)
  "Send input STR to UNREPL client connection.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (unrepl-loop--send unrepl-conn-id 'client str)
  str)


(defun unrepl-loop-client-dispatcher (msg conn-id)
  "Dispatch MSG to an `unrepl-loop--' client message handler.
CONN-ID is provided to client message handlers so they know which
project/repl to modify."
  (seq-let [tag payload group-id] msg
    (pcase tag
      (:unrepl/hello (unrepl-loop--hello conn-id payload))
      (:prompt (unrepl-loop--prompt conn-id payload))
      (:read (unrepl-loop--read conn-id payload group-id))
      (:started-eval (unrepl-loop--started-eval conn-id payload group-id))
      (:eval (unrepl-loop--eval conn-id payload group-id))
      (:out (unrepl-loop--out conn-id payload group-id))
      (:exception (unrepl-loop--placeholder-handler conn-id payload group-id))
      (_ (error (format "Unrecognized message: %S" tag))))))

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
              `(,v (map-elt payload ,(intern-soft (format ":%s" v)))))
            vars))
     ,@body))

(defun unrepl-loop--hello (conn-id payload)
  "Handle a `:unrepl/hello' message transmitted through CONN-ID.
It processes the PAYLOAD to init the corresponding REPL and subsequent
evaluation of inputs."
  (unrepl-loop--unpack-payload
      (actions)
    (unrepl-repl-connected conn-id)
    (unrepl-project-set-in conn-id :actions actions)))


(defun unrepl-loop--prompt (conn-id payload)
  "Handle a `:prompt' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:prompt' as a hash table."
  (unrepl-project-set-in conn-id :namespace (map-elt payload 'clojure.core/*ns*))
  (unrepl-repl-prompt conn-id))


(defun unrepl-loop--read (conn-id _payload group-id)
  "Handle a `:read' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:read' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation."
  (let ((history-assoc (unrepl-repl-input-history-assoc conn-id group-id)))
    ;; `history-assoc' is either nil or a tuple that contains
    ;; `:history-entry-id' as its first element and a history entry id as its
    ;; second element.  For more information, read its documentation.
    (apply #'unrepl-project-pending-eval-update conn-id group-id :read history-assoc)))


(defun unrepl-loop--started-eval (conn-id payload group-id)
  "Handle a `:started-eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:started-eval' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation."
  (unrepl-loop--unpack-payload
      (actions)
    (unrepl-project-pending-eval-update conn-id group-id :started-eval
                                        :actions actions)))


(defun unrepl-loop--eval (conn-id payload group-id)
  "Handle a `:eval' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation.

This function will determine where did this evaluation come from (REPL
buffer, `unrepl-eval-last-sexp' command, etc), and will call a different
function to display the result accordingly."
  (if-let (history-id (unrepl-project-pending-evals-get-history-id conn-id group-id))
      (unrepl-repl-insert-evaluation conn-id history-id payload)
    (message "%S" payload))  ;; placeholder, maybe add function that knows how to display payload
  (unrepl-project-pending-eval-update conn-id group-id :eval))


(defun unrepl-loop--out (conn-id payload group-id)
  "Handle a `:out' message transmitted through CONN-ID.
PAYLOAD is the UNREPL payload for `:eval' as a hash table.
GROUP-ID is an integer as described by UNREPL's documentation."
  (unrepl-repl-insert-out conn-id group-id payload))


(defun unrepl-loop--placeholder-handler (conn-id payload group-id)
  ""
  (unrepl-repl-insert-out conn-id group-id (format "%S" payload)))



;; Aux Connection Process
;; =============================================================================

(defun unrepl-aux-send (str)
  "Send input STR to UNREPL aux connection.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (unrepl-loop--send unrepl-conn-id 'aux str)
  str)



;; Side Loader Process
;; =============================================================================

(defun unrepl-side-loader-send (str)
  "Send input STR to UNREPL side loader connection.
Connection to sent the input to is inferred from `unrepl-conn-id'."
  (unrepl-loop--send unrepl-conn-id 'side-loader str)
  str)


(defun unrepl-loop-handle-side-loader-message (process output)
  "Decode EDN messages from PROCESS contained in OUTPUT and act accordingly."
  (with-current-buffer (process-buffer process)
    (unless (string-match-p (regexp-quote "user=>") output)
      (goto-char (point-max))
      (save-excursion (insert output)))))


(provide 'unrepl-loop)

;;; unrepl-loop.el ends here
