;;; unrepl-mode.el --- UNREPL minor mode for interactions with Socket Server -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-mode.el
;; Description:
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Nov 12 12:25:44 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; UNREPL minor mode for interactions with Socket Server
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

(require 'unrepl-ast)
(require 'unrepl-project)
(require 'unrepl-overlay)
(require 'unrepl-util)

(defcustom unrepl-ask-for-connection t
  "Automatically ask for host:port when trying to interact with UNREPL in an unconnected buffer."
  :type 'boolean
  :group 'unrepl)

(defcustom unrepl-display-repl-in-current-window nil
  "Whether to display the REPL in the current window."
  :type 'boolean
  :group 'unrepl-repl)

(defcustom unrepl-mode-line-show-connection t
  "Whether to have mode-line show the connection id or not."
  :type 'boolean
  :group 'unrepl)

(defcustom unrepl-mode-line
  '(:eval (format "un[%s]" (or unrepl-conn-id
                               "-")))
  "Mode line lighter for `unrepl-mode'."
  :type 'sexp
  :risky t
  :group 'unrepl)

(defcustom unrepl-eval-result-display 'both
  "Whether to display evaluation results with overlays, in the echo area, or both."
  :type '(choice (const :tag "End of line" overlay)
                 (const :tag "Echo area" echo)
                 (const :tag "Both" both))
  :group 'unrepl)

(defvar-local unrepl-conn-id nil
  "Port number used when creating a new Socket REPL.")

(defvar-local unrepl-latest-eval nil
  "Latest evaluation for the current buffer.")



;; Helpers
;; -------------------------------------------------------------------

(defun unrepl-mode--conn-id-prompt ()
  "Prompt the user for a HOST:PORT conn-id.
Return a conn-id symbol."
  (intern (format "%s:%s"
                  (read-string "Host: ")
                  (read-string "Port: "))))


(defun unrepl-ensure-connected! ()
  "Make sure an `unrepl-conn-id' exists for current buffer.
If this local variable is not already set, tries to find a good candidate
by looking at the buffer's file path and comparing to existing
`unrepl-projects'.
If that fails and `unrepl-ask-for-connection' is t-ish, asks the user for
an *existing* host:port connection to connect to.
If everything else fails, raise an error.

Return a UNREPL project"
  (if-let (project (and unrepl-conn-id
                        (unrepl-projects-get unrepl-conn-id)))
      project
    (if-let (conn-id (unrepl-mode--find-connection-by-file-name))
        (progn
          (setq-local unrepl-conn-id conn-id)
          (unrepl-projects-get conn-id))
      (if unrepl-ask-for-connection
          (let ((conn-id (unrepl-mode--conn-id-prompt)))
            (if-let (project (unrepl-projects-get conn-id))
                (progn
                  (setq-local unrepl-conn-id conn-id)
                  project)
              (error "Could not find an UNREPL connection for %s" conn-id)))
        (error "Could not find an UNREPL connection for this buffer")))))


(defmacro with-current-project (&rest body)
  "Ensure the current buffer is connected and put its project in BODY's local scope."
  `(let ((project (unrepl-ensure-connected!)))
     ,@body))


(defmacro unrepl--binding-print-limits (bindings &rest body)
  "Edit UNREPL `:print-limits' with BINDINGS, exec BODY and revert limits back.
This macro adds a `revert-bindings-back' function into BODY's lexical
context.  BODY is in charge of calling this function whenever it seems
appropriate."
  (declare (indent 1))
  `(with-current-project
    (let ((print-limits-templ (unrepl-project-actions-get project :print-limits))
          (ast-limits->alist (lambda (bindings-node)
                               (mapcar
                                (lambda (key)
                                  (cons key (parseclj-ast-value (unrepl-ast-map-elt bindings-node key))))
                                '(:unrepl.print/string-length
                                  :unrepl.print/coll-length
                                  :unrepl.print/nesting-depth)))))
      (unrepl-aux-send (unrepl-command-template print-limits-templ ,bindings)
                       (lambda (previous-limits)
                         (let ((revert-bindings-back (lambda (&rest _args)
                                                       (unrepl-aux-send (unrepl-command-template
                                                                         print-limits-templ
                                                                         (funcall ast-limits->alist
                                                                                  previous-limits))))))
                           ,@body))))))


(defun unrepl-mode--find-connection-by-file-name ()
  "Check the current buffer file name and try to find a matching UNREPL connection."
  nil)



;; Evaluation
;; -------------------------------------------------------------------

(declare-function unrepl-client-send "unrepl-loop")
(defun unrepl-eval (form eval-callback &optional stdout-callback)
  "Send FORM to UNREPL Socket Server for evaluation.
FORM can either be a string or a list tuple of buffer start, end positions.
This function sends everything through the `:client' connection, and
dispatches the evaluation payload (as an AST node) to EVAL-CALLBACK, which
can expect it as its only argument.  STDOUT-CALLBACK is also a function
that expects just one argument, any STDOUT belonging to this evaluation."
  (when (listp form)
    (remove-overlays (car form) (cadr form) 'temporary t))
  (let ((form (if (consp form)
                  (apply #'buffer-substring-no-properties form)
                form)))
    (unrepl-client-send form eval-callback stdout-callback (current-buffer))))


(defun unrepl-mode--interactive-eval-display-callback (eval-payload &optional bounds)
  "Display evaluation result EVAL-PAYLOAD as a string.
This function will put a string version of EVAL-PAYLOAD in the echo area,
font-locked as Clojure.
If BOUNDS is non-nil and `unrepl-eval-result-display' is something else
than 'echo, VALUE will also be displayed in an overlay starting at the end
bound."
  (let ((value (unrepl-ast-unparse-to-string eval-payload))
        (point (cadr bounds)))
    (when (and point (not (eql unrepl-eval-result-display 'echo)))
      (unrepl--make-result-overlay value point))
    (message "%s%s" unrepl-eval-result-prefix value)))


(defun unrepl-mode--interactive-eval-replace-callback (eval-payload bounds)
  "Replace whatever it is in BOUNDS with the evaluation result EVAL-PAYLOAD.
This function will delete whatever it is between BOUNDS in BUFFER, and
replace it with a string version of EVAL-PAYLOAD."
  (with-current-buffer (marker-buffer (car bounds))
    (apply #'delete-region bounds)
    (goto-char (car bounds))
    (unrepl-ast-unparse eval-payload)))


(defun unrepl-eval-last-sexp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with PREFIX, replace the evaluated for with its result in
current buffer."
  (interactive "P")
  (unrepl-ensure-connected!)
  (let ((bounds (unrepl-last-sexp 'marker-bounds))
        (callback (if prefix
                      #'unrepl-mode--interactive-eval-replace-callback
                    #'unrepl-mode--interactive-eval-display-callback)))
    (unrepl-eval bounds
                 (lambda (eval-payload)
                   (funcall callback eval-payload bounds)))))



;; Interactive Commands
;; -------------------------------------------------------------------

(defun unrepl-switch-to-repl-buffer ()
  "Switch to the REPL buffer for `unrepl-conn-id'."
  (interactive)
  (with-current-project
   (let ((repl-buffer (unrepl-project-repl-buffer project)))
     (if unrepl-display-repl-in-current-window
         (pop-to-buffer-same-window repl-buffer t)
       (pop-to-buffer repl-buffer nil t)))))


(declare-function unrepl-repl-insert-phantom-input "unrepl-repl")
(defun unrepl-inspect-last-eval ()
  "Replicate last evaluation in REPL buffer for further inspection."
  (interactive)
  (unrepl-repl-insert-phantom-input unrepl-latest-eval nil 'switch))


(declare-function unrepl-aux-send "unrepl-loop")
(defun unrepl-eval-interrupt ()
  "Interrupt pending evaluation."
  (interactive)
  (with-current-project
   (if-let (pending-eval (unrepl-pending-eval :client (unrepl-project-id project)))
       (let* ((actions (unrepl-pending-eval-entry-actions pending-eval))
              (interrupt-templ (unrepl-ast-map-elt actions :interrupt)))
         (unrepl-aux-send (unrepl-command-template interrupt-templ)
                          (lambda (_) (message "Evaluation interrupted!"))))
     (message "No operations pending..."))))


(defun unrepl-quit (&optional just-do-it conn-id)
  "Quit connection to CONN-ID or current `unrepl-conn-id'.
If JUST-DO-IT is non-nil, don't ask for confirmation."
  (interactive "P")
  (let ((conn-id (or conn-id
                     unrepl-conn-id
                     (unrepl-mode--conn-id-prompt))))
    (if-let (project (unrepl-projects-get conn-id))
        (when (or just-do-it
                  (y-or-n-p (format "Are you sure you want to quit connection to %s? " conn-id)))
          (unrepl-project-quit conn-id)
          (message "UNREPL connection to %s terminated" conn-id))
      (error "Connection %s could not be found" conn-id))))


(defconst unrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'unrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-x C-e") #'unrepl-eval-last-sexp)
    (define-key map (kbd "C-c C-r") #'unrepl-inspect-last-eval)
    (define-key map (kbd "C-c C-g") #'unrepl-eval-interrupt)
    (define-key map (kbd "C-c q") #'unrepl-quit)
    (define-key map (kbd "C-c C-q") #'unrepl-quit)
    map))


(define-minor-mode unrepl-mode
  "Minor mode for UNREPL.

\\{unrepl-mode-map\}"
  nil
  unrepl-mode-line
  unrepl-mode-map
  (unless unrepl-mode
    (kill-local-variable 'unrepl-conn-id)))


(provide 'unrepl-mode)

;;; unrepl-mode.el ends here
