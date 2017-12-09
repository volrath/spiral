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

(defcustom unrepl-auto-mode t
  "Whether or not to automatically enable `unrepl-mode' for all Clojure buffer."
  :type 'boolean
  :group 'unrepl)

(defvar unrepl-completion-last-context nil
  "Last compliment context used.")

(defvar-local unrepl-conn-id nil
  "Port number used when creating a new Socket REPL.")

(defvar-local unrepl-latest-eval nil
  "Latest evaluation for the current buffer.")



;; Helpers
;; -------------------------------------------------------------------

(defun unrepl-mode--find-project-by-file-name (&optional allow-nil)
  "Check the current buffer file name and try to find a matching UNREPL project.
If ALLOW-NIL is non-nil, allows returning projects with nil directory.
If there is more than one project for this buffers file name, return the
most recently created."
  (let ((project-dir (unrepl-clojure-dir)))
    (when (or project-dir allow-nil)
      (unrepl-projects-get-by-dir project-dir))))


(defun unrepl-mode-enable-auto ()
  "Automatically enable UNREPL's minor mode in every new Clojure buffer.
Setup a `clojure-mode-hook' that checks for a possible project connection
each time a new Clojure buffer gets opened."
  (add-hook 'clojure-mode-hook #'unrepl-mode-conditionally-turn-on))


(defun unrepl-mode--turn-on (project)
  "Turn on `unrepl-mode' in current buffer and associate PROJECT to it."
  (setq-local unrepl-conn-id (unrepl-project-id project))
  (unrepl-mode t)
  project)


(defun unrepl-mode-turn-on (conn-id-or-project &optional buffer)
  "Turn on `unrepl-mode' in BUFFER and associate CONN-ID-OR-PROJECT to it.
If BUFFER is nil, use current buffer.
Return the connected project."
  (with-current-buffer (or buffer (current-buffer))
    (let ((project (if (symbolp conn-id-or-project)
                       (unrepl-projects-get conn-id-or-project))))
      (unrepl-mode--turn-on project))))


(defun unrepl-mode-conditionally-turn-on ()
  "Turn on `unrepl-mode' only if variable `buffer-file-name' belongs to an existing project."
  (when-let (project (unrepl-mode--find-project-by-file-name))
    (unrepl-mode--turn-on project)))


(declare-function unrepl--connection-prompt "unrepl")
(defun unrepl-ensure-connected! ()
  "Make sure an `unrepl-conn-id' exists for current buffer.
If this local variable is not already set, tries to find a good candidate
by looking at the buffer's file path and comparing to existing
`unrepl-projects'.
If that fails and `unrepl-ask-for-connection' is non-nil, asks the user for
an *existing* host:port connection to connect to.  If everything else
fails, raise an error.

Return a UNREPL project"
  (if-let (project (and unrepl-conn-id
                        (unrepl-projects-get unrepl-conn-id)))
      project
    (if-let (project (unrepl-mode-conditionally-turn-on))
        project
      (if unrepl-ask-for-connection
          (seq-let [project _] (unrepl--connection-prompt (unrepl-clojure-dir))
            (unrepl-mode--turn-on project))
        (error "Could not find an UNREPL connection for this buffer")))))


(defmacro with-current-project (&rest body)
  "Ensure the current buffer is connected and put its project in BODY's local scope."
  `(let ((project (unrepl-ensure-connected!)))
     ,@body))


(defmacro unrepl-binding-print-limits (bindings &rest body)
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
   (let ((conn-id (unrepl-project-id project))
         (interrupt (lambda (pe)
                      (let* ((actions (unrepl-pending-eval-entry-actions pe))
                             (interrupt-templ (unrepl-ast-map-elt actions :interrupt)))
                        (unrepl-aux-send (unrepl-command-template interrupt-templ)
                                         (lambda (_) (message "Evaluation interrupted!")))))))
     (if-let (pending-eval (or (unrepl-pending-eval :client conn-id)
                               (unrepl-pending-eval :aux conn-id)))
         (funcall interrupt pending-eval)
       (message "No evaluations pending...")))))


(defun unrepl-eval-buffer (&optional buffer)
  "Eval BUFFER's file in UNREPL.
If no buffer is provided the command acts on the current buffer."
  (interactive)
  (check-parens)
  (with-current-project
   (with-current-buffer (or buffer (current-buffer))
     (unless buffer-file-name
       (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
     (when (and (buffer-modified-p)
                (y-or-n-p (format "Save file %s? " buffer-file-name)))
       (save-buffer))
     (remove-overlays nil nil 'temporary t)
     (let ((filename (buffer-file-name buffer))
           ;; (ns-form  (cider-ns-form))
           (load-file-templ (unrepl-project-actions-get project :unrepl.el/load-file)))
       (unrepl-aux-send (unrepl-command-template
                         load-file-templ
                         `((:unrepl.el/file . ,(unrepl-file-string filename))
                           (:unrepl.el/file-name . ,(funcall unrepl-filename-function filename))
                           (:unrepl.el/file-path . ,(file-name-nondirectory filename))))
                        (lambda (payload)
                          (message "%s" (unrepl-ast-unparse-to-string payload 'mute-ui))))
       (message "Loading %s..." filename)))))



(defun unrepl-quit (&optional just-do-it conn-id)
  "Quit connection to CONN-ID or current `unrepl-conn-id'.
If JUST-DO-IT is non-nil, don't ask for confirmation."
  (interactive "P")
  (let ((conn-id (or conn-id
                     unrepl-conn-id)))
    (if-let (project (unrepl-projects-get conn-id))
        (when (or just-do-it
                  (y-or-n-p (format "Are you sure you want to quit connection to %s? " conn-id)))
          (unrepl-project-quit conn-id)
          (message "UNREPL connection to %s terminated" conn-id))
      (error "Connection %s could not be found" conn-id))))



;; Completion
;; -------------------------------------------------------------------

(defun unrepl-completion-get-context-at-point ()
  "Extract the context at point.
Parse the \"top-level\" form where point is, if any, and replaces the symbol
where point is by a symbol `__prefix__'."
  )


(defun unrepl-completion-get-context ()
  "Extract context depending on `cider-completion-use-context' and major mode.
BORROWED FROM CIDER."
  (let ((context (when (derived-mode-p 'clojure-mode)
                   ;; Important because `beginning-of-defun' and
                   ;; `ending-of-defun' work incorrectly in the REPL
                   ;; buffer, so context extraction fails there.
                   (unrepl-completion-get-context-at-point))))
    (if (string= unrepl-completion-last-context context)
        :same
      (setq unrepl-completion-last-context context)
      context)))


(declare-function unrepl-aux-sync-request "unrepl-loop")
(defun unrepl-complete-candidates (str &optional ns)
  "Find completion candidates for STR.
NS is an optional namespace symbol."
  (with-current-project
   (let* ((context (unrepl-completion-get-context))
          (complete-tmpl (-> project
                             (unrepl-project-actions)
                             (unrepl-ast-map-elt :unrepl.el/complete)))
          (candidates (unrepl-aux-sync-request
                       (unrepl-command-template complete-tmpl
                                                `((:unrepl.el/prefix . ,str)
                                                  (:unrepl.el/context . ,context)
                                                  (:unrepl.el/ns . ,ns))))))
     (mapcar
      (lambda (candidate-node)
        (let* ((node-get (lambda (key) (-> candidate-node
                                      (unrepl-ast-map-elt key)
                                      (parseclj-ast-value))))
               (candidate (funcall node-get :candidate))
               (type (funcall node-get :type))
               (ns (funcall node-get :ns)))
          (put-text-property 0 1 'type type candidate)
          (put-text-property 0 1 'ns ns candidate)
          candidate))
      (parseclj-ast-children candidates)))))


(defun unrepl-complete-at-point ()
  "Complete the symbol at point.
Used as a `completion-at-point-functions' function.
BORROWED FROM CIDER."
  (when (not (or (unrepl-in-string-p) (unrepl-in-comment-p)))
    (when-let (bounds (bounds-of-thing-at-point 'symbol))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic #'unrepl-complete-candidates)
            ;; :annotation-function #'cider-annotate-symbol
            ;; :company-doc-buffer #'cider-create-doc-buffer
            ;; :company-location #'cider-company-location
            ;; :company-docsig #'cider-company-docsig
            ))))



;; Setup
;; -------------------------------------------------------------------

(defconst unrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-TAB") #'complete-symbol)
    (define-key map (kbd "C-c C-z") #'unrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-x C-e") #'unrepl-eval-last-sexp)
    (define-key map (kbd "C-c C-r") #'unrepl-inspect-last-eval)
    (define-key map (kbd "C-c C-b") #'unrepl-eval-buffer)
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
  (if unrepl-mode
      (progn
        (make-local-variable 'completion-at-point-functions)
        (add-to-list 'completion-at-point-functions
                     #'unrepl-complete-at-point))
    (mapc #'kill-local-variable '(unrepl-conn-id
                                  completion-at-point-functions))))


(provide 'unrepl-mode)

;;; unrepl-mode.el ends here
