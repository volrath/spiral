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

(defcustom unrepl-use-overlays t
  "Whether to display evaluation results with overlays."
  :type 'boolean
  :group 'unrepl)

(defcustom unrepl-eval-result-prefix "=> "
  "Prefix displayed before a evaluation result value."
  :type 'string
  :group 'unrepl)

(defvar-local unrepl-conn-id nil
  "Port number used when creating a new Socket REPL.")


(defun unrepl-mode--conn-id-prompt ()
  "Prompt the user for a HOST:PORT conn-id.
Return a conn-id symbol."
  (intern (format "%s:%s"
                  (read-string "Host: ")
                  (read-string "Port: "))))


(defun unrepl-connected-buffers (conn-id)
  "List all buffers connected to CONN-ID."
  ;; We could maybe add a buffer list to the project data structure, but it can
  ;; get tricky to maintain synced pretty easily.
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (and (bound-and-true-p unrepl-mode)
            (eql unrepl-conn-id conn-id))))
   (buffer-list)))


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


(defun unrepl-mode--find-connection-by-file-name ()
  "Check the current buffer file name and try to find a matching UNREPL connection."
  nil)


;; Evaluation
;; -------------------------------------------------------------------

(defun unrepl-mode--make-result-overlay (_value _point)
  "Display VALUE in an overlay at POINT."
  )


(defun unrepl-mode--display-evaluation (eval-payload &optional point)
  "Display evaluation result EVAL-PAYLOAD as a string.
This function will put an unparsed version of EVAL-PAYLOAD in the echo
area, font-locked as Clojure.
If POINT and `unrepl-use-overlays' are non-nil, VALUE will also be
displayed in an overlay starting at POINT.

BORROWED FROM CIDER."
  (let ((value (unrepl-ast-unparse-to-string eval-payload)))
    (when (and point unrepl-use-overlays)
      (unrepl-mode--make-result-overlay value point))
    (message "%s%s" unrepl-eval-result-prefix value)))


(declare-function unrepl-client-send "unrepl-repl")
(defun unrepl-eval-last-sexp ()
  "Evaluate the expression preceding point."
  (interactive)
  (unrepl-ensure-connected!)
  (unrepl-client-send (unrepl-last-sexp)
                      (lambda (result)
                        (unrepl-mode--display-evaluation result (end-of-line)))))


;; Interactive Commands
;; -------------------------------------------------------------------

(defun unrepl-switch-to-repl-buffer ()
  "Switch to the REPL buffer for `unrepl-conn-id'."
  (interactive)
  (with-current-project
   (let ((repl-buffer (unrepl-project-repl-buffer project)))
     (if unrepl-display-repl-in-current-window
         (pop-to-buffer-same-window repl-buffer)
       (pop-to-buffer repl-buffer)))))


(declare-function unrepl-aux-send "unrepl-loop")
(defun unrepl-eval-interrupt ()
  "Interrupt pending evaluation."
  (interactive)
  (with-current-project
   (if-let (pending-eval (unrepl-project-pending-eval project))
       (let* ((actions (unrepl-project-pending-eval-entry-actions pending-eval))
              (interrupt-templ (unrepl-ast-map-elt actions :interrupt)))
         (unrepl-aux-send (unrepl-command-template interrupt-templ)))
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
    (define-key map (kbd "C-c C-g") #'unrepl-eval-interrupt)
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
