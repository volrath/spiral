;;; unrepl-repl.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-repl.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Nov 10 18:05:43 2017 (+0100)
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

(require 'clojure-mode)

(require 'unrepl-mode)
(require 'unrepl-project)


(defcustom unrepl-pop-repl-on-connect t
  "Pop REPL buffer on connect to new project.
When nil, the REPL buffer will be created but not displayed."
  :type 'boolean
  :group 'unrepl)

(defvar-local unrepl-repl-input-start-mark nil
  "Point marker of current input start.")


(defun unrepl-repl-newline-and-indent ()
  "Insert a new line, then indent."
  (newline))


(defun unrepl-repl--input-complete-p (end)
  "Return t if the input region is a complete sexp.
The input region is calculated as the region from
`unrepl-repl-input-start-mark' to END.

BORROWED FROM CIDER."
  (save-excursion
    (let ((start unrepl-repl-input-start-mark))
      (goto-char start)
      (cond ((looking-at-p "\\s *[@'`#]?[(\"]")
             (ignore-errors
               (save-restriction
                 (narrow-to-region start end)
                 ;; Keep stepping over blanks and sexps until the end of
                 ;; buffer is reached or an error occurs. Tolerate extra
                 ;; close parens.
                 (cl-loop do (skip-chars-forward " \t\r\n)")
                          until (eobp)
                          do (forward-sexp))
                 t)))
            (t t)))))


(declare-function unrepl-loop-send "unrepl-loop")
(defun unrepl-repl-return (&optional end-of-input)
  "Send the current input string to UNREPL for evaluation.

Input is expected to be a complete expression (whole form).  In case of
invoking this function with an incomplete/unbalanced expression as input,
this function will only insert a new line but won't send anything to UNREPL
for evaluation.

END-OF-INPUT as non-nil will override this behavior and send whatever input
there is in there.

Most of the behavior is BORROWED FROM CIDER."
  (interactive "P")
  (cond
   (end-of-input
    (unrepl-loop-send))
   ((unrepl-repl--input-complete-p (point-max))
    (unrepl-loop-send)
    (newline))
   (t
    (unrepl-repl-newline-and-indent)
    (message "[input not complete]"))))


(defun unrepl-repl-insert (str)
  "Insert STR text in REPL buffer."
  (insert str))


(defun unrepl-repl-quit-project (&optional just-do-it)
  "Quit the project this REPL belongs to.
If JUST-DO-IT is non-nil, don't ask for confirmation."
  (interactive "P")
  (when (or just-do-it
            (y-or-n-p "Are you sure? "))
    (unrepl-quit-project)))


;; REPL Buffer
;; -------------------------------------------------------------------

(defun unrepl-repl-buffer-name (conn-id)
  "Return a proper name for an UNREPL REPL to CONN-ID."
  (format "UNREPL [%s]" conn-id))

(defun unrepl-repl-create-buffer (conn-id)
  "Create a new UNREPL buffer for a connection CONN-ID.

This function would kill any buffer that share's the same CONN-ID, to
guarantee a fresh start.

Associates to it some control local variables:
- `unrepl-current-namespace': holds the current namespace in UNREPL, as
  provided by the `:prompt' message.
- `unrepl-repl-ngid': next group-id to be processed by UNREPL, starting
  with 1.
- `unrepl-repl-pending-evals': an AList to store pending evaluations.  Keys
  are UNREPL group ids and values would be keywords representing the last
  received message for said group."
  (let ((buf-name (unrepl-repl-buffer-name conn-id)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((repl-buffer (get-buffer-create buf-name)))
      (with-current-buffer repl-buffer
        (unless (derived-mode-p 'unrepl-repl-mode)
          (unrepl-repl-mode))
        ;; Init REPL
        (setq-local unrepl-conn-id conn-id)
        (insert "Waiting on UNREPL... ")
        (when unrepl-pop-repl-on-connect
          (pop-to-buffer repl-buffer))))))


;; UNREPL REPL mode
;; -------------------------------------------------------------------

(defvar unrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unrepl-repl-return)
    (define-key map (kbd "C-c C-q") #'unrepl-repl-quit-project)
    map))

(defvar unrepl-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(define-derived-mode unrepl-repl-mode fundamental-mode "REPL"
  "Major mode for Clojure UNREPL interactions.

\\{unrepl-repl-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (unrepl-mode)
  (set-syntax-table unrepl-repl-mode-syntax-table)
  ;; TODO: eldoc
  (hack-dir-local-variables-non-file-buffer))

(provide 'unrepl-repl)

;;; unrepl-repl.el ends here
