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
(require 'dash)

(require 'unrepl-mode)
(require 'unrepl-project)


(defgroup unrepl-repl nil
  "UNREPL interactive repl"
  :prefix "unrepl-repl-"
  :group 'unrepl)

(defcustom unrepl-repl-pop-on-connect t
  "Pop REPL buffer on connect to new project.
When nil, the REPL buffer will be created but not displayed."
  :type 'boolean
  :group 'unrepl-repl)

(defcustom unrepl-repl-group-stdout t
  "Group evaluation's output."
  :type 'boolean
  :group 'unrepl-repl)

(defface unrepl-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-repl-prompt-result-face
  '((t (:inherit font-lock-warning-face)))
  "Face for the result prompt in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-repl-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face for constant things in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-repl-stdout-face
  '((t (:inherit font-lock-string-face)))
  "Face for STDOUT output in the REPL buffer."
  :group 'unrepl-repl)

(defvar-local unrepl-repl-input-start-mark 1
  "Point marker of current input start.")

(defvar-local unrepl-repl-inputting nil
  "Boolean value that indicates if the latest input sent to the server sent
  using the REPL.")

(defvar-local unrepl-repl-history nil
  "A list that holds history entries.
A History Entry is a 3-tuple: the input string, an UNREPL group id, and a
prompt position in buffer.")


;; Utilities
;; -------------------------------------------------------------------

(defmacro unrepl-propertize-region (props &rest body)
  "Add PROPS to all the inserted text while executing BODY.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY.

BORROWED FROM CIDER."
  (declare (indent 1))
  (let ((start (make-symbol "start")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))


(defun unrepl-repl-newline-and-indent ()
  "Insert a new line, then indent."
  (newline))


(defun unrepl-repl--in-input-area-p ()
  "Return t if in input area."
  (<= unrepl-repl-input-start-mark (point)))


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


(defmacro with-current-repl (&rest body)
  "Automatically switch to the inferred REPL buffer and eval BODY.
This macro needs a `conn-id' variable in the scope, otherwise it will throw
an error.
A `project' variable will be added to the local scope."
  `(if conn-id
       (let* ((project (unrepl-projects-get conn-id))
              (repl-buffer (unrepl-project-repl-buffer project)))
         (with-current-buffer repl-buffer
           ,@body))
     (error "Couldn't find a connection ID for this REPL")))


;; History
;; -------------------------------------------------------------------

(defun unrepl-repl--history-add (entry)
  "Add History ENTRY to `unrepl-repl-history'."
  (push entry unrepl-repl-history))


(defun unrepl-repl--make-history-entry (str)
  "Create a History Entry for STR and return it."
  (list
   str
   nil ;; pending eval's group-id -- optional
   nil ;; next prompt position
   ))


(defun unrepl-repl--history-get (idx)
  "Return the history entry with reverse index IDX.
Indices, as saved in pending evaluations, start with 1."
  (nth
   (- (length unrepl-repl-history) idx)
   unrepl-repl-history))


(defun unrepl-repl--history-entry-str (entry)
  "Return the string of the given History ENTRY."
  (car entry))


(defun unrepl-repl--history-entry-group-id (entry)
  "Return the UNREPL group id of the given History ENTRY."
  (cadr entry))


(defun unrepl-repl--history-entry-prompt-pos (entry)
  "Return the prompt position of the given History ENTRY."
  (cl-caddr entry))


(defun unrepl-repl--add-input-to-history (str)
  "Add input STR to history."
  (unless (string= str "")
    (unrepl-repl--history-add (unrepl-repl--make-history-entry str))))


(defun unrepl-repl--history-add-gid-to-top-entry (group-id)
  "Add GROUP-ID to the top entry in history."
  (setf (cadr (car unrepl-repl-history)) group-id))


(defun unrepl-repl--history-set-prompt-pos (history-idx pos &optional override)
  "Set POS as the HISTORY-IDX entry `prompt-pos'.
Optional arg OVERRIDE indicates if previously set prompt pos should be
ignored."
  (let ((entry (unrepl-repl--history-get history-idx)))
    (when (and entry
               (or (not (unrepl-repl--history-entry-prompt-pos entry))
                   override))
      (setf
       (cl-caddr
        (nth
         (- (length unrepl-repl-history) history-idx)
         unrepl-repl-history))
       pos))))


(defun unrepl-repl-input-history-assoc (conn-id group-id)
  "Possibly return a list =(:history-entry-id <some id>)=.
Check CONN-ID REPL to see if `unrepl-repl-inputting' is true.  If so,
return the tuple using the latest history id
available.  nil otherwise.

This function includes an important side effect: If REPL is inputting, the
latest history entry will be associated with GROUP-ID."
  (with-current-repl
   (when (and unrepl-repl-inputting
              unrepl-repl-history)
     (unrepl-repl--history-add-gid-to-top-entry group-id)
     `(:history-entry-id ,(length unrepl-repl-history)))))


;; Interactive
;; -------------------------------------------------------------------

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
  (unless (unrepl-repl--in-input-area-p)
    (error "No input at point"))
  (let ((start unrepl-repl-input-start-mark)
        (end (point-max)))
    (cond
     ;; (end-of-input
     ;;  (unrepl-loop-send start (point)))
     ((unrepl-repl--input-complete-p end)
      (newline)
      (goto-char (point-max))
      (add-text-properties unrepl-repl-input-start-mark (point)
                           '(read-only t rear-nonsticky (read-only)))
      (-> (unrepl-loop-send start end)
          (unrepl-repl--add-input-to-history))
      (setq-local unrepl-repl-inputting t))
     (t
      (unrepl-repl-newline-and-indent)
      (message "[input not complete]")))))


(defun unrepl-repl-quit-project (&optional just-do-it)
  "Quit the project this REPL belongs to.
If JUST-DO-IT is non-nil, don't ask for confirmation."
  (interactive "P")
  (when (or just-do-it
            (y-or-n-p "Are you sure you want to quit? "))
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
- `unrepl-repl-history': holds the current history of this REPL."
  (let ((buf-name (unrepl-repl-buffer-name conn-id)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((repl-buffer (get-buffer-create buf-name)))
      (with-current-buffer repl-buffer
        (unless (derived-mode-p 'unrepl-repl-mode)
          (unrepl-repl-mode))
        ;; Init REPL
        (setq-local unrepl-conn-id conn-id)
        (-> ";; Waiting on UNREPL... "
            (propertize 'font-lock-face 'font-lock-comment-face)
            (insert))
        (when unrepl-repl-pop-on-connect
          (pop-to-buffer repl-buffer))))))


(defun unrepl-repl-connected (conn-id)
  "Init the REPL buffer for CONN-ID."
  (with-current-repl
   (-> "Connected to %s\n"
       (format conn-id)
       (propertize 'font-lock-face 'font-lock-comment-face)
       (insert))))


(defun unrepl-repl--build-prompt (history-id namespace &optional result)
  "Build a prompt for HISTORY-ID and NAMESPACE.
RESULT is a boolean flag.  When not nil, the prompt is said to be a result
prompt, which is use to show results of evaluations."
  (let ((ns (if result
                (make-string (length namespace) ?\s)
              namespace))
        (font-face (if result
                       'unrepl-repl-prompt-result-face
                     'unrepl-repl-prompt-face)))
    (-> "%s [%s]=> "
        (format ns history-id)
        (propertize 'font-lock-face font-face
                    'field 'unrepl-repl-prompt-field
                    'intangible t
                    'read-only t
                    'rear-nonsticky '(field font-lock-face intangible read-only)))))


(defun unrepl-repl-prompt (conn-id)
  "Insert prompt in CONN-ID'S REPL."
  (with-current-repl
   (unless (bolp)
     (insert (propertize "%\n" 'font-lock-face 'unrepl-repl-constant-face)))
   (goto-char (point-max))
   ;; Tell previous history entry that the new prompt starts here
   (when unrepl-repl-history
     (unrepl-repl--history-set-prompt-pos (length unrepl-repl-history)
                                          (point)))
   ;; Insert prompt
   (insert
    (unrepl-repl--build-prompt (1+ (length unrepl-repl-history))
                               (unrepl-project-namespace project)))
   ;; Mark current input start
   (setq-local unrepl-repl-input-start-mark (point))
   ;; Reset the `unrepl-repl-inputting' variable.
   (setq-local unrepl-repl-inputting nil)))


(defun unrepl-repl-insert-evaluation (conn-id history-id evaluation)
  "In CONN-ID REPL buffer, get history entry with HISTORY-ID and print EVALUATION at the end of it."
  (with-current-repl
   (unless (bolp)
     (insert (propertize "%\n" 'font-lock-face 'unrepl-repl-constant-face)))
   (insert
    (unrepl-repl--build-prompt history-id
                               (unrepl-project-namespace project)
                               t)
    (format "%S\n" evaluation))))


(defun unrepl-repl-insert-out (conn-id history-id str)
  "Print STR with HISTORY-ID in CONN-ID REPL."
  (with-current-repl
   (let ((propertized-str (propertize str 'font-lock-face 'unrepl-repl-stdout-face)))
     (if (and unrepl-repl-group-stdout
              (< history-id (length unrepl-repl-history)))  ;; if there's another prompt already
         (save-excursion
           (goto-char (-> history-id
                          (unrepl-repl--history-get)
                          (unrepl-repl--history-entry-prompt-pos)))
           (insert propertized-str)
           (unrepl-repl--history-set-prompt-pos (1+ history-id) (point) t))
       (insert propertized-str)))))


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
