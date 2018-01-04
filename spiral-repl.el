;;; spiral-repl.el --- REPL interactions -*- lexical-binding: t; -*-
;;
;; Filename: spiral-repl.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Nov 10 18:05:43 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; REPL interactions
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

(require 'avy)
(require 'clojure-mode)
(require 'highlight)
(require 'subr-x)

(require 'spiral-mode)
(require 'spiral-project)
(require 'spiral-stacktrace)
(require 'spiral-util)


(defgroup spiral-repl nil
  "SPIRAL interactive repl"
  :prefix "spiral-repl-"
  :group 'spiral)

(defcustom spiral-repl-pop-on-connect t
  "Pop REPL buffer on connect to new project.
When nil, the REPL buffer will be created but not displayed."
  :type 'boolean
  :group 'spiral-repl)

(defcustom spiral-repl-group-stdout t
  "Group evaluation's output."
  :type 'boolean
  :group 'spiral-repl)

(defcustom spiral-repl-print-length 10
  "Set the maximum length of elements in sequences to be printed before elision.
This variable sets `clojure.core/*print-length*' in the running Socket
REPL.  If nil, UNREPL's default value will be used.  If `max', the maximum
number will be used (Long/MAX_VALUE)."
  :type '(choice integer (const nil) (const 'max))
  :group 'spiral-repl)

(defcustom spiral-repl-print-level 8
  "Set the number of levels deep the printer will print nested objects.
This variable sets `clojure.core/*print-level*' in the running Socket REPL.
If nil, UNREPL's default value will be used.  If `max', the maximum
number will be used (Long/MAX_VALUE)."
  :type '(choice integer (const nil) (const 'max))
  :group 'spiral-repl)

(defcustom spiral-repl-stdout-string-length 'max
  "Set the maximum length of stdout strings before elision.
If nil, UNREPL's default value will be used.  If `max', the maximum
number will be used (Long/MAX_VALUE)."
  :type '(choice integer (const nil) (const 'max))
  :group 'spiral-repl)

(defvar-local spiral-repl-prompt-start-mark nil
  "Point marker of current prompt start.")

(defvar-local spiral-repl-input-start-mark nil
  "Point marker of current input start.")

(defvar-local spiral-repl-inputting nil
  "Boolean value that indicates if the latest input sent to the server sent
  using the REPL.")

(defvar-local spiral-repl-transient-text-gid nil
  "Group ID of the last output displaying as transient text.")

(defvar-local spiral-repl-transient-text-start-mark nil
  "Marker to the beginning of a transient text, or nil if there's none.")

(defvar-local spiral-repl-transient-text-end-mark nil
  "Marker to the end of a transient text, or nil if there's none.")

(defvar-local spiral-repl-history nil
  "A list that holds history entries.
A History Entry is a 3-tuple: the input string, an SPIRAL group id, and a
prompt position in buffer.")

(defvar-local spiral-repl-history-lookup nil
  "A number that represents the current history index being looked upon
  when searching through history.  When nil, search is inactive.")



;; Common Faces
;; -------------------------------------------------------------------

(defface spiral-font-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-result-prompt-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for the result prompt in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-exception-prompt-face
  '((t (:inherit font-lock-warning-face)))
  "Face for the result prompt in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face for constant things in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for constant things in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-class-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for constant things in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-stdout-face
  '((t (:inherit font-lock-doc-face)))
  "Face for STDOUT output in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-stderr-face
  '((t (:inherit compilation-error)))
  "Face for STDERR output in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-doc-face
  '((t (:inherit font-lock-comment-face)))
  "Face for auto-documentation in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-notification-info-face
  '((t (:inherit compilation-line-number)))
  "Face for auto-documentation in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-notification-warn-face
  '((t (:inherit compilation-warning)))
  "Face for auto-documentation in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-notification-error-face
  '((t (:inherit compilation-error)))
  "Face for auto-documentation in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-notification-msg-face
  '((t (:inherit compilation-common-part)))
  "Face for auto-documentation in the REPL buffer."
  :group 'spiral-repl)

(defface spiral-font-tooltip-face
  '((t . (:inherit button)))
  "Face for tooltips."
  :group 'spiral-repl)



;; Utilities
;; -------------------------------------------------------------------


(defun spiral-repl--quit-confirm ()
  "Ask for confirmation before quitting the REPL."
  (y-or-n-p
   (format
    (concat "By killing the buffer, you will be disconnecting all Clojure "
            "buffers from %S.  Are you sure you want to proceed?")
    spiral-conn-id)))


(defun spiral-repl--insert-newline (count)
  "Insert COUNT new lines.  Negative COUNT is not allowed."
  (let ((count (or count 1)))
    (while (> count 0)
      (insert "\n")
      (cl-decf count))))


(defun spiral-repl--recenter (&optional arg)
  "Recenter REPL buffer's window.
ARG is the same as in `recenter'."
  (when-let (win (get-buffer-window (current-buffer) t))
    (with-selected-window win
      (set-window-point win (point-max))
      (recenter arg))))


(defun spiral-repl--newline-if-needed ()
  "Go to max point in buffer and make sure it is the beginning of a new line."
  (unless (bolp)
    (insert (propertize "%\n" 'font-lock-face 'spiral-font-constant-face))))


(defun spiral-repl--newline-and-indent (&optional count)
  "Insert COUNT new lines, then indent."
  (spiral-repl--insert-newline count)
  (lisp-indent-line))


(defun spiral-repl-newline-and-scroll (&optional count)
  "Insert COUNT new lines and scroll til the end of the buffer.
COUNT defaults to 1, negative numbers are not allowed."
  (spiral-repl--insert-newline count)
  (when (eobp)
    (spiral-repl--recenter -1)))


(defun spiral-repl--input-str ()
  "Return the current input string in REPL buffer."
  (buffer-substring-no-properties spiral-repl-input-start-mark (point-max)))


(defun spiral-repl--in-input-area-p ()
  "Return t if in input area."
  (<= spiral-repl-input-start-mark (point)))


(defun spiral-repl--input-complete-p (end)
  "Return t if the input region is a complete sexp.
The input region is calculated as the region from
`spiral-repl-input-start-mark' to END.

BORROWED FROM CIDER."
  (save-excursion
    (let ((start spiral-repl-input-start-mark))
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


(defun spiral-repl--interactive-input-p (group-id)
  "Return whether GROUP-ID belongs to an interactive input.
Interactive inputs are those that were not sent via REPL, hence do not have
a REPL history entry.

This function should only be called inside REPL buffers."
  (let ((last-history-group-id (thread-first spiral-repl-history
                                 (car)
                                 (spiral-repl--history-entry-group-id)))
        (pending-eval-group-id (spiral-pending-eval-group-id :client spiral-conn-id)))
    (and (eql group-id pending-eval-group-id)
         (not (eql group-id last-history-group-id)))))


(defun spiral-repl--find-next-prompt (&optional backwards)
  "Find the beginning pos of the next prompt in the buffer, or nil if none.
If BACKWARDS is non-nil, search backwards."
  (let ((p (point))
        (current-field nil)
        (nav-fn (if backwards
                    #'previous-single-char-property-change
                  #'next-single-char-property-change)))
    (while (not (or (and backwards (= p 1))
                    (and (not backwards) (= p (point-max)))
                    (eql current-field 'spiral-repl-prompt-field)))
      (setq p (funcall nav-fn p'field))
      (setq current-field (get-char-property p 'field)))
    (save-excursion
      (goto-char p)
      (line-beginning-position))))


(defun spiral-repl--goto-char (pos)
  "Move current buffer's cursor to POS, no matter what!
This function works when the buffer is active, in a viewable window, or
buried."
  (dolist (window (get-buffer-window-list (current-buffer)))
    (set-window-point window pos))
  (goto-char pos))


(defun spiral-repl--move-cursor-to-point-max ()
  "Move current buffer's cursor to `point-max', no matter what!"
  (spiral-repl--goto-char (point-max)))


(defun spiral-repl-move-to-next-prompt (&optional backwards)
  "Move current buffer's cursor to the beginning position of next prompt.
I there's no next prompt, goes to `point-max'.

If BACKWARDS is non-nil, goes to previous point (next point backwards)."
  (spiral-repl--goto-char (spiral-repl--find-next-prompt backwards)))


(defun spiral-repl-move-to-last-prompt ()
  "Move to the beginning position of the last prompt."
  (spiral-repl--move-cursor-to-point-max)
  (spiral-repl-move-to-next-prompt 'backwards))


(defmacro with-current-repl (&rest body)
  "Automatically switch to the inferred REPL buffer and eval BODY.
This macro needs a `conn-id' variable in the scope, otherwise it will throw
an error.
A `project' variable will be added to the local scope."
  `(let* ((project (if (boundp 'conn-id)
                       (spiral-projects-get conn-id)
                     (spiral-ensure-connected!)))
          (repl-buffer (spiral-project-repl-buffer project)))
     (with-current-buffer repl-buffer
       ,@body)))



;; Notifications
;; -------------------------------------------------------------------

(defmacro spiral-repl-highlighted-block (&rest body)
  "Create a highlighted text block with BODY insertions.
The highlighted text block is preceded and followed by empty lines, and
it's highlighted with `hl-line' background color."
  `(progn
     (unless (bolp) (insert "\n"))
     (let ((start (point)))
       (insert "\n")
       ,@body
       (insert "\n")
       (hlt-highlight-region start (point)))
     (spiral-repl--recenter -1)))

(defun spiral-repl-notify (type title &optional msg more)
  "Create a highlighted text block for a message containing TITLE.
TYPE is any of the following symbols: `info', `warn', `error'.
TITLE is a text that will be inserted with a special font locking face
depending on TYPE.
MSG is a notification message as a string.
MORE is another notification message that will be hidden behind a 'Show
More' button."
  (with-current-repl
   (let ((type-face (cl-case type
                      ('info  'spiral-font-notification-info-face)
                      ('warn  'spiral-font-notification-warn-face)
                      ('error 'spiral-font-notification-error-face)
                      (t (when spiral-debug
                           (error "Unrecognized notification type: %s" type))))))
     (save-excursion
       (spiral-repl-move-to-last-prompt)
       (spiral-repl-highlighted-block
        (insert (propertize title 'font-lock-face type-face) "\n")
        (when msg
          (insert "\n")
          (insert (propertize msg 'font-lock-face 'spiral-font-notification-msg-face) "\n")
          (when more
            (insert "\n")
            (spiral-button-throwaway-insert
             "[Show More]"
             (lambda (_button)
               (insert (propertize more 'font-lock-face 'spiral-font-notification-msg-face))))
            (insert "\n"))))))))



;; Phantom Input Entries
;; -------------------------------------------------------------------

(defun spiral-repl-insert-phantom-input (evaluation &optional payload display)
  "Insert a phantom input for EVALUATION.
Adds a History Entry for this new input, as if it were typed by a ghost.

Saves whatever there might be in the current input area, and inserts the
pending evaluation input, with its related PAYLOAD.  Then inserts a fresh
new prompt as if it were created by a `:prompt' message, and restores the
saved input.

DISPLAY is expected to be either 'pop or 'switch.  When non-nil, pops or
switches to the REPL buffer in another window."
  (with-current-repl
   (spiral-repl--transient-text-remove)
   (let* ((current-input (buffer-substring spiral-repl-input-start-mark (point-max)))
          (evaluation-input (spiral-pending-eval-entry-input evaluation))
          (payload (or payload (spiral-pending-eval-entry-payload evaluation)))
          (group-id (spiral-pending-eval-entry-group-id evaluation))
          (pending-eval-status (spiral-pending-eval-entry-status evaluation))
          (insert-payload-fn (pcase pending-eval-status
                               (:eval #'spiral-repl-insert-evaluation)
                               (:exception #'spiral-repl-insert-exception)
                               (:out (lambda (payload point &rest _)
                                       (spiral-repl-insert-std-stream :out payload point)))
                               (:err (lambda (payload point &rest _)
                                       (spiral-repl-insert-std-stream :err payload point)))
                               (_ (when spiral-debug
                                    (error "No phantom insert function for pending eval status %S"
                                           pending-eval-status))))))
     (goto-char spiral-repl-input-start-mark)
     ;; Insert phantom input and payload
     (delete-region spiral-repl-input-start-mark (point-max))
     (let ((p (point)))
       (insert evaluation-input)
       (spiral-repl-newline-and-scroll)
       (indent-region p (point)))
     (funcall insert-payload-fn
              payload
              (point)
              (1+ (length spiral-repl-history))
              (spiral-project-namespace project))
     ;; Insert it into history
     (spiral-repl--add-input-to-history evaluation-input)
     (spiral-repl--history-add-gid-to-top-entry group-id)
     ;; Insert new prompt
     (spiral-repl-prompt spiral-conn-id)
     ;; Restore current input
     (insert current-input)
     ;; Display the repl accordingly
     (when (and display
                (not (eql (current-buffer) (window-buffer (selected-window)))))
       (spiral-repl-display (current-buffer) display))
     ;; Move to the end of the repl
     (spiral-repl--move-cursor-to-point-max))))



;; Transient Text
;; -------------------------------------------------------------------

(defun spiral-repl--transient-text-insert (group-id text &optional properties)
  "Insert TEXT at the end of the REPL in a 'transient' way.
This TEXT is meant to be shown momentarily and to disappear at some
point (by calling `spiral-repl--transient-text-remove').

TEXT can be either a string or a AST node.  This function will either
insert it or unparse it accordingly.
GROUP-ID is an integer, and it's meant to identify text to be *appended* to
a transient text block.  If there's already a transient text showing for
GROUP-ID N, then any other subsequent call to
`spiral-repl--transient-text-insert' with the same GROUP-ID will be
appended to it.  If there's a call with a different GROUP-ID, the text will
be replaced.

PROPERTIES is a plist of text properties."
  (with-current-repl
   (unless (eql spiral-repl-transient-text-gid group-id)
     (spiral-repl--transient-text-remove)
     (setq-local spiral-repl-transient-text-gid group-id))
   (save-excursion
     ;; Find the right place to start inserting.
     (if (marker-position spiral-repl-transient-text-end-mark)
         (goto-char spiral-repl-transient-text-end-mark)
       (goto-char (point-max))
       (insert "\n"))
     ;; If start is not set already, set it to current position.
     (unless (marker-position spiral-repl-transient-text-start-mark)
       (set-marker spiral-repl-transient-text-start-mark (point)))
     ;; Insert text
     (let ((inhibit-read-only t))
       (spiral-propertize-region (append properties
                                         '(font-lock-face spiral-font-doc-face
                                                          read-only t
                                                          intangible t
                                                          field spiral-repl-transient-field))
         (spiral-ast-unparse-stdout-string text)))
     ;; And mark the end
     (set-marker spiral-repl-transient-text-end-mark (point)))
   (recenter)))


(defun spiral-repl--transient-text-remove ()
  "Remove transient text from the REPL buffer."
  (with-current-repl
   (when (and (marker-position spiral-repl-transient-text-start-mark)
              (marker-position spiral-repl-transient-text-end-mark))
     (save-excursion
       (let ((inhibit-read-only t))
         (goto-char spiral-repl-transient-text-start-mark)
         (delete-region spiral-repl-transient-text-start-mark
                        (point-max))
         (delete-char -1)))
     (set-marker spiral-repl-transient-text-start-mark nil)
     (set-marker spiral-repl-transient-text-end-mark nil))))



;; History
;; -------------------------------------------------------------------

(defun spiral-repl--history-add (entry)
  "Add History ENTRY to `spiral-repl-history'."
  (push entry spiral-repl-history))


(defun spiral-repl--make-history-entry (str)
  "Create a History Entry for STR and return it."
  (list
   (1+ (length spiral-repl-history))  ;; index
   str                                ;; input
   nil                                ;; pending eval's group-id -- optional
   nil                                ;; next prompt marker
   ))


(defun spiral-repl--history-get (idx)
  "Return the history entry with reverse index IDX.
Indices, as saved in pending evaluations, start with 1."
  (nth
   (- (length spiral-repl-history) idx)
   spiral-repl-history))


(defun spiral-repl--history-get-by-group-id (group-id)
  "Return a history entry for GROUP-ID, if any."
  (seq-find (lambda (e)
              (eql (spiral-repl--history-entry-group-id e)
                   group-id))
            spiral-repl-history))


(defun spiral-repl--history-entry-idx (entry)
  "Return the index of the given History ENTRY."
  (car entry))


(defun spiral-repl--history-entry-str (entry)
  "Return the string of the given History ENTRY."
  (cadr entry))


(defun spiral-repl--history-entry-group-id (entry)
  "Return the UNREPL group id of the given History ENTRY."
  (cl-caddr entry))


(defun spiral-repl--history-entry-prompt-marker (entry)
  "Return the prompt position of the given History ENTRY."
  (cl-cadddr entry))


(defun spiral-repl--add-input-to-history (str)
  "Add input STR to history."
  (unless (string= str "")
    (spiral-repl--history-add (spiral-repl--make-history-entry str))))


(defun spiral-repl--history-add-gid-to-top-entry (group-id)
  "Add GROUP-ID to the top entry in history."
  (setf (cl-caddr (car spiral-repl-history)) group-id))


(defun spiral-repl--history-set-prompt-marker (history-idx)
  "Set HISTORY-IDX entry `prompt-marker' to current point."
  (setf
   (cl-cadddr
    (nth
     (- (length spiral-repl-history) history-idx)
     spiral-repl-history))
   (point-marker)))


(defun spiral-repl-input-history-assoc (conn-id group-id)
  "Possibly return a list =(:repl-history-idx <some index>)=.
Check CONN-ID REPL to see if `spiral-repl-inputting' is true.  If so,
return the tuple using the latest history index available.  nil otherwise.

This function includes an important side effect: If REPL is inputting, the
latest history entry will be associated with GROUP-ID."
  (with-current-repl
   (when (and spiral-repl-inputting
              spiral-repl-history)
     (spiral-repl--history-add-gid-to-top-entry group-id)
     `(:repl-history-idx ,(length spiral-repl-history)))))



;; Interactive
;; -------------------------------------------------------------------

(declare-function spiral-client-send "spiral-loop")
(declare-function spiral-aux-send "spiral-loop")
(defun spiral-repl-return (&optional _end-of-input)
  "Send the current input string to UNREPL for evaluation.

Input is expected to be a complete expression (whole form).  In case of
invoking this function with an incomplete/unbalanced expression as input,
this function will only insert a new line but won't send anything to UNREPL
for evaluation.

END-OF-INPUT as non-nil will override this behavior and send whatever input
there is in there.

Most of the behavior is BORROWED FROM CIDER."
  (interactive "P")
  (unless (spiral-repl--in-input-area-p)
    (error "No input at point"))
  (spiral-repl--transient-text-remove)
  (cond
   ;; (end-of-input
   ;;  (spiral-client-send start (point)))
   ((spiral-repl--input-complete-p (point-max))
    (goto-char (point-max))
    (let ((history-idx (1+ (length spiral-repl-history))))
      (thread-first (spiral-repl--input-str)
        (spiral-client-send (lambda (result-payload)
                              (with-current-repl
                               (spiral-repl-insert-evaluation
                                result-payload nil
                                (spiral-project-namespace project)
                                history-idx))))
        (spiral-repl--add-input-to-history)))
    (spiral-repl-newline-and-scroll)
    (setq-local spiral-repl-inputting t))
   (t
    (spiral-repl--newline-and-indent)
    (message "[input not complete]"))))


(defun spiral-request-symbol-doc ()
  "Request `clojure.repl/doc' for `symbol-at-point' through the `:aux' conn.
Point needs to be in the REPL input.
If `:aux' returns a string of data, display it temporarily as stdout.  This
would get automatically removed after input is sent."
  (interactive)
  (when (spiral-repl--in-input-area-p)
    (when-let (sym (symbol-at-point))
      (with-current-repl
       (spiral-aux-send
        (spiral-project-templated-action project :spiral/doc
                                         :spiral/symbol sym)
        nil
        (lambda (stdout-payload group-id)
          (spiral-repl--transient-text-insert group-id
                                              stdout-payload)))))))


;; history

(defun spiral-repl--replace-input (str)
  "Replace the current REPL input with STR."
  (delete-region spiral-repl-input-start-mark (point-max))
  (goto-char (point-max))
  (insert str))


(defun spiral-repl--history-search-in-progress-p ()
  "Return t if there's a search in progress, by looking at `last-command'."
  (eq last-command 'spiral-repl--history-replace))


(defun spiral-repl--history-replace (direction-fn)
  "Replace current input with the next history input following DIRECTION-FN.
DIRECTION-FN is a function that takes a history index and returns a tuple
with the next history entry's idx and input string to be evaluated."
  (let* (next-in-history
         (history-size (length spiral-repl-history))
         (lookup (cond
                  ((spiral-repl--history-search-in-progress-p)
                   spiral-repl-history-lookup)
                  (t
                   (1+ history-size))))
         (current-input (spiral-repl--input-str)))
    (setq next-in-history (funcall direction-fn lookup))
    (while (and
            (<= 1 (car next-in-history) history-size)
            (string= (cdr next-in-history) current-input))
      (setq next-in-history (funcall direction-fn (car next-in-history))))
    (let ((idx (car next-in-history))
          (str (cdr next-in-history)))
      (cond
       ((< idx 1) (message "Beginning of history"))
       ((> idx history-size) (message "End of history"))
       (t (spiral-repl--replace-input str)))
      (setq-local spiral-repl-history-lookup idx))
    (setq this-command 'spiral-repl--history-replace)))


(defun spiral-repl--history-search-tuple (idx)
  "Helper function that return a tuple of (index, str) for a given IDX.
This function makes sure to not get out of history boundaries."
  (if (<= 1 idx (length spiral-repl-history))
      (cons idx (spiral-repl--history-entry-str
                 (spiral-repl--history-get idx)))
    (cons idx nil)))


(defun spiral-repl-previous-input ()
  "Replace current input with previous input in history."
  (interactive)
  (spiral-repl--history-replace
   (lambda (idx)
     (spiral-repl--history-search-tuple (1- idx)))))


(defun spiral-repl-next-input ()
  "Replace current input with previous input in history."
  (interactive)
  (spiral-repl--history-replace
   (lambda (idx)
     (spiral-repl--history-search-tuple (1+ idx)))))


;; Button navigation

(defun spiral-repl--avy-button-candidates ()
  "Return a list of `avy' candidates for SPIRAL buttons on the current buffer.
`avy' candidates take the following form: ((BEG . END) . WND), where BEG &
END are the beginning and end positions of the candidate, respectively, and
WND is the window of the candidate's window (in our case, the REPL buffer's
window)."
  (let* ((window (get-buffer-window (current-buffer)))
         (w-beg (window-start))
         (w-end (window-end window t))
         (pos w-beg)
         button
         candidates)
    (save-excursion
      (narrow-to-region w-beg w-end)
      (while (setq button (next-button pos 'count-current))
        (let ((b-start (button-start button))
              (b-end (button-end button)))
          (push (cons (cons b-start b-end)
                      window)
                candidates)
          (setq pos b-end)))
      (widen)
      (nreverse candidates))))


(defun spiral-repl-button-goto ()
  "Invoke an `avy' jump focused only on SPIRAL buttons."
  (interactive)
  (let ((avy-all-windows nil)
        (avy-background t))
    (avy--process
     (spiral-repl--avy-button-candidates)
     (avy--style-fn avy-style))))



;; REPL Buffer
;; -------------------------------------------------------------------

(defun spiral-repl-buffer-name (conn-id)
  "Return a proper name for a SPIRAL REPL to CONN-ID."
  (format "SPIRAL[%s]" conn-id))

(defun spiral-repl-create-buffer (conn-id server-buffer)
  "Create a new SPIRAL buffer for a connection CONN-ID.

This function would kill any buffer that share's the same CONN-ID, to
guarantee a fresh start.

Associates to it some control local variables:
- `spiral-repl-history': holds the current history of this REPL.

SERVER-BUFFER is a buffer for the Socket REPL server, or nil.  When
SERVER-BUFFER and `spiral-repl-pop-on-connect' are non nil, this function
will open the REPL buffer in SERVER-BUFFER window."
  (let ((buf-name (spiral-repl-buffer-name conn-id)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((repl-buffer (get-buffer-create buf-name)))
      (with-current-buffer repl-buffer
        (unless (derived-mode-p 'spiral-repl-mode)
          (spiral-repl-mode))
        ;; Init REPL
        (setq-local spiral-conn-id conn-id)
        (setq-local spiral-repl-prompt-start-mark (make-marker))
        (setq-local spiral-repl-input-start-mark (make-marker))
        (setq-local spiral-repl-transient-text-start-mark (make-marker))
        (setq-local spiral-repl-transient-text-end-mark (make-marker))
        (thread-first ";; Waiting on UNREPL... "
          (propertize 'font-lock-face 'font-lock-comment-face)
          (insert))
        (when spiral-repl-pop-on-connect
          (if (and server-buffer
                   (get-buffer-window server-buffer))
              (set-window-buffer (get-buffer-window server-buffer) repl-buffer)
            (pop-to-buffer repl-buffer)))
        repl-buffer))))


(defun spiral-repl-display (conn-id-or-buffer method)
  "Display REPL buffer using METHOD.
METHOD can be either 'switch or 'pop.  Defaults to 'pop.
CONN-ID-OR-BUFFER is either a connection id or a REPL buffer.
This function only if it's not displayed in another window already."
  (let ((repl-buffer (if (bufferp conn-id-or-buffer)
                         conn-id-or-buffer
                       (spiral-project-repl-buffer
                        (spiral-projects-get conn-id-or-buffer)))))
    (pcase method
      ('switch (switch-to-buffer-other-window repl-buffer t))
      ('pop (when (not (get-buffer-window repl-buffer))
              (pop-to-buffer repl-buffer nil t)))
      (_ (when spiral-debug
           (error "Unrecognized `spiral-repl-display' method: %S" method))))
    repl-buffer))


(defun spiral-repl-connected (conn-id)
  "Init the REPL buffer for CONN-ID."
  (with-current-repl
   (thread-first "Connected to %s\n"
     (format conn-id)
     (propertize 'font-lock-face 'font-lock-comment-face)
     (insert))))


(defun spiral-repl-disconnect-message (conn-id message)
  "Show a disconnection MESSAGE in REPL buffer for CONN-ID."
  (with-current-buffer (spiral-repl-display conn-id 'pop)
    (goto-char (point-max))
    (insert "\n\n"
            (propertize message 'font-lock-face 'spiral-font-exception-title-face))
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (point-max) '(read-only t)))))


(defun spiral-repl--build-prompt (history-id namespace)
  "Build a prompt for HISTORY-ID and NAMESPACE."
  (format "[%s] %s=> " history-id namespace))


(defun spiral-repl--build-result-indicator (_history-id _namespace)
  "Return an indicator for results of evaluation."
  "> ")


(defun spiral-repl--build-exception-indicator (_history-id _namespace)
  "Return an indicator for exception."
  "~ ")


(defun spiral-repl-prompt (conn-id)
  "Insert prompt in CONN-ID'S REPL."
  (with-current-repl
   (goto-char (point-max))
   (spiral-repl--newline-if-needed)
   ;; Remove 'field property from previous prompt
   ;; (when (marker-position spiral-repl-prompt-start-mark)
   ;;   (let ((inhibit-read-only t))
   ;;     (remove-text-properties spiral-repl-prompt-start-mark
   ;;                             spiral-repl-input-start-mark
   ;;                             '(field nil))))
   ;; The new prompt starts here, so we mark it.
   (set-marker spiral-repl-prompt-start-mark (point))
   ;; Tell previous history entry that the new prompt starts here
   (when spiral-repl-history
     (spiral-repl--history-set-prompt-marker (length spiral-repl-history)))
   ;; Insert prompt
   (spiral-propertize-region '(font-lock-face spiral-font-prompt-face
                                              field spiral-repl-prompt-field
                                              intangible t
                                              read-only t
                                              rear-nonsticky (field font-lock-face intangible read-only))
     (insert
      (spiral-repl--build-prompt (1+ (length spiral-repl-history))
                                 (spiral-project-namespace project))))
   ;; Mark current input start
   (set-marker spiral-repl-input-start-mark (point))
   ;; Reset the `spiral-repl-inputting' variable.
   (setq-local spiral-repl-inputting nil)
   (spiral-repl--move-cursor-to-point-max)))


(defun spiral-repl-insert-evaluation (eval-payload &optional point history-idx namespace)
  "In CONN-ID REPL buffer, unparse EVAL-PAYLOAD AST node at POINT.
If POINT is nil, unparse at the end of the buffer.
HISTORY-IDX and NAMESPACE are used to format/populate an exception prompt
for the REPL.  If any of them is nil, the exception prompt won't be
inserted."
  (goto-char (or point (point-max)))
  (spiral-repl--newline-if-needed)
  (spiral-propertize-region '(font-lock-face spiral-font-result-prompt-face
                                             intangible t
                                             read-only t
                                             rear-nonsticky (font-lock-face intangible read-only))
    (insert
     (spiral-repl--build-result-indicator history-idx namespace)))
  (spiral-ast-unparse eval-payload)
  (spiral-repl-newline-and-scroll))


(defun spiral-repl-insert-std-stream (type payload &optional point)
  "Insert std TYPE PAYLOAD at POINT.
TYPE can be either `:out' or `:err', and it's used to present the unparsed
PAYLOAD in formatted in a recognizable way.
PAYLOAD is either a string or a #unrepl/string tagged literal.
If POINT is nil, prints the payload right before the last prompt"
  (goto-char (or point spiral-repl-prompt-start-mark))
  (if (eql type :err)
      (spiral-propertize-region '(font-lock-face 'spiral-font-stderr-face)
        (spiral-ast-unparse-stdout-string payload))
    (spiral-ast-unparse-stdout-string payload))
  ;; Update marker.
  ;; If point was a marker, move it forward
  ;; If there was no point (*philosophical sadness rushes in*), move
  ;; `spiral-repl-prompt-start-mark' forward
  ;; Else, point was just a point, nothing to do.
  (when (markerp point)
    (set-marker point (point)))
  (unless point
    (set-marker spiral-repl-prompt-start-mark (point))))


(defun spiral-repl-handle-std-stream (type conn-id payload group-id)
  "Figure out where to unparse PAYLOAD for GROUP-ID in CONN-ID REPL.
TYPE can be either `:out' or `:err', and it's used to present the unparsed
PAYLOAD in formatted in a recognizable way.
STDOUT-PAYLOAD is either a string or a #unrepl/string tagged literal.
GROUP-ID is a number as described in UNREPL docs.

If GROUP-ID is not the same as the current pending evaluation's, and
`spiral-repl-group-stdout' is non nil, this function will try to figure out
a place in the REPL buffer where to print the STDOUT-PAYLOAD, by searching
through history for an entry that shares its same GROUP-ID.  If it can't
find a match, it will print it at the end of the buffer but before the last
prompt."
  (with-current-repl
   (if spiral-repl-group-stdout
       ;; Find the best place to print the output.
       (let ((pending-eval (spiral-pending-eval :client conn-id)))
         (if (spiral-repl--interactive-input-p group-id)  ;; interactive input
             (spiral-repl-insert-phantom-input pending-eval payload 'pop)
           (if-let (h-entry (spiral-repl--history-get-by-group-id group-id))
               (if (eql (spiral-repl--history-entry-group-id h-entry)
                        (spiral-pending-eval-entry-group-id pending-eval))  ;; last repl input
                   (spiral-repl-insert-std-stream type payload (point-max))
                 (save-excursion  ;; old repl input
                   (spiral-repl-insert-std-stream
                    type
                    payload
                    (spiral-repl--history-entry-prompt-marker h-entry))))
             (save-excursion  ;; didn't find a right place to print, printing before the last prompt
               (spiral-repl-insert-std-stream type payload)))))
     ;; Just print right before the last prompt
     (save-excursion
       (spiral-repl-insert-std-stream type payload)))))


(defun spiral-repl-insert-exception (payload &optional point history-idx namespace)
  "Insert exception PAYLOAD at POINT.
HISTORY-IDX and NAMESPACE are used to format/populate an exception prompt
for the REPL.  If any of them is nil, the exception prompt won't be
inserted."
  (goto-char (or point (point-max)))
  (spiral-repl--newline-if-needed)
  (when (and history-idx namespace)
    (spiral-propertize-region '(font-lock-face spiral-font-exception-prompt-face
                                               intangible t
                                               read-only t
                                               rear-nonsticky (font-lock-face intangible read-only))
      (insert
       (spiral-repl--build-exception-indicator history-idx namespace))))
  (spiral-stacktrace-insert payload))


(defun spiral-repl-handle-exception (conn-id payload group-id)
  "Figure out where to insert exception's PAYLOAD for GROUP-ID in CONN-ID's REPL."
  (with-current-repl
   (let ((namespace (spiral-project-namespace project)))
     (if (spiral-repl--interactive-input-p group-id)  ;; Interactive input
         (let ((pending-eval (spiral-pending-eval :client conn-id)))
           (spiral-repl-insert-phantom-input pending-eval payload 'switch))
       (let ((h-entry (spiral-repl--history-get-by-group-id group-id)))  ;; REPL input
         (when (and (not h-entry)  ;; an exception happened before `:read'
                    (spiral-repl--history-entry-group-id (car spiral-repl-history)))
           (spiral-repl--history-add-gid-to-top-entry group-id)
           (setq h-entry (car spiral-repl-history)))
         (spiral-repl-insert-exception
          payload
          (spiral-repl--history-entry-prompt-marker h-entry)
          (spiral-repl--history-entry-idx h-entry)
          namespace))))))



;; SPIRAL REPL mode
;; -------------------------------------------------------------------

(defvar spiral-repl-mode-hook nil
  "Hook for `spiral-repl-mode'.")

(defvar spiral-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'spiral-repl-return)
    (define-key map (kbd "C-c C-c") #'spiral-eval-interrupt)
    (define-key map (kbd "C-c C-d") #'spiral-request-symbol-doc)
    (define-key map (kbd "C-<up>") #'spiral-repl-previous-input)
    (define-key map (kbd "C-<down>") #'spiral-repl-next-input)
    (define-key map (kbd "M-s-.") #'spiral-repl-button-goto)
    map))

(defvar spiral-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(define-derived-mode spiral-repl-mode fundamental-mode "REPL"
  "Major mode for Clojure SPIRAL interactions.

\\{spiral-repl-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (spiral-mode)
  (set-syntax-table spiral-repl-mode-syntax-table)
  (add-hook 'kill-buffer-hook (lambda () (spiral-project-quit (spiral-projects-get spiral-conn-id))) 'append 'local)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list 'kill-buffer-query-functions #'spiral-repl--quit-confirm)
  ;; TODO: eldoc
  (hack-dir-local-variables-non-file-buffer)

  (run-hooks 'spiral-repl-mode-hook))

(provide 'spiral-repl)

;;; spiral-repl.el ends here
