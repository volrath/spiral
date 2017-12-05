;;; unrepl-repl.el --- REPL interactions -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-repl.el
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

(require 'clojure-mode)
(require 'dash)
(require 'subr-x)

(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-stacktrace)
(require 'unrepl-util)


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

(defvar-local unrepl-repl-prompt-start-mark nil
  "Point marker of current prompt start.")

(defvar-local unrepl-repl-input-start-mark nil
  "Point marker of current input start.")

(defvar-local unrepl-repl-inputting nil
  "Boolean value that indicates if the latest input sent to the server sent
  using the REPL.")

(defvar-local unrepl-repl-transient-text-gid nil
  "Group ID of the last output displaying as transient text.")

(defvar-local unrepl-repl-transient-text-start-mark nil
  "Marker to the beginning of a transient text, or nil if there's none.")

(defvar-local unrepl-repl-transient-text-end-mark nil
  "Marker to the end of a transient text, or nil if there's none.")

(defvar-local unrepl-repl-history nil
  "A list that holds history entries.
A History Entry is a 3-tuple: the input string, an UNREPL group id, and a
prompt position in buffer.")

(defvar-local unrepl-repl-history-lookup nil
  "A number that represents the current history index being looked upon
  when searching through history.  When nil, search is inactive.")



;; Common Faces
;; -------------------------------------------------------------------

(defface unrepl-font-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-result-prompt-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for the result prompt in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-exception-prompt-face
  '((t (:inherit font-lock-warning-face)))
  "Face for the result prompt in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face for constant things in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for constant things in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-class-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for constant things in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-stdout-face
  '((t (:inherit font-lock-doc-face)))
  "Face for STDOUT output in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-doc-face
  '((t (:inherit font-lock-comment-face)))
  "Face for auto-documentation in the REPL buffer."
  :group 'unrepl-repl)

(defface unrepl-font-tooltip-face
  '((t . (:inherit button)))
  "Face for tooltips."
  :group 'unrepl-repl)



;; Utilities
;; -------------------------------------------------------------------

(defun unrepl-repl--newline-if-needed ()
  "Go to max point in buffer and make sure it is the beginning of a new line."
  (unless (bolp)
    (insert (propertize "%\n" 'font-lock-face 'unrepl-font-constant-face))))

(defun unrepl-repl--newline-and-indent ()
  "Insert a new line, then indent."
  (insert "\n")
  (lisp-indent-line))


(defun unrepl-repl--newline-and-scroll ()
  "Insert a new line and scroll til the end of the buffer."
  (insert "\n")
  (when (eobp)
    (when-let (win (get-buffer-window (current-buffer) t))
      (with-selected-window win
        (set-window-point win (point-max))
        (recenter -1)))))


(defun unrepl-repl--input-str ()
  "Return the current input string in REPL buffer."
  (buffer-substring-no-properties unrepl-repl-input-start-mark (point-max)))


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


(defun unrepl-repl--interactive-input-p (group-id)
  "Return whether GROUP-ID belongs to an interactive input.
Interactive inputs are those that were not sent via REPL, hence do not have
a REPL history entry.

This function should only be called inside REPL buffers."
  (let ((last-history-group-id (-> unrepl-repl-history
                                   (car)
                                   (unrepl-repl--history-entry-group-id)))
        (pending-eval-group-id (unrepl-pending-eval-group-id :client unrepl-conn-id)))
    (and (eql group-id pending-eval-group-id)
         (not (eql group-id last-history-group-id)))))


(defmacro with-current-repl (&rest body)
  "Automatically switch to the inferred REPL buffer and eval BODY.
This macro needs a `conn-id' variable in the scope, otherwise it will throw
an error.
A `project' variable will be added to the local scope."
  `(let* ((project (if (boundp 'conn-id)
                       (unrepl-projects-get conn-id)
                     (unrepl-ensure-connected!)))
          (repl-buffer (unrepl-project-repl-buffer project)))
     (with-current-buffer repl-buffer
       ,@body)))



;; Phantom Input Entries
;; -------------------------------------------------------------------

(defun unrepl-repl-insert-phantom-input (evaluation &optional payload display)
  "Insert a phantom input for EVALUATION.
Adds a History Entry for this new input, as if it were typed by a ghost.

Saves whatever there might be in the current input area, and inserts the
pending evaluation input, with its related PAYLOAD.  Then inserts a fresh
new prompt as if it were created by a `:prompt' message, and restores the
saved input.

DISPLAY is expected to be either 'pop or 'switch.  When non-nil, pops or
switches to the REPL buffer in another window."
  (with-current-repl
   (unrepl-repl--transient-text-remove)
   (let ((current-input (buffer-substring unrepl-repl-input-start-mark (point-max)))
         (evaluation-input (unrepl-pending-eval-entry-input evaluation))
         (payload (or payload (unrepl-pending-eval-entry-payload evaluation)))
         (group-id (unrepl-pending-eval-entry-group-id evaluation))
         (insert-payload-fn (pcase (unrepl-pending-eval-entry-status evaluation)
                              (:eval #'unrepl-repl-insert-evaluation)
                              (:exception #'unrepl-repl-insert-exception)
                              (_ #'unrepl-repl-insert-out))))
     (goto-char unrepl-repl-input-start-mark)
     ;; Insert phantom input and payload
     (delete-region unrepl-repl-input-start-mark (point-max))
     (insert evaluation-input)
     (funcall insert-payload-fn
              payload
              (point)
              (1+ (length unrepl-repl-history))
              (unrepl-project-namespace project))
     ;; Insert it into history
     (unrepl-repl--add-input-to-history (substring evaluation-input 0 -1))
     (unrepl-repl--history-add-gid-to-top-entry group-id)
     ;; Insert new prompt
     (unrepl-repl-prompt unrepl-conn-id)
     ;; Restore current input
     (insert current-input)
     ;; Move to the end of the repl
     (unless (eq (current-buffer) (window-buffer (selected-window)))
       (when display
         (unrepl-repl-display (current-buffer) display))
       (dolist (window (get-buffer-window-list (current-buffer)))
         (set-window-point window (point-max))))
     (goto-char (point-max)))))



;; Transient Text
;; -------------------------------------------------------------------

(defun unrepl-repl--transient-text-insert (group-id text &optional properties)
  "Insert TEXT at the end of the REPL in a 'transient' way.
This TEXT is meant to be shown momentarily and to disappear at some
point (by calling `unrepl-repl--transient-text-remove').

TEXT can be either a string or a AST node.  This function will either
insert it or unparse it accordingly.
GROUP-ID is an integer, and it's meant to identify text to be *appended* to
a transient text block.  If there's already a transient text showing for
GROUP-ID N, then any other subsequent call to
`unrepl-repl--transient-text-insert' with the same GROUP-ID will be
appended to it.  If there's a call with a different GROUP-ID, the text will
be replaced.

PROPERTIES is a plist of text properties."
  (with-current-repl
   (unless (eql unrepl-repl-transient-text-gid group-id)
     (unrepl-repl--transient-text-remove)
     (setq-local unrepl-repl-transient-text-gid group-id))
   (save-excursion
     ;; Find the right place to start inserting.
     (if (marker-position unrepl-repl-transient-text-end-mark)
         (goto-char unrepl-repl-transient-text-end-mark)
       (goto-char (point-max))
       (insert "\n"))
     ;; If start is not set already, set it to current position.
     (unless (marker-position unrepl-repl-transient-text-start-mark)
       (set-marker unrepl-repl-transient-text-start-mark (point)))
     ;; Insert text
     (let ((inhibit-read-only t))
       (unrepl-propertize-region (append properties
                                         '(font-lock-face unrepl-font-doc-face
                                                          read-only t
                                                          intangible t
                                                          field unrepl-repl-transient-field))
         (unrepl-ast-unparse-stdout-string text)))
     ;; And mark the end
     (set-marker unrepl-repl-transient-text-end-mark (point)))))


(defun unrepl-repl--transient-text-remove ()
  "Remove transient text from the REPL buffer."
  (with-current-repl
   (when (and (marker-position unrepl-repl-transient-text-start-mark)
              (marker-position unrepl-repl-transient-text-end-mark))
     (save-excursion
       (let ((inhibit-read-only t))
         (goto-char unrepl-repl-transient-text-start-mark)
         (delete-region unrepl-repl-transient-text-start-mark
                        (point-max))
         (delete-char -1)))
     (set-marker unrepl-repl-transient-text-start-mark nil)
     (set-marker unrepl-repl-transient-text-end-mark nil))))



;; History
;; -------------------------------------------------------------------

(defun unrepl-repl--history-add (entry)
  "Add History ENTRY to `unrepl-repl-history'."
  (push entry unrepl-repl-history))


(defun unrepl-repl--make-history-entry (str)
  "Create a History Entry for STR and return it."
  (list
   (1+ (length unrepl-repl-history))  ;; index
   str                                ;; input
   nil                                ;; pending eval's group-id -- optional
   nil                                ;; next prompt marker
   ))


(defun unrepl-repl--history-get (idx)
  "Return the history entry with reverse index IDX.
Indices, as saved in pending evaluations, start with 1."
  (nth
   (- (length unrepl-repl-history) idx)
   unrepl-repl-history))


(defun unrepl-repl--history-get-by-group-id (group-id)
  "Return a history entry for GROUP-ID, if any."
  (seq-find (lambda (e)
              (eql (unrepl-repl--history-entry-group-id e)
                   group-id))
            unrepl-repl-history))


(defun unrepl-repl--history-entry-idx (entry)
  "Return the index of the given History ENTRY."
  (car entry))


(defun unrepl-repl--history-entry-str (entry)
  "Return the string of the given History ENTRY."
  (cadr entry))


(defun unrepl-repl--history-entry-group-id (entry)
  "Return the UNREPL group id of the given History ENTRY."
  (cl-caddr entry))


(defun unrepl-repl--history-entry-prompt-marker (entry)
  "Return the prompt position of the given History ENTRY."
  (cl-cadddr entry))


(defun unrepl-repl--add-input-to-history (str)
  "Add input STR to history."
  (unless (string= str "")
    (unrepl-repl--history-add (unrepl-repl--make-history-entry str))))


(defun unrepl-repl--history-add-gid-to-top-entry (group-id)
  "Add GROUP-ID to the top entry in history."
  (setf (cl-caddr (car unrepl-repl-history)) group-id))


(defun unrepl-repl--history-set-prompt-marker (history-idx)
  "Set HISTORY-IDX entry `prompt-marker' to current point."
  (setf
   (cl-cadddr
    (nth
     (- (length unrepl-repl-history) history-idx)
     unrepl-repl-history))
   (point-marker)))


(defun unrepl-repl-input-history-assoc (conn-id group-id)
  "Possibly return a list =(:repl-history-idx <some index>)=.
Check CONN-ID REPL to see if `unrepl-repl-inputting' is true.  If so,
return the tuple using the latest history index available.  nil otherwise.

This function includes an important side effect: If REPL is inputting, the
latest history entry will be associated with GROUP-ID."
  (with-current-repl
   (when (and unrepl-repl-inputting
              unrepl-repl-history)
     (unrepl-repl--history-add-gid-to-top-entry group-id)
     `(:repl-history-idx ,(length unrepl-repl-history)))))



;; Interactive
;; -------------------------------------------------------------------

(declare-function unrepl-client-send "unrepl-loop")
(declare-function unrepl-aux-send "unrepl-loop")
(defun unrepl-repl-return (&optional _end-of-input)
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
  (unrepl-repl--transient-text-remove)
  (cond
   ;; (end-of-input
   ;;  (unrepl-client-send start (point)))
   ((unrepl-repl--input-complete-p (point-max))
    (goto-char (point-max))
    (let ((history-idx (1+ (length unrepl-repl-history))))
      (-> (unrepl-repl--input-str)
          (unrepl-client-send (lambda (result-payload)
                                (with-current-repl
                                 (unrepl-repl-insert-evaluation
                                  result-payload nil
                                  (unrepl-project-namespace project)
                                  history-idx))))
          (unrepl-repl--add-input-to-history)))
    (unrepl-repl--newline-and-scroll)
    (setq-local unrepl-repl-inputting t))
   (t
    (unrepl-repl--newline-and-indent)
    (message "[input not complete]"))))


(defun unrepl-request-symbol-doc ()
  "Request `clojure.repl/doc' for `symbol-at-point' through the `:aux' conn.
Point needs to be in the REPL input.
If `:aux' returns a string of data, display it temporarily as stdout.  This
would get automatically removed after input is sent."
  (interactive)
  (when (unrepl-repl--in-input-area-p)
    (when-let (sym (symbol-at-point))
      (with-current-repl
       (let ((doc-action-templ (unrepl-project-actions-get project :unrepl.el/doc)))
         (unrepl--binding-print-limits '((:unrepl.print/string-length . Long/MAX_VALUE)
                                         (:unrepl.print/coll-length . Long/MAX_VALUE)
                                         (:unrepl.print/nesting-depth . Long/MAX_VALUE))
           (unrepl-aux-send (unrepl-command-template doc-action-templ
                                                     `((:unrepl.el/symbol . ,sym)))
                            revert-bindings-back
                            (lambda (stdout-payload group-id)
                              (unrepl-repl--transient-text-insert group-id
                                                                  stdout-payload)))))))))


;; history

(defun unrepl-repl--replace-input (str)
  "Replace the current REPL input with STR."
  (delete-region unrepl-repl-input-start-mark (point-max))
  (goto-char (point-max))
  (insert str))


(defun unrepl-repl--history-search-in-progress-p ()
  "Return t if there's a search in progress, by looking at `last-command'."
  (eq last-command 'unrepl-repl--history-replace))


(defun unrepl-repl--history-replace (direction-fn)
  "Replace current input with the next history input following DIRECTION-FN.
DIRECTION-FN is a function that takes a history index and returns a tuple
with the next history entry's idx and input string to be evaluated."
  (let* (next-in-history
         (history-size (length unrepl-repl-history))
         (lookup (cond
                  ((unrepl-repl--history-search-in-progress-p)
                   unrepl-repl-history-lookup)
                  (t
                   (1+ history-size))))
         (current-input (unrepl-repl--input-str)))
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
       (t (unrepl-repl--replace-input str)))
      (setq-local unrepl-repl-history-lookup idx))
    (setq this-command 'unrepl-repl--history-replace)))


(defun unrepl-repl--history-search-tuple (idx)
  "Helper function that return a tuple of (index, str) for a given IDX.
This function makes sure to not get out of history boundaries."
  (if (<= 1 idx (length unrepl-repl-history))
      (cons idx (unrepl-repl--history-entry-str
                 (unrepl-repl--history-get idx)))
    (cons idx nil)))


(defun unrepl-repl-previous-input ()
  "Replace current input with previous input in history."
  (interactive)
  (unrepl-repl--history-replace
   (lambda (idx)
     (unrepl-repl--history-search-tuple (1- idx)))))


(defun unrepl-repl-next-input ()
  "Replace current input with previous input in history."
  (interactive)
  (unrepl-repl--history-replace
   (lambda (idx)
     (unrepl-repl--history-search-tuple (1+ idx)))))



;; REPL Buffer
;; -------------------------------------------------------------------

(defun unrepl-repl-buffer-name (conn-id)
  "Return a proper name for an UNREPL REPL to CONN-ID."
  (format "UNREPL[%s]" conn-id))

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
        (setq-local unrepl-repl-prompt-start-mark (make-marker))
        (setq-local unrepl-repl-input-start-mark (make-marker))
        (setq-local unrepl-repl-transient-text-start-mark (make-marker))
        (setq-local unrepl-repl-transient-text-end-mark (make-marker))
        (-> ";; Waiting on UNREPL... "
            (propertize 'font-lock-face 'font-lock-comment-face)
            (insert))
        (when unrepl-repl-pop-on-connect
          (pop-to-buffer repl-buffer))
        repl-buffer))))


(defun unrepl-repl-display (conn-id-or-buffer method)
  "Display REPL buffer using METHOD.
METHOD can be either 'switch or 'pop.  Defaults to 'pop.
CONN-ID-OR-BUFFER is either a connection id or a REPL buffer.
This function only if it's not displayed in another window already."
  (let ((repl-buffer (if (bufferp conn-id-or-buffer)
                         conn-id-or-buffer
                       (unrepl-project-repl-buffer
                        (unrepl-projects-get conn-id-or-buffer)))))
    (pcase method
      ('switch (switch-to-buffer-other-window repl-buffer t))
      ('pop (when (not (get-buffer-window repl-buffer))
              (pop-to-buffer repl-buffer nil t)))
      (_ (when unrepl-debug
           (error "Unrecognized `unrepl-repl-display' method: %S" method))))
    repl-buffer))


(defun unrepl-repl-connected (conn-id)
  "Init the REPL buffer for CONN-ID."
  (with-current-repl
   (-> "Connected to %s\n"
       (format conn-id)
       (propertize 'font-lock-face 'font-lock-comment-face)
       (insert))))


(defun unrepl-repl-disconnect (conn-id message)
  "Disconnect REPL buffer for CONN-ID, and display explanation MESSAGE to user."
  (with-current-buffer (unrepl-repl-display conn-id 'pop)
    (goto-char (point-max))
    (insert "\n\n"
            (propertize message 'font-lock-face 'unrepl-font-exception-title-face))
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (point-max) '(read-only t)))))


(defun unrepl-repl--build-prompt (history-id namespace)
  "Build a prompt for HISTORY-ID and NAMESPACE."
  (format "[%s] %s=> " history-id namespace))


(defun unrepl-repl--build-result-indicator (_history-id _namespace)
  "Return an indicator for results of evaluation."
  "> ")


(defun unrepl-repl--build-exception-indicator (_history-id _namespace)
  "Return an indicator for exception."
  "~ ")


(defun unrepl-repl-prompt (conn-id)
  "Insert prompt in CONN-ID'S REPL."
  (with-current-repl
   (goto-char (point-max))
   (unrepl-repl--newline-if-needed)
   ;; Remove 'field property from previous prompt
   (when (marker-position unrepl-repl-prompt-start-mark)
     (let ((inhibit-read-only t))
       (remove-text-properties unrepl-repl-prompt-start-mark
                               unrepl-repl-input-start-mark
                               '(field nil))))
   ;; The new prompt starts here, so we mark it.
   (set-marker unrepl-repl-prompt-start-mark (point))
   ;; Tell previous history entry that the new prompt starts here
   (when unrepl-repl-history
     (unrepl-repl--history-set-prompt-marker (length unrepl-repl-history)))
   ;; Insert prompt
   (unrepl-propertize-region '(font-lock-face unrepl-font-prompt-face
                                              field unrepl-repl-prompt-field
                                              intangible t
                                              read-only t
                                              rear-nonsticky (field font-lock-face intangible read-only))
     (insert
      (unrepl-repl--build-prompt (1+ (length unrepl-repl-history))
                                 (unrepl-project-namespace project))))
   ;; Mark current input start
   (set-marker unrepl-repl-input-start-mark (point))
   ;; Reset the `unrepl-repl-inputting' variable.
   (setq-local unrepl-repl-inputting nil)))


(defun unrepl-repl-insert-evaluation (eval-payload &optional point history-idx namespace)
  "In CONN-ID REPL buffer, unparse EVAL-PAYLOAD AST node at POINT.
If POINT is nil, unparse at the end of the buffer.
HISTORY-IDX and NAMESPACE are used to format/populate an exception prompt
for the REPL.  If any of them is nil, the exception prompt won't be
inserted."
  (goto-char (or point (point-max)))
  (unrepl-repl--newline-if-needed)
  (unrepl-propertize-region '(font-lock-face unrepl-font-result-prompt-face
                                             intangible t
                                             read-only t
                                             rear-nonsticky (font-lock-face intangible read-only))
    (insert
     (unrepl-repl--build-result-indicator history-idx namespace)))
  (unrepl-ast-unparse eval-payload)
  (unrepl-repl--newline-and-scroll))


(defun unrepl-repl-insert-out (stdout-payload &optional point &rest _)
  "Insert stdout STDOUT-PAYLOAD at POINT.
STDOUT-PAYLOAD is either a string or a #unrepl/string tagged literal.
If POINT is nil, prints the payload right before the last prompt"
  (goto-char (or point unrepl-repl-prompt-start-mark))
  (unrepl-ast-unparse-stdout-string stdout-payload)
  (unrepl-repl--newline-if-needed)
  ;; Update marker.
  ;; If point was a marker, move it forward
  ;; If there was no point (*philosophical sadness rushes in*), move
  ;; `unrepl-repl-prompt-start-mark' forward
  ;; Else, point was just a point, nothing to do.
  (when (markerp point)
    (set-marker point (point)))
  (unless point
    (set-marker unrepl-repl-prompt-start-mark (point))))


(defun unrepl-repl-handle-out (conn-id stdout-payload group-id)
  "Figure out where to unparse STDOUT-PAYLOAD for GROUP-ID in CONN-ID REPL.
STDOUT-PAYLOAD is either a string or a #unrepl/string tagged literal.
GROUP-ID is a number as described in UNREPL docs.

If GROUP-ID is not the same as the current pending evaluation's, and
`unrepl-repl-group-stdout' is non nil, this function will try to figure out
a place in the REPL buffer where to print the STDOUT-PAYLOAD, by searching
through history for an entry that shares its same GROUP-ID.  If it can't
find a match, it will print it at the end of the buffer but before the last
prompt."
  (with-current-repl
   (if unrepl-repl-group-stdout
       ;; Find the best place to print the output.
       (let ((pending-eval (unrepl-pending-eval :client conn-id)))
         (if (unrepl-repl--interactive-input-p group-id)  ;; interactive input
             (unrepl-repl-insert-phantom-input pending-eval stdout-payload 'pop)
           (if-let (h-entry (unrepl-repl--history-get-by-group-id group-id))
               (if (eql (unrepl-repl--history-entry-group-id h-entry)
                        (unrepl-pending-eval-entry-group-id pending-eval))  ;; last repl input
                   (unrepl-repl-insert-out stdout-payload (point-max))
                 (save-excursion  ;; old repl input
                   (unrepl-repl-insert-out
                    stdout-payload
                    (unrepl-repl--history-entry-prompt-marker h-entry))))
             (unrepl-repl-insert-out stdout-payload))))
     ;; Just print right before the last prompt
     (unrepl-repl-insert-out stdout-payload))))


(defun unrepl-repl-insert-exception (payload &optional point history-idx namespace)
  "Insert exception PAYLOAD at POINT.
HISTORY-IDX and NAMESPACE are used to format/populate an exception prompt
for the REPL.  If any of them is nil, the exception prompt won't be
inserted."
  (goto-char (or point (point-max)))
  (unrepl-repl--newline-if-needed)
  (when (and history-idx namespace)
    (unrepl-propertize-region '(font-lock-face unrepl-font-exception-prompt-face
                                               intangible t
                                               read-only t
                                               rear-nonsticky (font-lock-face intangible read-only))
      (insert
       (unrepl-repl--build-exception-indicator history-idx namespace))))
  (unrepl-stacktrace-insert payload))


(defun unrepl-repl-handle-exception (conn-id payload group-id)
  "Figure out where to insert exception's PAYLOAD for GROUP-ID in CONN-ID's REPL."
  (with-current-repl
   (let ((namespace (unrepl-project-namespace project)))
     (if (unrepl-repl--interactive-input-p group-id)  ;; Interactive input
         (let ((pending-eval (unrepl-pending-eval :client conn-id)))
           (unrepl-repl-insert-phantom-input pending-eval payload 'switch))
       (if-let (h-entry (unrepl-repl--history-get-by-group-id group-id))  ;; REPL input
           (unrepl-repl-insert-exception
            payload
            (unrepl-repl--history-entry-prompt-marker h-entry)
            (unrepl-repl--history-entry-idx h-entry)
            namespace)
         (error "[%S] Unhandled exception %S" group-id payload))))))



;; UNREPL REPL mode
;; -------------------------------------------------------------------

(defvar unrepl-repl-mode-hook nil
  "Hook for `unrepl-repl-mode'.")

(defvar unrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unrepl-repl-return)
    (define-key map (kbd "C-c C-c") #'unrepl-eval-interrupt)
    (define-key map (kbd "C-c C-d") #'unrepl-request-symbol-doc)
    (define-key map (kbd "C-<up>") #'unrepl-repl-previous-input)
    (define-key map (kbd "C-<down>") #'unrepl-repl-next-input)
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
  (hack-dir-local-variables-non-file-buffer)

  (run-hooks 'unrepl-repl-mode-hook))

(provide 'unrepl-repl)

;;; unrepl-repl.el ends here
