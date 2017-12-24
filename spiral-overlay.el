;;; spiral-overlay.el --- General Overlays Functionality -*- lexical-binding: t; -*-
;;
;; Filename: spiral-overlay.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Mon Nov 20 12:33:28 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Create and remove overlays with ease.
;;
;; Borrowed from CIDER.
;; Original author: Artur Malabarba <bruce.connor.am@gmail.com>
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

(require 'subr-x)

(require 'spiral-util)


(defcustom spiral-overlays-use-font-lock t
  "Whether or not to font-lock results when displayed in overlays."
  :type 'boolean
  :group 'spiral)


(defcustom spiral-overlay-eval-result-duration 'command
  "Duration, in seconds, of eval-result overlays.
If nil, overlays last indefinitely.
If the symbol `command', they're erased after the next command.
Also see `spiral-eval-result-dispaly'."
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :group 'spiral)


(defface spiral-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `spiral-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'spiral)


(defun spiral-overlay--should-truncate-string-p (str)
  "Return non-nil if STR should be truncated.
Base decision on string length, and a guess over STR properties: it checks
for a 'spiral-elision category property or whether the string starts like
an inspectable object."
  ;; TODO: figure out if UNREPL protocol can be improved so that we don't have
  ;; to guess.
  (or (> (string-width str) (window-width))
      (text-property-any 0 (length str) 'category 'spiral-elision str)
      (string-prefix-p (format " %s#image" spiral-eval-result-prefix) str)))


(defun spiral--delete-overlay (o &rest _)
  "Safely delete overlay O.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay o)))


(defun spiral--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'spiral--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))


(defun spiral--remove-result-overlay-after-command ()
  "Locally add `spiral--remove-result-overlay' to `post-command-hook'.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'spiral--remove-result-overlay-after-command 'local)
  (add-hook 'post-command-hook #'spiral--remove-result-overlay nil 'local))


(defun spiral--make-overlay (start end type &rest props)
  "Place an overlay between START and END and return it.
TYPE is a symbol put on the overlay's category property, to be able to
easily categorize them and remove them if needed.
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay start end (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'spiral--delete-overlay (overlay-get o 'modification-hooks))))


(defun spiral--make-result-overlay (value &optional where duration type
                                          &rest props)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay.  The overlay itself is placed from
beginning to end of current line.
Return nil if the overlay was not placed or if it might not be visible, and
return the overlay otherwise.

This function takes some optional arguments:

  If WHERE is a number or a marker, apply the overlay over
  the entire line at that place (defaulting to `point').  If
  it is a list, the car and cadr determine the start and
  end of the overlay.
  DURATION takes the same possible values as the
  `spiral-eval-overlay-result-duration' variable.
  TYPE is passed to `spiral--make-overlay' (defaults to `result').

All arguments beyond these (PROPS) are properties to be used on the
overlay."
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (cond ((number-or-marker-p where)
               (goto-char where))
              ((and (consp where)
                    (number-or-marker-p (car where)))
               (goto-char (car where))))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((type (or type 'result))
               (beg (if (listp where)
                        (car where)
                      (save-excursion
                        (clojure-backward-logical-sexp 1)
                        (point))))
               (end (if (listp where)
                        (cadr where)
                      (line-end-position)))
               (display-string (format " %s%s "
                                       spiral-eval-result-prefix
                                       value))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if spiral-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face 'spiral-result-overlay-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (spiral-overlay--should-truncate-string-p display-string)
            (let ((sub-display-string (substring display-string 0 (min (length display-string)
                                                                       (window-width)))))
              (setq display-string
                    (concat sub-display-string
                            (substitute-command-keys
                             "\n... Result truncated. Type `\\[spiral-inspect-last-eval]' to inspect it.")))))
          ;; Create the result overlay.
          (setq o (apply #'spiral--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase (or duration spiral-overlay-eval-result-duration)
            ((pred numberp) (run-at-time duration nil #'spiral--delete-overlay o))
            ('command
             ;; If inside a command-loop, tell `spiral--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook 'post-command-hook
                           #'spiral--remove-result-overlay-after-command
                           nil 'local)
               (spiral--remove-result-overlay-after-command))))
          (when-let (win (get-buffer-window buffer))
            ;; Left edge is visible.
            (when (and (<= (window-start win) (point))
                       ;; In 24.3 `<=' is still a binary predicate.
                       (<= (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))



(provide 'spiral-overlay)

;;; spiral-overlay.el ends here
