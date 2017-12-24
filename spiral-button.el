;;; spiral-button.el --- BUTTON related helpers -*- lexical-binding: t; -*-
;;
;; Filename: spiral-button.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Mon Nov 20 12:33:28 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Common functions for handling buttons.
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

(defvar spiral-button-elision-label "..."
  "Label used to represent elisions.")


(defun spiral-button-insert (label action-fn &rest extra-props)
  "Insert a button with LABEL that execute ACTION-FN when pressed.
If ACTION is a function, it will be executed when the button is pressed,
EVAL-CALLBACK and STDOUT-CALLBACK params will be ignored.
EXTRA-PROPS are button properties to add to the button."
  (insert " ")
  (apply #'insert-text-button
         label
         'follow-link t
         'action action-fn
         extra-props))


(defun spiral-button-throwaway-insert (label action-fn
                                             &optional kill-from kill-to
                                             &rest extra-props)
  "Same as `spiral-button-insert' but only clickable once.
This button will delete itself after the user interacts with it the first
time.
The ACTION-FN will be wrapped so that the button deletes the region marked
from KILL-FROM to KILL-TO.
KILL-FROM default value is =(point)=.  KILL-TO default value is the end of
the new created button.
ACTION-FN can safely assume that the cursor will be at KILL-FROM, and
it is responsible to decide where to leave the cursor when finished.
LABEL and EXTRA-PROPS are the same as in `spiral-button-insert'."
  (let* ((kill-from-marker (make-marker))
         (kill-to-marker (make-marker))
         (button-action (lambda (button)
                          (with-current-buffer (marker-buffer kill-from-marker)
                            ;; Kill the button region
                            (goto-char kill-from-marker)
                            (let ((inhibit-read-only t))
                              (delete-region kill-from-marker kill-to-marker))
                            ;; Run the button action
                            (funcall action-fn button)))))
    (set-marker kill-from-marker (or kill-from (point)))
    (apply #'spiral-button-insert label button-action nil nil extra-props)
    (set-marker kill-to-marker (or kill-to (point)))))


(declare-function spiral-aux-send "spiral-loop")
(defun spiral-button-aux-action-throwaway-insert (label action eval-callback
                                                        &optional
                                                        stdout-callback
                                                        kill-from kill-to
                                                        extra-props)
  "Create a throwaway button that send ACTION through the `:aux' connection.
ACTION is expected to be an input string to be sent through `:aux'.
EVAL-CALLBACK and STDOUT-CALLBACK are attached to this new pending
evaluation.
KILL-FROM and KILL-TO are the same as in `spiral-button-throwaway-insert'.
LABEL and EXTRA-PROPS are the same as in `spiral-button-insert'."
  (let* ((buf (current-buffer))
         (button-action (lambda (_button)
                          (spiral-aux-send action
                                           (lambda (eval-payload)
                                             (with-current-buffer buf
                                               (funcall eval-callback eval-payload)))
                                           (when stdout-callback
                                             (lambda (out-payload)
                                               (with-current-buffer buf
                                                 (funcall stdout-callback out-payload))))))))
    (apply #'spiral-button-throwaway-insert label button-action kill-from kill-to extra-props)))


(provide 'spiral-button)

;;; spiral-button.el ends here
