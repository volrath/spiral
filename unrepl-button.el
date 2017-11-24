;;; unrepl-button.el --- BUTTON related helpers -*- lexical-binding: t; -*-
;; 
;; Filename: unrepl-button.el
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

(defvar unrepl-button-elision-label "..."
  "Label used to represent elisions.")


(declare-function unrepl-aux-send "unrepl-loop")
(defun unrepl-button-insert (label action eval-callback
                                   &optional stdout-callback
                                   &rest extra-props)
  "Insert a button with LABEL, that sends ACTION through the `:aux' connection.
A callback function will be created for the ACTION's evaluation, and it
will internally call EVAL-CALLBACK with the evaluation payload.
If STDOUT-CALLBACK is non-nil, another callback function will be created
for the ACTION's stdout, and it will internally call STDOUT-CALLBACK with
any output payload given.
EXTRA-PROPS are button properties to add to the button."
  (let ((button-action (lambda (_button)
                         (unrepl-aux-send action eval-callback stdout-callback))))
    (insert " ")
    (apply #'insert-text-button
           label
           'follow-link t
           'action button-action
           'help-echo "mouse-1, RET: "
           extra-props)))


(defun unrepl-button-insert-one-off (label action eval-callback
                                           &optional stdout-callback kill-from kill-to
                                           &rest extra-props)
  "Same as `unrepl-button-insert' but only clickable once.
This button will delete itself after the user interacts with it the first
time.
LABEL, ACTION, EVAL-CALLBACK, STDOUT-CALLBACK, and EXTRA-PROPS are the same
as explained in `unrepl-button-insert'.
The evaluation callback for this button will delete the region where the
button is, which should be defined by KILL-FROM and KILL-TO.
KILL-FROM default value is =(point)=.  KILL-TO default value is the end of
the new created button.
EVAL-CALLBACK can safely assume that the cursor will be at KILL-FROM, and
it is responsible to decide where to leave the cursor when finished."
  (let* ((kill-from-marker (make-marker))
         (kill-to-marker (make-marker))
         (eval-callback (lambda (eval-payload)
                          (with-current-buffer (marker-buffer kill-from-marker)
                            ;; Kill the button region
                            (goto-char kill-from-marker)
                            (delete-region kill-from-marker kill-to-marker)
                            ;; Run the actual callback
                            (funcall eval-callback eval-payload)))))
    (set-marker kill-from-marker (or kill-from (point)))
    (apply #'unrepl-button-insert label action eval-callback stdout-callback extra-props)
    (set-marker kill-to-marker (or kill-to (point)))))


(provide 'unrepl-button)

;;; unrepl-button.el ends here
