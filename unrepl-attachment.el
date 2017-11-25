;;; unrepl-attachment.el --- attachment related helpers -*- lexical-binding: t; -*-
;; 
;; Filename: unrepl-attachment.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Mon Nov 20 12:33:28 2017 (+0100)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; Handle UNREPL attachments gracefully.
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
(require 'map)

(require 'unrepl-button)
(require 'unrepl-loop)
(require 'unrepl-mode)


(defun unrepl-attachment--generate-button-action (load-action-str eval-callback
                                                                  delete-from delete-to
                                                                  &optional stdout-callback)
  "Generate a callback function for a button action.
This new function will:
1. Make sure the `:aux' process buffer is not font-locking
2. Temporarily increase the print limits on `:aux'
3. Send LOAD-ACTION-STR through `:aux' with EVAL-CALLBACK and
STDOUT-CALLBACK.
4. Make sure to revert print-limits bindings and delete region from
DELETE-FROM to DELETE-TO"
  (lambda (_button)
    ;; 1.
    (with-current-project
     (with-current-buffer (process-buffer (unrepl-project-conn-pool-get-process project :aux))
       (font-lock-mode -1)))
    ;; 2.
    (unrepl--binding-print-limits '((:unrepl.print/string-length . Long/MAX_VALUE)
                                    (:unrepl.print/coll-length . Long/MAX_VALUE)
                                    (:unrepl.print/nesting-depth . Long/MAX_VALUE))
      ;; 3.
      (unrepl-aux-send (format "(%s)" load-action-str)   ;; hack, read below
                       (lambda (eval-payload)
                         (with-current-buffer (marker-buffer delete-from)
                           ;; 4.
                           (goto-char delete-from)
                           (delete-region delete-from delete-to)
                           (funcall revert-bindings-back)
                           (funcall eval-callback eval-payload)))
                       stdout-callback))))
;; In this line we're enclosing the command into parens (see `format') because
;; UNREPL mime tagged literal's `:get' action returns a function, so we need to
;; call it again.  TODO: make sure this is not an UNREPL bug.


(defun unrepl-attachment--handle-image (eval-payload)
  "Load a base64 encoded image from EVAL-PAYLOAD and display it."
  (let* ((image-data (-> eval-payload
                         (parseclj-ast-value)
                         (base64-decode-string)
                         (string-as-unibyte)))
         (image (create-image image-data 'png t)))
    (condition-case nil
        (save-excursion
          (insert "\n\n  ")
          (insert-image image "image-data")
          (insert "\n"))
      (error (ding (message "Not a valid image"))))
    (goto-char (point-max))))


(defun unrepl-attachment-find-handler (content-type)
  "Find a handler function for CONTENT-TYPE."
  (cond ((string-prefix-p "image" content-type)
         `((:label . "View")
           (:eval-callback . ,#'unrepl-attachment--handle-image)))))


(defun unrepl-attachment-insert-button (handler load-action-str)
  "Insert a button to handle a certain attachment.
This is a special type of one-off-button.
HANDLER is an attachment handler as provided by
`unrepl-attachment-find-handler'.
LOAD-ACTION-STR is a UNREPL action string."
  (let* ((delete-from-marker (point-marker))
         (delete-to-marker (make-marker))
         (button-action (unrepl-attachment--generate-button-action load-action-str
                                                                   (map-elt handler :eval-callback)
                                                                   delete-from-marker delete-to-marker)))
    (unrepl-button-insert (format "[%s]" (map-elt handler :label))
                          button-action
                          nil nil
                          'help-echo "mouse-1, RET: Load Image")
    (set-marker delete-to-marker (point))))


(provide 'unrepl-attachment)

;;; unrepl-attachment.el ends here
