;;; spiral-attachment.el --- attachment related helpers -*- lexical-binding: t; -*-
;;
;; Filename: spiral-attachment.el
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
(require 'map)
(require 'parseclj)

(require 'spiral-button)


(declare-function spiral-aux-send "spiral-loop")
(defun spiral-attachment--generate-button-action (load-action-str eval-callback
                                                                  delete-from delete-to
                                                                  &optional stdout-callback)
  "Generate a callback function for a button action.
This new function will:
1. Make sure the `:aux' process buffer is not font-locking
2. Send LOAD-ACTION-STR through `:aux' with EVAL-CALLBACK and
STDOUT-CALLBACK.
3. Make sure to revert print-limits bindings and delete region from
DELETE-FROM to DELETE-TO"
  (lambda (_button)
    ;; 1.
    ;; (with-current-project
    ;;  (with-current-buffer (process-buffer (spiral-project-conn-pool-get-process project :aux))
    ;;    (font-lock-mode -1)))
    ;; 2.
    (spiral-aux-send load-action-str
                     (lambda (eval-payload)
                       (with-current-buffer (marker-buffer delete-from)
                         ;; 3.
                         (goto-char delete-from)
                         (delete-region delete-from delete-to)
                         (funcall eval-callback eval-payload)))
                     stdout-callback)))


(declare-function spiral-ast-tag-child "spiral-ast")
(declare-function spiral-repl-newline-and-scroll "spiral-repl")
(defun spiral-attachment--handle-image (eval-payload)
  "Load a base64 encoded image from EVAL-PAYLOAD and display it."
  (let* ((image-data (thread-first eval-payload
                       (spiral-ast-tag-child)
                       (parseclj-ast-value)
                       (base64-decode-string)
                       (string-as-unibyte)))
         (image (create-image image-data 'png t)))
    (condition-case nil
        (progn
          (spiral-repl-newline-and-scroll 2)
          (insert-image image "image-data")
          (spiral-repl-newline-and-scroll))
      (error (ding (message "Not a valid image"))))
    (goto-char (point-max))
    (recenter -1)))


(defun spiral-attachment-find-handler (content-type)
  "Find a handler function for CONTENT-TYPE."
  (cond ((string-prefix-p "image" content-type)
         `((:label . "View")
           (:eval-callback . ,#'spiral-attachment--handle-image)))))


(defun spiral-attachment-insert-button (handler load-action-str)
  "Insert a button to handle a certain attachment.
This is a special type of one-off-button.
HANDLER is an attachment handler as provided by
`spiral-attachment-find-handler'.
LOAD-ACTION-STR is a UNREPL action string."
  (let* ((delete-from-marker (point-marker))
         (delete-to-marker (make-marker))
         (button-action (spiral-attachment--generate-button-action load-action-str
                                                                   (map-elt handler :eval-callback)
                                                                   delete-from-marker delete-to-marker)))
    (spiral-button-insert (format "[%s]" (map-elt handler :label))
                          button-action
                          nil nil
                          'help-echo "mouse-1, RET: Load Image")
    (set-marker delete-to-marker (point))))


(provide 'spiral-attachment)

;;; spiral-attachment.el ends here
