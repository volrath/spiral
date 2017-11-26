;;; unrepl-util.el --- Common utility functions -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-util.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov 16 01:09:49 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Common utility functions
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

(require 'ansi-color)
(require 'clojure-mode)
(require 'dash)
(require 'find-func)
(require 'parseclj)
(require 'subr-x)
(require 'treepy)


(defcustom unrepl-debug t
  "Annoyingly raise errors."
  :type 'boolean
  :group 'unrepl)

(defcustom unrepl-eval-result-prefix "=> "
  "Prefix displayed before a evaluation result value."
  :type 'string
  :group 'unrepl)

(defcustom unrepl-handle-rich-media t
  "Handle attachments and mime tags."
  :type 'boolean
  :group 'unrepl)

(defvar unrepl--mode-buffers nil
  "Alist with (mode . buffer) pairs.")


;; Random utilities
;; -------------------------------------------------------------------

(defun unrepl-conn-host-port (conn-id)
  "Return a tuple of host<string>, port<integer> from CONN-ID."
  (let* ((s-host-port (split-string (symbol-name conn-id) ":"))
         (host (car s-host-port))
         (port (string-to-number (cadr s-host-port))))
    (cons host port)))


(defun unrepl-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead.
If BOUNDS is 'marker-bounds, this list will have markers instead of
numbered positions.

BORROWED FROM CIDER."
  (let* ((bound-points (save-excursion
                         (clojure-backward-logical-sexp 1)
                         (list (point)
                               (progn (clojure-forward-logical-sexp 1)
                                      (skip-chars-forward "[:blank:]")
                                      (when (looking-at-p "\n") (forward-char 1))
                                      (point)))))
         (bound-points (if (eql bounds 'marker-bounds)
                           (mapcar (lambda (bp)
                                     (let ((m (make-marker)))
                                       (set-marker m bp)
                                       m))
                                   bound-points)
                         bounds)))
    (apply (if bounds #'list #'buffer-substring-no-properties)
           bound-points)))


(defun unrepl--make-buffer-for-mode (mode)
  "Return a temp buffer using MODE as `major-mode'.
Buffers are stored in `unrepl--mode-buffers', and are reused whenever
possible.  `unrepl--mode-buffers' is updated each time this function runs,
by eliminating killed buffers from it.

BORROWED FROM CIDER."
  (setq unrepl--mode-buffers (map-filter (lambda (_mode buffer) (buffer-live-p buffer))
                                         unrepl--mode-buffers))
  (or (map-elt unrepl--mode-buffers mode)
      (let ((buffer (generate-new-buffer (format " *unrepl-temp %s*" mode))))
        (map-put unrepl--mode-buffers mode buffer)
        (with-current-buffer buffer
          ;; suppress major mode hooks as we care only about their font-locking
          ;; otherwise modes like whitespace-mode and paredit might interfere
          (setq-local delay-mode-hooks t)
          (setq delayed-mode-hooks nil)
          (funcall mode))
        buffer)))


(defun unrepl-font-lock-as (mode string)
  "Font-lock STRING as in MODE.
BORROWED FROM CIDER."
  (let ((string (if (string-match "^\\[" string)
                    (substring-no-properties (ansi-color-apply string))
                  string)))
    (with-current-buffer (unrepl--make-buffer-for-mode mode)
      (erase-buffer)
      (insert string)
      (font-lock-fontify-region (point-min) (point-max))
      (buffer-string))))


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


;; Debugging
;; -------------------------------------------------------------------

(defmacro comment (&rest _body)
  "A wannabe 'clojure-like' comment macro."
  nil)


(defun unrepl-make-blob ()
  "Generate a new UNREPL blob, an place it in `unrepl--find-default-blob'."
  (interactive)
  (let* ((unrepl-file (cdr (find-function-library 'unrepl-connect)))
         (unrepl-dir (file-name-directory unrepl-file))
         (default-directory (expand-file-name "unrepl" unrepl-dir)))
    (message "... generating new blob.clj")
    (shell-command-to-string "lein unrepl-make-blob ../blob.clj ../session-actions.edn")
    (message "done.")))


(comment  ;; For debugging purposes.
 (defun unrepl-debug-retry ()
   "Reload everything and connect again."
   (interactive)
   (let ((unrepl-dir (file-name-directory buffer-file-name)))
     (add-to-list 'load-path unrepl-dir)
     (add-to-list 'load-path (expand-file-name "parseclj" unrepl-dir))
     (mapc (lambda (filename)
             (load (expand-file-name filename unrepl-dir)))
           (directory-files default-directory nil "^unrepl.*\\.el$")))
   (unrepl-project-quit '127.0.0.1:5555)
   (unrepl-connect-to "localhost" 5555))

 (global-set-key (kbd "C-c C-u r") #'unrepl-debug-retry)
 (global-set-key (kbd "C-c C-u m") #'unrepl-make-blob)
 )


(provide 'unrepl-util)

;;; unrepl-util.el ends here
