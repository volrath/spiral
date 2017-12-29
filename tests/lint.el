;;; lint.el --- Make sure everything is tidy -*- lexical-binding: t; -*-
;;
;; Filename: test-repl.el
;; Author: Daniel Barreto
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Dec 17 23:42:58 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Lint Emacs Lisp files.
;; Borrowed from Nicolas Petton's `indium'.
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

(require 'checkdoc)
(require 'seq)

(let ((checkdoc-arguments-in-order-flag nil)
      (checkdoc-symbol-words '("UNREPL" "unrepl" "stdout" "print-level" "print-length"))
      (files (seq-filter (lambda (file)
                           (and (string= (file-name-extension file) "el")
                                (string-prefix-p "spiral" file)))
                         (directory-files "."))))
  (seq-doseq (file files)
    (with-current-buffer (find-file file)
      (message "Linting %s..." file)
      (checkdoc-current-buffer))))

(provide 'lint)

;;; lint.el ends here
