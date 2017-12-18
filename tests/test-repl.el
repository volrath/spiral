;;; test-repl.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-connect.el
;; Author: Daniel Barreto
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Dec 17 23:42:58 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Test REPL stuff
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

(add-to-list 'load-path (expand-file-name "parseclj/"))
;; HACK: this is a temporary meassure while MELPA/package-build.el fixes:
;;       https://github.com/melpa/package-build/commit/063e64ad55ac07a348af0373605fd90348a8bc11#r26028026

(require 'buttercup)

(require 'unrepl)

(describe "REPL"
  (before-all
    (unrepl--connect-to "localhost" 5555)

    (with-current-buffer "UNREPL[localhost:5555]"  ;; wait for it to start.
      (while (null (marker-buffer unrepl-repl-prompt-start-mark))
        (accept-process-output nil 0.1))))

  (after-all
    (unrepl-quit 'do-it 'localhost:5555))

  (it "evaluates inputs"
    (with-current-buffer "UNREPL[localhost:5555]"
      (goto-char (point-max))
      (insert "1")
      (unrepl-repl-return)
      (expect (length unrepl-repl-history) :to-equal 1))))

;;; test-repl.el ends here
