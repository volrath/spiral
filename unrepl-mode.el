;;; unrepl-mode.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-mode.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Nov 12 12:25:44 2017 (+0100)
;; Version:
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
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

(defvar-local unrepl-conn-id nil
  "Port number used when creating a new Socket REPL.")

(defvar-local unrepl-repl-buffer nil
  "REPL buffer for current connection.")

(defvar-local unrepl-current-namespace nil
  "Holds the current namespace in UNREPL, as provided by the `:prompt' message.")

(defvar-local unrepl-repl-ngid nil
  "Next UNREPL group-id to be processed, starting with 1.")

(defvar-local unrepl-repl-pending-evals nil
  "Associative list storing all pending evaluations.
Keys are UNREPL group ids and values would be keywords representing the
last received message for said group.")


(define-minor-mode unrepl-mode
  "Minor mode for UNREPL."
  :init-value nil
  :lighter "UNREPL"
  :group 'unrepl
  (setq-local unrepl-current-namespace nil)
  (setq-local unrepl-repl-ngid 1)
  (setq-local unrepl-repl-pending-evals nil))


(provide 'unrepl-mode)

;;; unrepl-mode.el ends here
