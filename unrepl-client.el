;;; unrepl-client.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-client.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Fri Nov 10 18:05:43 2017 (+0100)
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


(defvar-local unrepl-client-greeted-p nil
  "Predicate that defines if the client for the current buffer has been greeted already.")


(defun unrepl-handle-client-message (process output)
  "Decode EDN messages from PROCESS contained in OUTPUT and dispatch accordingly."
  (with-current-buffer (process-buffer process)
    (when (and (not unrepl-client-greeted-p)
               (string-match-p (regexp-quote "[:unrepl/hello")
                               output))
      (setq-local unrepl-client-greeted-p t)
      (message "UNREPL says hi!"))
    (when unrepl-client-greeted-p
      (goto-char (point-max))
      (save-excursion (insert output))

      ;; Read EDN and dispatch accordingly.
      )))


(defun unrepl-handle-side-loader-message (process output)
  "Decode EDN messages from PROCESS contained in OUTPUT and act accordingly."
  (with-current-buffer (process-buffer process)
    (unless (string-match-p (regexp-quote "user=>") output)
      (goto-char (point-max))
      (save-excursion (insert output)))))

(provide 'unrepl-client)

;;; unrepl-client.el ends here
