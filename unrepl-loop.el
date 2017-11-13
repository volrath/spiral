;;; unrepl-loop.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-loop.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Nov 11 20:07:16 2017 (+0100)
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

(require 'parseedn)

(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-repl)


(defvar unrepl-loop--global-edn-tag-readers
  `((unrepl/ns . ,#'symbol-name))
  "Global EDN tag readers to be used on every incoming client message.")

(defvar-local unrepl-loop-greeted-p nil
  "Predicate that defines if the client for the current buffer has been greeted already.")


(defun unrepl-loop-send (str)
  "Send string STR to the UNREPL connection represented by `unrepl-conn-id'."
  (let* ((project (unrepl-projects-get unrepl-conn-id))
         (client-proc (unrepl-project-client-proc project)))
    (process-send-string client-proc str)))


(defun unrepl-loop--client-dispatcher (msg conn-id)
  "Dispatch MSG to an `unrepl-loop--' client message handler.
CONN-ID is provided to client message handlers so they know which
project/repl to modify."
  (let ((tag (elt msg 0))
        (payload (elt msg 1)))
    (pcase tag
      (:unrepl/hello (unrepl-loop--hello conn-id payload))
      (:prompt (unrepl-loop--prompt conn-id payload))
      (_ (error (format "Unrecognized message: %S" tag))))))


(declare-function unrepl-process-conn-id "unrepl")
(defun unrepl-loop-handle-client-message (process output)
  "Decode EDN messages from PROCESS contained in OUTPUT and dispatch accordingly."
  (with-current-buffer (process-buffer process)
    (unless unrepl-loop-greeted-p
      (when-let (hello-match (string-match-p (regexp-quote "[:unrepl/hello")
                                             output))
        (setq output (substring output hello-match))
        (setq-local unrepl-loop-greeted-p t)
        (message "UNREPL says hi!")))
    (when unrepl-loop-greeted-p
      (goto-char (point-max))
      (save-excursion (insert output))

      ;; There can be several EDN messages in OUTPUT, so we iterate over them.
      (mapcar (lambda (edn-msg)
                (unrepl-loop--client-dispatcher edn-msg
                                                (unrepl-process-conn-id process)))
              (parseedn-read unrepl-loop--global-edn-tag-readers)))))


;; UNREPL Client Message Processing
;; -------------------------------------------------------------------

(defun unrepl-loop--hello (conn-id payload)
  "Handle a `:unrepl/hello' message transmitted through CONN-ID.
It processes the PAYLOAD to init the corresponding REPL and subsequent
evaluation of inputs."
  (let ((repl-buffer (unrepl-project-repl-buffer (unrepl-projects-get conn-id))))
    (with-current-buffer repl-buffer
      (unrepl-repl-insert (format "Connected to %S\n" conn-id))))
  (unrepl-project-set-in conn-id
                         :actions
                         (map-elt payload :actions)))


(defun unrepl-loop--prompt (conn-id payload)
  "Handle a `:prompt' message transmitted through CONN-ID.
PAYLOAD is the standard `:prompt' message data."
  (let ((repl-buffer (unrepl-project-repl-buffer (unrepl-projects-get conn-id))))
    (with-current-buffer repl-buffer
      (unrepl-repl-insert (format "%s => " (map-elt payload 'clojure.core/*ns*))))))


;; Side loader
;; -------------------------------------------------------------------

(defun unrepl-loop-handle-side-loader-message (process output)
  "Decode EDN messages from PROCESS contained in OUTPUT and act accordingly."
  (with-current-buffer (process-buffer process)
    (unless (string-match-p (regexp-quote "user=>") output)
      (goto-char (point-max))
      (save-excursion (insert output)))))


(provide 'unrepl-loop)

;;; unrepl-loop.el ends here
