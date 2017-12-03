;;; unrepl.el --- Emacs UNREPL Client -*- lexical-binding: t; -*-
;;
;; Filename: unrepl.el
;; Description:
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov  9 23:26:00 2017 (+0100)
;; Version:
;; Package-Requires: ((emacs "25.1") (clojure-mode "5.6.0") (dash "2.13.0") (treepy "1.0.0"))
;; URL: https://github.com/volrath/unrepl.el
;; Keywords: languages, clojure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Client interface to a Clojure Socket REPL through UNREPL protocol.
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


(require 'dash)

(require 'unrepl-loop)
(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-socket)
(require 'unrepl-util)


(defvar unrepl-connect--history nil
  "Used as a minibuffer history variable for `unrepl-connect' prompt.")


(defun unrepl--assoc-buffer (project &optional new-p)
  "Associate current buffer to PROJECT.
If NEW-P is non-nil, search for all other clojure-mode buffers in PROJECT's
project-dir and associate them to the same project."
  (setq-local unrepl-conn-id (unrepl-project-id project))
  (when new-p
    ;; TODO: ...
    )
  (message "Successfully connected to %s" (unrepl-project-repr project)))


(defun unrepl--init-project-connection (host port project-dir
                                             &optional server-proc)
  "Create a new Project for PROJECT-DIR with a conn-pool connected to HOST:PORT.
Adds the project to `unrepl-projects'.  This function can be called for
Socket REPLs that are already running, or it can be called for a Socket
REPL that has been automatically created by UNREPL.el, in which case
SERVER-PROC should be the process that represents it."
  (let* ((conn-id (unrepl-make-conn-id host port))
         (conn-pool `((:client . ,(unrepl-socket-connect
                                   :client host port
                                   #'unrepl-loop-client-dispatcher))))
         (new-project (unrepl-create-project conn-id
                                             project-dir
                                             conn-pool
                                             server-proc)))
    (unrepl-projects-add new-project)
    new-project))


(defun unrepl--connection-prompt-choices (project-dir)
  "Return a list of available connections.
The returned list will have connections related to PROJECT-DIR appear
first."
  (let* ((projects (unrepl-projects-as-list))
         (for-project-dir (-filter (lambda (p)
                                     (string= (unrepl-project-dir p) project-dir))
                                   projects))
         (for-project-dir (-sort (lambda (p1 p2)
                                   (time-less-p (unrepl-project-created p2)
                                                (unrepl-project-created p1)))
                                 for-project-dir))
         (rest (-filter (lambda (p)
                          (not (string= (unrepl-project-dir p) project-dir)))
                        projects)))
    (mapcar #'unrepl-project-repr (append for-project-dir rest))))


(defun unrepl--connection-prompt (&optional project-dir)
  "Prompt the user for a Clojure Socket REPL coordinates.
The user is prompted based on available connections found in
`unrepl-projects', with the possibility to specify a host:port for a
project that might not be in the existing choices.

This function returns a list with an UNREPL project and a 'new project'
boolean flag (nil or t-ish) indicating if the selected project comes from
an existing selection (nil), or if it was brand new (t-ish).

If PROJECT-DIR is non-nil and a string, `unrepl-projects' connections
already created for PROJECT-DIR will appear first in the list."
  (let* ((connection-choices (unrepl--connection-prompt-choices project-dir))
         (prompt (if connection-choices
                     "Select project (or type <host>:<port> for a new connection): "
                   "Type <host>:<port> coordinates of your running Socket REPL: "))
         (selection (completing-read prompt
                                     connection-choices
                                     nil nil nil
                                     'unrepl-connect--history
                                     (car connection-choices)))
         (conn-id-regexp "[^\\:]+:[0-9]+")
         (named-project-repr-regexp (format "[a-zA-Z0-9].* \\[\\(%s\\)\\]"
                                            conn-id-regexp))
         (conn-id (cond ((string-match named-project-repr-regexp selection)
                         (intern (match-string 1 selection)))
                        ((string-match-p conn-id-regexp selection)
                         (intern selection))
                        (t (user-error "%S does not look like a valid <host>:<port>" selection)))))
    (if-let (project (unrepl-projects-get conn-id))
        (list (unrepl-projects-get conn-id) nil)
      (let* ((host-port (unrepl-conn-host-port conn-id))
             (host (car host-port))
             (port (cdr host-port)))
        (list (unrepl--init-project-connection host port project-dir)
              'new)))))


(defun unrepl--connect-to (host port &optional buffer)
  "Create a project for a connection to a Socket REPL running on HOST:PORT.
If BUFFER is non-nil, associates it to the new project/connection.

This function is meant to be a debugging helper, and not to be used to
connect to socket REPLs.  Refer to `unrepl-connect' and
`unrepl-connect-to'."
  (let ((project (unrepl--init-project-connection host port (unrepl-clojure-dir))))
    (when buffer
      (with-current-buffer buffer
        (unrepl--assoc-buffer project 'new)))))



;; Main interactive functions
;; -------------------------------------------------------------------

;;;###autoload
(defun unrepl-connect (&optional just-ask)
  "Ask user for a Clojure Socket REPL coordinates and connect to it.
Use a list of suitable connections to aid the user into selecting and
connecting to an existing one.  Before connecting, this command will try to
infer the actual Clojure project directory from the buffer's file name, if
any.  In case it's possible to infer a project directory, this command
automatically connects all other buffers for the same project to the new
selected connection, even if they were already connected to a different
Socket REPL.

If the current buffer is already connected, this command will only warn the
user that a connection already exists, and then will exit.  When called
with JUST-ASK, this command will ask the user for a new connection either
way."
  (interactive "P")
  (when (or (derived-mode-p 'clojure-mode)
            (y-or-n-p "This is not a Clojure buffer, are you sure you want to connect it? "))
    (let* ((project-dir (unrepl-clojure-dir))
           (project (unrepl-projects-get-by-dir project-dir)))
      (cond (just-ask (apply #'unrepl--assoc-buffer (unrepl--connection-prompt project-dir)))
            (unrepl-conn-id (-> (concat "You are already connected to %s.  "
                                        "If you really want to change this connection, type "
                                        "`C-u \\[unrepl-connect]'")
                                (format unrepl-conn-id)
                                (substitute-command-keys)
                                (message)
                                (ding)))
            (project (unrepl--assoc-buffer project))
            (project-dir (unrepl-socket-create-server
                          (lambda (process host port)
                            (unrepl--assoc-buffer
                             (unrepl--init-project-connection host port project-dir process)
                             'new))))
            (t (apply #'unrepl--assoc-buffer (unrepl--connection-prompt)))))))


;;;###autoload
(defun unrepl-connect-to ()
  "Same as `unrepl-connect' but force a prompt so the user can decide where to connect."
  (interactive)
  (unrepl-connect 'ask!))


(provide 'unrepl)

;;; unrepl.el ends here
