;;; spiral.el --- Clojure IDE based on UNREPL -*- lexical-binding: t; -*-
;;
;; Filename: spiral.el
;; Description:
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov  9 23:26:00 2017 (+0100)
;; Version: 0.2.0-snapshot
;; Package-Requires: ((emacs "25.1") (a "0.1.0alpha4") (avy "0.4.0") (clojure-mode "5.6.0") (highlight "0") (treepy "1.0.0"))
;; URL: https://github.com/Unrepl/spiral
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

(require 'spiral-loop)
(require 'spiral-mode)
(require 'spiral-project)
(require 'spiral-socket)
(require 'spiral-util)


(defvar spiral-connect--history nil
  "Used as a minibuffer history variable for `spiral-connect' prompt.")


(defun spiral--assoc-buffer (project &optional new-p buffer)
  "Associate current buffer to PROJECT.
If NEW-P is non-nil, search for all other clojure-mode buffers in PROJECT's
project-dir and associate them to the same project.
If BUFFER is non-nil, associates BUFFER instead of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'clojure-mode)
      (let ((conn-id (spiral-project-id project)))
        (spiral-mode-turn-on conn-id)
        (when (and spiral-auto-mode new-p)
          (spiral-mode-enable-auto)
          (dolist (buf (spiral-project-buffers project))
            (spiral-mode-turn-on conn-id buf))))
      (message "Successfully connected to %s" (spiral-project-repr project)))))


(defun spiral--init-project-connection (host port project-dir
                                             &optional server-proc)
  "Create a new Project for PROJECT-DIR with a conn-pool connected to HOST:PORT.
Adds the project to `spiral-projects'.  This function can be called for
Socket REPLs that are already running, or it can be called for a Socket
REPL that has been automatically created by SPIRAL, in which case
SERVER-PROC should be the process that represents it."
  (let* ((conn-id (spiral-make-conn-id host port))
         (conn-pool `((:client . ,(spiral-socket-connect
                                   :client host port
                                   #'spiral-loop-client-dispatcher))))
         (new-project (spiral-create-project conn-id
                                             project-dir
                                             conn-pool
                                             server-proc)))
    (spiral-projects-add new-project)
    new-project))


(defun spiral--connection-prompt-choices (project-dir)
  "Return a list of available connections.
The returned list will have connections related to PROJECT-DIR appear
first."
  (let* ((projects (spiral-projects-as-list))
         (for-project-dir (seq-filter (lambda (p)
                                        (string= (spiral-project-dir p) project-dir))
                                      projects))
         (rest (seq-filter (lambda (p)
                             (not (string= (spiral-project-dir p) project-dir)))
                           projects)))
    (mapcar #'spiral-project-repr (append for-project-dir rest))))


(defun spiral--connection-prompt (&optional project-dir)
  "Prompt the user for a Clojure Socket REPL coordinates.
The user is prompted based on available connections found in
`spiral-projects', with the possibility to specify a host:port for a
project that might not be in the existing choices.

This function returns a list with an SPIRAL project and a 'new project'
boolean flag (nil or t-ish) indicating if the selected project comes from
an existing selection (nil), or if it was brand new (t-ish).

If PROJECT-DIR is non-nil and a string, `spiral-projects' connections
already created for PROJECT-DIR will appear first in the list."
  (let* ((connection-choices (spiral--connection-prompt-choices project-dir))
         (prompt (if connection-choices
                     "Select project (or type <host>:<port> for a new connection): "
                   "Type <host>:<port> coordinates of your running Socket REPL: "))
         (selection (completing-read prompt
                                     connection-choices
                                     nil nil nil
                                     'spiral-connect--history
                                     (car connection-choices)))
         (conn-id-regexp "[^\\:]+:[0-9]+")
         (named-project-repr-regexp (format "[a-zA-Z0-9].* \\[\\(%s\\)\\]"
                                            conn-id-regexp))
         (conn-id (cond ((string-match named-project-repr-regexp selection)
                         (intern (match-string 1 selection)))
                        ((string-match-p conn-id-regexp selection)
                         (intern selection))
                        (t (user-error "%S does not look like a valid <host>:<port>" selection)))))
    (if-let (project (spiral-projects-get conn-id))
        (list (spiral-projects-get conn-id) nil)
      (let* ((host-port (spiral-conn-host-port conn-id))
             (host (car host-port))
             (port (cdr host-port)))
        (list (spiral--init-project-connection host port project-dir)
              'new)))))


(defun spiral--connect-to (host port &optional buffer)
  "Create a project for a connection to a Socket REPL running on HOST:PORT.
If BUFFER is non-nil, associates it to the new project/connection.

This function is meant to be a debugging helper, and not to be used to
connect to socket REPLs.  Refer to `spiral-connect' and
`spiral-connect-to'."
  (if-let (project (spiral-projects-get (spiral-make-conn-id host port)))
      (spiral--assoc-buffer project nil buffer)
    (let ((project (spiral--init-project-connection host port (spiral-clojure-dir))))
      (spiral--assoc-buffer project 'new buffer))))



;; Main interactive functions
;; -------------------------------------------------------------------

;;;###autoload
(defun spiral-connect (&optional just-ask)
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
            (y-or-n-p
             (concat "This is not a Clojure buffer. You can connect to a Socket "
                     "REPL but this buffer won't be able to interact with it.\n"
                     "Do you still want to create a connection to a REPL? ")))
    (let* ((project-dir (spiral-clojure-dir))
           (project (spiral-projects-get-by-dir project-dir)))
      (cond ((or just-ask (not (derived-mode-p 'clojure-mode)))
             (apply #'spiral--assoc-buffer (spiral--connection-prompt project-dir)))
            (spiral-conn-id (thread-first (concat
                                           "You are already connected to %s.  "
                                           "If you really want to change this connection, "
                                           "type `C-u \\[spiral-connect]'")
                              (format spiral-conn-id)
                              (substitute-command-keys)
                              (message)
                              (ding)))
            (project (spiral--assoc-buffer project))
            (project-dir (spiral-socket-create-server
                          (lambda (process host port)
                            (spiral--assoc-buffer
                             (spiral--init-project-connection host port project-dir process)
                             'new))))
            (t (apply #'spiral--assoc-buffer (spiral--connection-prompt)))))))


;;;###autoload
(defun spiral-connect-to ()
  "Same as `spiral-connect' but force a prompt so the user can decide where to connect."
  (interactive)
  (spiral-connect 'ask!))


(provide 'spiral)

;;; spiral.el ends here
