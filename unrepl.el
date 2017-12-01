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

(require 'clojure-mode)
(require 'find-func)
(require 'map)
(require 'seq)

(require 'unrepl-loop)
(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-repl)
(require 'unrepl-util)


(defgroup unrepl '()
  "Emacs UNREPL integration."
  :prefix "unrepl-"
  :group 'applications)

(defcustom unrepl-blob-path nil
  "Path to UNREPL 'blob' file.
When nil, UNREPL.el will try to find the blob file in the same directory
where the unrepl package ins installed."
  :type 'file
  :group 'unrepl)

(defcustom unrepl-starting-port 60100
  "Default starting port for UNREPL.el jack ins.

Each new `jack in' will incrementally use a port starting from this
number."
  :type 'integer
  :group 'unrepl)

(defcustom unrepl-prompt-for-project-on-connect 'when-needed
  "Controls whether to prompt for associated projects on `unrepl-connect'."
  :type '(choice (const :tag "always" t)
                 (const when-needed)
                 (const :tag "never" nil))
  :group 'unrepl)

(defvar-local unrepl-server-host-port nil
  "Tuple with host and port of a Socket REPL.
Only used when spinning a new Socket REPL and waiting for it to boot so
that its corresponding connection pool can be created.")



;; Utility functions
;; -------------------------------------------------------------------

(defun unrepl--find-default-blob ()
  "Look for the blob file in the same dir where `unrepl-connect' is defined."
  (let* ((unrepl-file (cdr (find-function-library 'unrepl-connect)))
         (unrepl-dir (file-name-directory unrepl-file)))
    (expand-file-name "blob.clj" unrepl-dir)))


(defun unrepl--blob ()
  "Return the UNREPL blob as a string."
  (let ((blob-path (or unrepl-blob-path
                       (unrepl--find-default-blob))))
    (with-temp-buffer
      (insert-file-contents blob-path)
      (buffer-string))))


(defun unrepl--make-conn-id (host port)
  "Return a symbol of the form HOST:PORT."
  (intern (format "%s:%S" host port)))


(defun unrepl--project-dir ()
  "Try to guess current buffer's project dir."
  (clojure-project-dir))  ;; TODO: self-hosted clojurescript


(defun unrepl--get-network-buffer (type server-host server-port)
  "Return a buffer for a connection of type TYPE.

The buffer will hold a `unrepl-conn-id' variable of the form:
SERVER-HOST:SERVER-PORT.

TYPE is a keyword, it can either be `:client', `:side-loader' or
`:aux', and it is only used to generate a correct name for the buffer.

This function makes sure to get or create a buffer to be used for the
output of a network connection process."
  (let ((buff (get-buffer-create
               (format "*unrepl-%s[%s:%S]*"
                       (unrepl-keyword-name type)
                       server-host server-port))))
    (when (and server-host server-port)
      (with-current-buffer buff
        (setq-local unrepl-server-host-port (cons server-host server-port))))
    buff))



;; Connection functions
;; -------------------------------------------------------------------

(defun unrepl--socket-repl-cmd (project-type port)
  "Return a string specifying a Socket REPL cmd using PROJECT-TYPE on PORT.

PROJECT-TYPE is a symbol.  It can be either `boot', `leiningen' or
`gradle'."
  (format "%S socket-server --port %S wait" project-type port))


(defun unrepl--create-socket-repl! (server-filter-handler)
  "Create a new Clojure Socket REPL and return its process.
SERVER-FILTER-HANDLER is the function used as a filter for the new REPL
process."
  (let* ((port (unrepl--issue-new-socket-port))
         (cmd (unrepl--socket-repl-cmd (unrepl-project-type) port))
         (server-buf (unrepl--get-network-buffer :server "127.0.0.1" port))
         (server-proc (start-file-process-shell-command
                       "unrepl-socket-server"
                       server-buf
                       cmd)))
    (message "Starting a new Socket REPL via %s"
             (propertize cmd 'face 'font-lock-keyword-face))
    (set-process-filter server-proc server-filter-handler)
    (set-process-sentinel server-proc #'unrepl--server-sentinel)
    (set-process-coding-system server-proc 'utf-8-unix 'utf-8-unix)
    server-proc))


(defun unrepl--server-message-handler (calling-buffer project-dir)
  "Create a proper 'process filter' function for a newly created Socket REPL.
The returned function receives two arguments: PROCESS and OUTPUT, and waits
for the Socket REPL initialization to create and associate an UNREPL
project to it.

CALLING-BUFFER is the buffer that initially called for creating a new
Socket REPL, which should be connected to the resulting Project.
PROJECT-DIR is the directory for the new Project to be created."
  (lambda (process output)
    (let ((server-buffer (process-buffer process)))
      (when (buffer-live-p server-buffer)
        (with-current-buffer server-buffer
          (when (string-match "Socket server started on port" output) ;; TODO: is this always the first line?
            (let* ((host (car unrepl-server-host-port))
                   (port (cdr unrepl-server-host-port)))
              (message "Clojure Socket REPL server started on %s:%S." host port)
              (with-current-buffer calling-buffer
                (unrepl--connect-buffer
                 (unrepl-connect--init-project host port
                                               project-dir
                                               process)
                 'new))))
          (goto-char (point-max))
          (insert output))))))


(defun unrepl--server-sentinel (process event)
  "Handle a Socket REPL server PROCESS EVENT."
  (let* ((server-buffer (process-buffer process))
         (conn-id (buffer-local-value 'unrepl-conn-id server-buffer))
         (problem (if (and server-buffer (buffer-live-p server-buffer))
                      (with-current-buffer server-buffer
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (unrepl-project-quit conn-id)
    (unless (string-match-p "^killed\\|^interrupt\\|^hangup" event)
      (error "Could not start nREPL server: %s" problem))))


(defun unrepl--issue-new-socket-port ()
  "Return an open port for a new Socket connection.

Increases from the value of =unrepl--last-port-used=."
  unrepl-starting-port)


(defun unrepl--socket-repl-connect (host port type)
  "Connect to a Clojure Socket REPL identified by HOST:PORT with a given TYPE.

TYPE is a keyword that can either be `:client', `:side-loader', or
`:aux', and it is used to set the correct name of the network process,
and to properly set the filter function for the process output.

Return a network connection process."
  (make-network-process
   :name (format "unrepl-%s" (unrepl-keyword-name type))
   :buffer (unrepl--get-network-buffer type host port)
   :host host
   :service port
   :filter #'unrepl-loop-handle-proc-message))


(defun unrepl-create-connection-process (type host port upgrade-msg dispatcher-fn)
  "Create a new TYPE connection process to HOST:PORT.
TYPE is a keyword: `:client', `:aux', or `:side-loader'.
UPGRADE-MSG is a STR to be sent to the newly created process.
DISPATCHER-FN is a function that will receive UNREPL EDN messages and will
dispatch to handlers according to the message's tag.

Returns a pair (TYPE . new-process)."
  (let* ((new-proc (unrepl--socket-repl-connect host port type))
         (conn-id (unrepl-process-conn-id new-proc)))
    ;; Setup process
    (process-send-string new-proc upgrade-msg)
    (with-current-buffer (process-buffer new-proc)
      (clojure-mode)
      (setq-local unrepl-conn-id conn-id)
      (setq-local unrepl-loop-process-type type)
      (setq-local unrepl-loop-process-dispatcher dispatcher-fn))
    ;; Finally return the process
    new-proc))


(defun unrepl--conn-pool-proc (pool type)
  "Look in POOL for the network process of type TYPE and return it."
  (map-elt pool type))


(defun unrepl--conn-pool-procs (pool)
  "Return a list of all the processes in the POOL."
  (mapcar #'cdr pool))


(defun unrepl-process-conn-id (process)
  "Return a `conn-id' taken from the PROCESS `:remote' information."
  (let* ((remote-info (process-contact process :remote))
         (remote-length (length remote-info)))
    (intern
     (format "%s:%S"
             (mapconcat #'number-to-string (seq-subseq remote-info 0 -1) ".")
             (elt remote-info (- remote-length 1))))))



;; Main interactive entry
;; -------------------------------------------------------------------

(defvar unrepl-calling-buffer nil
  "Saves the calling buffer for more initialization after connecting.")

(defvar unrepl-connect--history nil
  "Used as a minibuffer history variable for `unrepl-connect' prompt.")


(defun unrepl--connect-buffer (project &optional new-p)
  "Connects buffer to PROJECT.
If NEW-P is non-nil, search for all other clojure-mode buffers in PROJECT's
project-dir and connect them to the same project."
  (unless (or (derived-mode-p 'clojure-mode)
              (y-or-n-p "This is not a Clojure buffer, are you sure you want to connect it? "))
    (setq-local unrepl-conn-id (unrepl-project-id project))
    (when new-p
      ;; TODO: ...
      )
    (message "Successfully connected to %s" (unrepl-project-repr project))))


(defun unrepl-connect--init-project (host port project-dir
                                          &optional server-proc)
  "Create a new Project for PROJECT-DIR with a conn-pool connected to HOST:PORT.
This function can be called for Socket REPLs that are already running, or
it can be called for a Socket REPL that has been automatically created by
UNREPL.el, in which case SERVER-PROC should be the process that represents
it."
  (let* ((conn-id (unrepl--make-conn-id host port))
         (conn-pool `((:client . ,(unrepl-create-connection-process
                                   :client host port
                                   (unrepl--blob)
                                   #'unrepl-loop-client-dispatcher))))
         (new-project (unrepl-create-project conn-id
                                             conn-pool
                                             project-dir
                                             server-proc)))
    (unrepl-projects-add new-project)))


(defun unrepl-connect--new-project (project-dir &optional host port)
  "Create a new Project for PROJECT-DIR, with a suitable connection for it.
HOST and PORT are optional coordinates for an existing Socket REPL.  If not
given, a new Socket REPL will be created.
Returns the new project."
  (if (and host port)
      (let ((conn-id (unrepl--make-conn-id host port)))
        (if-let (project (unrepl-projects-get conn-id))
            (unrepl--connect-buffer project 'new)
          (unrepl-connect--init-project host port project-dir)))
    (unrepl--create-socket-repl!
     (unrepl--server-message-handler (current-buffer) project-dir))))


(defun unrepl-connect--choices (project-dir)
  "Return a list of available connections.
The returned list will have connections related to PROJECT-DIR appear
first."
  (let* ((projects (map-values unrepl-projects))
         (for-project-dir (-filter (lambda (p)
                                     (eql (unrepl-project-dir p) project-dir))
                                   projects))
         (for-project-dir (-sort (lambda (p1 p2)
                                   (time-less-p (unrepl-project-created p2)
                                                (unrepl-project-created p1)))
                                 for-project-dir))
         (rest (-filter (lambda (p)
                          (not (eql (unrepl-project-dir p) project-dir)))
                        projects)))
    (mapcar (lambda (p) (unrepl-project-repr p))
            (append for-project-dir rest))))


(defun unrepl-connect--generate-random-port ()
  "Generate a random port number between 60100 and 60199.
Avoids using known ports in `unrepl-projects' for localhost."
  (let* ((conn-ids (mapcar #'symbol-name
                           (map-keys unrepl-projects)))
         (port-used-p (lambda (p)
                        (-find (lambda (c-id)
                                 (or (string= c-id (format "localhost:%d" p))
                                     (string= c-id (format "127.0.0.1:%d" p))))
                               conn-ids)))
         (gen-new-port (lambda () (+ 60100 (random 100))))
         (port (funcall gen-new-port)))
    (while (funcall port-used-p port)
      (setq port (funcall gen-new-port)))
    port))


;; TODO: make it work for clojurescript
(defun unrepl-connect--find-project-by-dir (project-dir)
  "Find a project in `unrepl-projects' for PROJECT-DIR.
If more than one project matches with PROJECT-DIR, return the most recently
created.
Return matching project or nil"
  (-find (lambda (p) (eql (unrepl-project-dir p) project-dir))
         (-sort (lambda (p1 p2)
                  (time-less-p (unrepl-project-created p2)
                               (unrepl-project-created p1)))
                unrepl-projects)))


(defun unrepl-connect--prompt (&optional project-dir)
  "Prompt the user for a Clojure Socket REPL coordinates.
The user is prompted based on available connections found in
`unrepl-projects', with the possibility to specify a host:port for a
project that might not be in the existing choices.

This function returns a project or a list containing (project 'new) if the
project didn't come from any pre-existing connection.  if PROJECT-DIR is
non-nil and a string, `unrepl-projects' connections already created for
PROJECT-DIR will appear first in the list."
  (let* ((connection-choices (unrepl-connect--choices project-dir))
         (prompt (if connection-choices
                     "Select project (or type <host>:<port> for a new connection): "
                   "Type <host>:<port> coordinates of your running Socket REPL: "))
         (selection (completing-read prompt
                                     connection-choices
                                     nil nil nil
                                     'unrepl-connect--history
                                     (car connection-choices)))
         (conn-id-regexp "\\([^\\:]+\\):\\([0-9]+\\)")
         (project-repr-regexp (format "[a-zA-Z0-9].* \\[\\(%s\\)\\]"
                                      conn-id-regexp)))
    (if (string-match project-repr-regexp selection)
        (unrepl-projects-get (intern (match-string 1 selection)))
      (if (string-match conn-id-regexp selection)
          (let ((host (match-string 2 selection))
                (port (string-to-number (match-string 3 selection))))
            (list (unrepl-connect--init-project host port project-dir)
                  'new))
        (user-error "%s does not look like a valid <host>:<port>" selection)))))


;;;###autoload
(defun unrepl-connect (&optional just-ask)
  "Ask user for a Clojure Socket REPL coordinates and connect to it.
Use a list of suitable connections to aid the user into selecting and
connecting to an existing one.  After connecting, this command will try to
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
  (let* ((project-dir (unrepl--project-dir))
         (project (unrepl-projects-get-by-dir project-dir)))
    (cond (just-ask (apply #'unrepl--connect-buffer (unrepl-connect--prompt)))
          (unrepl-conn-id (-> (concat "You are already connected to %s.  "
                                      "If you really want to change this connection, type "
                                      "`C-u \\[unrepl-connect]'")
                              (format unrepl-conn-id)
                              (substitute-command-keys)
                              (message)
                              (ding)))
          (project (unrepl--connect-buffer project))
          (project-dir (unrepl--connect-buffer (unrepl-connect--new-project project-dir) 'new))
          (t (apply #'unrepl--connect-buffer (unrepl-connect--prompt))))))


;;;###autoload
(defun unrepl-connect-to ()
  "Same as `unrepl-connect' but force a prompt so the user can decide where to connect."
  (interactive)
  (unrepl-connect 'ask!))


(defun unrepl--connect-to (host port &optional buffer)
  "Connect a BUFFER to a Socket REPL running on HOST:PORT.
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (unrepl--connect-buffer
     (unrepl-connect--init-project host port (unrepl--project-dir))
     'new)))


(provide 'unrepl)
;;; unrepl.el ends here
