;;; unrepl.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov  9 23:26:00 2017 (+0100)
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

(require 'clojure-mode)
(require 'map)
(require 'seq)

(require 'unrepl-client)


(defgroup unrepl '()
  "Emacs UNREPL integration."
  :prefix "unrepl-"
  :group 'applications)

(defcustom unrepl-blob-path (expand-file-name "blob.clj")
  "Path to UNREPL 'blob' file."
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

(defcustom unrepl-default-socket-repl-command 'boot
  "The default command to be used when creating a Clojure Socket REPL."
  :type '(choice (const boot)
                 (const lein)
                 (const gradle)))

(defcustom unrepl-preferred-build-tool nil
  "Allow choosing a build system when there are many.
When there are artifacts from multiple build systems (\"lein\", \"boot\",
\"gradle\") the user is prompted to select one of them.  When non-nil, this
variable will suppress this behavior and will select whatever build system
is indicated by the variable if present.  Note, this is only when CIDER
cannot decide which of many build systems to use and will never override a
command when there is no ambiguity."
  :type '(choice (const "lein")
                 (const "boot")
                 (const "gradle")
                 (const :tag "Always ask" nil))
  :group 'unrepl
  :safe #'stringp)

(defvar unrepl-projects nil
  "AList containing all projects identified by a Connection ID \"host:port\".

Each element of this AList is also another AList containing a 'Connection
Pool', and optionally the `project-dir', `project-type', and `socket-repl'
process.")

(defvar-local unrepl-conn-id nil
  "Port number used when creating a new Socket REPL.")


;; Utility functions
;; -------------------------------------------------------------------

(defun unrepl--blob ()
  "Return the UNREPL blob as a string."
  (with-temp-buffer
    (insert-file-contents unrepl-blob-path)
    (buffer-string)))


(defun unrepl--current-dir ()
  "Return the directory of the current buffer."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))


(defun unrepl--keyword-name (keyword)
  "Return the name of KEYWORD, without the leading colon `:'."
  (substring (symbol-name keyword) 1))


(defun unrepl--make-conn-id (host port)
  "Return a symbol of the form HOST:PORT."
  (intern (format "%s:%S" host port)))


(defun unrepl--conn-host-port (conn-id)
  "Return a tuple of host<string>, port<integer> from CONN-ID."
  (let* ((s-host-port (split-string (symbol-name conn-id) ":"))
         (host (car s-host-port))
         (port (string-to-number (cadr s-host-port))))
    (cons host port)))


(defun unrepl--get-network-buffer (type &optional server-host server-port)
  "Return a buffer for a connection of type TYPE.

Optionally the buffer will hold a `unrepl-conn-id' variable of the form:
SERVER-HOST:SERVER-PORT.

TYPE is a keyword, it can either be `:client', `:side-loader' or
`:tooling', and it is only used to generate a correct name for the buffer.

This function makes sure to get or create a buffer to be used for the
output of a network connection process."
  (let ((buff (get-buffer-create
               (format "*unrepl-%s[%s:%S]*"
                       (unrepl--keyword-name type)
                       server-host server-port))))
    (when (and server-host server-port)
      (with-current-buffer buff
        (setq-local unrepl-conn-id (unrepl--make-conn-id server-host server-port))))
    buff))


;; Connection functions
;; -------------------------------------------------------------------

(defun unrepl--socket-repl-cmd (project-type port)
  "Return a string specifying a Socket REPL cmd using PROJECT-TYPE on PORT.

PROJECT-TYPE is a symbol.  It can be either `boot', `leiningen' or
`gradle'."
  (format "%S socket-server --port %S wait" project-type port))


(defun unrepl--create-socket-repl! ()
  "Create a new Clojure Socket REPL."
  (let* ((port (unrepl--issue-new-socket-port))
         (cmd (unrepl--socket-repl-cmd (unrepl--project-type) port))
         (server-buf (unrepl--get-network-buffer :server "localhost" port))
         (server-proc (start-file-process-shell-command
                       "unrepl-socket-server"
                       server-buf
                       cmd)))
    (message "Starting a new Socket REPL via %s"
             (propertize cmd 'face 'font-lock-keyword-face))
    (set-process-filter server-proc #'unrepl--handle-server-message)
    (set-process-sentinel server-proc #'unrepl--server-sentinel)
    (set-process-coding-system server-proc 'utf-8-unix 'utf-8-unix)
    server-proc))


(defun unrepl--handle-server-message (process output)
  "Process Clojure Socket REPL messages from PROCESS contained in OUTPUT.

Mostly copied from Cider's `nrepl-server-filter'"
  (let ((server-buffer (process-buffer process)))
    (when (buffer-live-p server-buffer)
      (with-current-buffer server-buffer
        (when (string-match "Socket server started on port" output) ;; This is always the first line.
          (let* ((host-port (unrepl--conn-host-port unrepl-conn-id))
                 (host (car host-port))
                 (port (cdr host-port)))
            (message "Clojure Socket REPL server started on %s:%S." host port)
            (unrepl-connect-to host port process)))
        (goto-char (point-max))
        (insert output)))))


(defun unrepl--server-sentinel (process event)
  "Handle a Socket REPL server PROCESS EVENT."
  (let* ((server-buffer (process-buffer process))
         (conn-id (buffer-local-value 'unrepl-conn-id server-buffer))
         (problem (if (and server-buffer (buffer-live-p server-buffer))
                      (with-current-buffer server-buffer
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (unrepl--kill-project conn-id)
    (unless (string-match-p "^killed\\|^interrupt\\|^hangup" event)
      (error "Could not start nREPL server: %s" problem))))


(defun unrepl--issue-new-socket-port ()
  "Return an open port for a new Socket connection.

Increases from the value of =unrepl--last-port-used=."
  unrepl-starting-port)


(defun unrepl--upgrade-connection! (proc)
  "Upgrade the PROC Socket Connection to UNREPL by feeding the UNREPL blob to it."
  (process-send-string proc (unrepl--blob)))


(defun unrepl--socket-repl-connect (host port type)
  "Connect to a Clojure Socket REPL identified by HOST:PORT with a given TYPE.

TYPE is a keyword that can either be `:client', `:side-loader', or
`:tooling', and it is used to set the correct name of the network process,
and to properly set the filter function for the process output.

Return a network connection process."
  (make-network-process
   :name (format "unrepl-%s" (unrepl--keyword-name type))
   :buffer (unrepl--get-network-buffer type host port)
   :host host
   :service port
   :filter (intern (format "unrepl-handle-%s-message" (unrepl--keyword-name type)))))


(defun unrepl--create-connection-pool (host port)
  "Create a new Connection Pool to the Socket REPL in HOST:PORT."
  (message "Upgrading to UNREPL.")
  (let* ((client-proc (unrepl--socket-repl-connect host port :client))
         ;; (side-loader-proc (unrepl--socket-repl-connect host port :side-loader))
         ;; (tooling-proc (unrepl--socket-repl-connect host port :tooling))
         )
    (unrepl--upgrade-connection! client-proc)
    `((:client . ,client-proc)
      ;; (:side-loader . ,side-loader-proc)
      ;; (:tooling . ,tooling-proc)
      )))


(defun unrepl--conn-pool-proc (pool type)
  "Look in POOL for the network process of type TYPE and return it."
  (map-elt pool type))


(defun unrepl--conn-pool-procs (pool)
  "Return a list of all the processes in the POOL."
  (mapcar #'cdr pool))


;; Project functions
;; -------------------------------------------------------------------

(defun unrepl--identify-buildtools-present ()
  "Identify build systems present by their build files in PROJECT-DIR.

BORROWED FROM CIDER."
  (let* ((default-directory (clojure-project-dir (unrepl--current-dir)))
         (build-files '((lein . "project.clj")
                        (boot . "build.boot")
                        (gradle . "build.gradle"))))
    (delq nil
          (mapcar (lambda (candidate)
                    (when (file-exists-p (cdr candidate))
                      (car candidate)))
                  build-files))))


(defun unrepl--project-type ()
  "Determine the type of Clojure project.

BORROWED FROM CIDER.

If more than one project file types are present, check for a preferred
build tool in `unrepl-preferred-build-tool', otherwise prompt the user to
choose."
  (let* ((choices (unrepl--identify-buildtools-present))
         (multiple-project-choices (> (length choices) 1))
         (default (car choices)))
    (cond ((and multiple-project-choices
                (member unrepl-preferred-build-tool choices))
           unrepl-preferred-build-tool)
          (multiple-project-choices
           (completing-read (format "Which command should be used (default %s): " default)
                            choices nil t nil nil default))
          (choices
           (car choices))
          (t unrepl-default-socket-repl-command))))


(defun unrepl--create-project (host port server-proc)
  "Create a new project structure for a conn to HOST:PORT re pr by SERVER-PROC.

The returned data structure is meant to be placed in `unrepl-projects'.

If HOST and PORT are not provided, a new Clojure Socket REPL will be
created using the appropriate command for PROJECT-DIR.

If PROJECT-DIR is nil, the value of `unrepl-default-socket-repl-command'
will be used to initialize the Clojure Socket REPL."
  (let ((project-dir (clojure-project-dir (unrepl--current-dir))))
    `((:id . ,(unrepl--make-conn-id host port))
      (:project-dir . ,project-dir)
      (:socket-repl . ,server-proc)
      (:conn-pool . ,(unrepl--create-connection-pool host port)))))


(defun unrepl--kill-project (conn-id)
  "Kill and remove project with CONN-ID."
  (let* ((proj (unrepl--projects-get conn-id))
         (server-proc (unrepl--project-socket-repl proj))
         (server-buf (when server-proc (process-buffer server-proc)))
         (pool (unrepl--project-conn-pool proj)))
    ;; Kill the main Socket REPL, if any.
    (when server-proc
      (delete-process server-proc))
    (when server-buf
      (kill-buffer server-buf))
    ;; Kill the pool connection processes.
    (mapc (lambda (p-conn-proc)
            (let ((p-conn-buf (process-buffer p-conn-proc)))
              (when p-conn-proc
                (delete-process p-conn-proc))
              (when p-conn-buf
                (kill-buffer p-conn-buf))))
          (unrepl--conn-pool-procs pool))
    ;; Remove the entry from `unrepl-projects'
    (message "removing project %S from list" conn-id)
    (setq unrepl-projects (map-delete unrepl-projects conn-id))))


(defun unrepl--project-id (proj)
  "Return the ID of the given PROJ."
  (map-elt proj :id))


(defun unrepl--project-port (proj)
  "Return the Socket REPL port for the given PROJ."
  (cdr (unrepl--conn-host-port (unrepl--project-id proj))))


(defun unrepl--project-host (proj)
  "Return the Socket REPL host for the given PROJ."
  (car (unrepl--conn-host-port (unrepl--project-id proj))))


(defun unrepl--project-dir (proj)
  "Return the directory of the given PROJ."
  (map-elt proj :project-dir))


(defun unrepl--project-socket-repl (proj)
  "Return a plist with the `:host' `:port' kv pairs for the PROJ's Socket REPL."
  (map-elt proj :socket-repl))


(defun unrepl--project-conn-pool (proj)
  "Return the PROJ's 'Connection Pool'."
  (map-elt proj :conn-pool))


(defun unrepl--projects-get (conn-id)
  "Return the project with CONN-ID, or nil."
  (map-elt unrepl-projects conn-id))

;; Main interactive entry
;; -------------------------------------------------------------------

(defun unrepl-connect-to (host port &optional server-proc)
  "Create a new project connection to a socket in HOST:PORT represented by SERVER-PROC."
  (interactive "sHost: \nnPort: ")
  (let ((new-project (unrepl--create-project host port server-proc)))
    (map-put unrepl-projects (unrepl--project-id new-project) new-project)
    new-project))


(defun unrepl-connect ()
  "Create a new Clojure Socket REPL and establish a project connection to it."
  (interactive)
  (unrepl--create-socket-repl!))


(provide 'unrepl)

;;; unrepl.el ends here
