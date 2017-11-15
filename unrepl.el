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

(require 'unrepl-loop)
(require 'unrepl-mode)
(require 'unrepl-project)
(require 'unrepl-repl)


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

(defvar-local unrepl-server-host-port nil
  "Tuple with host and port of a Socket REPL.
Only used when spinning a new Socket REPL and waiting for it to boot so
that its corresponding connection pool can be created.")


;; Utility functions
;; -------------------------------------------------------------------

(defun unrepl--blob ()
  "Return the UNREPL blob as a string."
  (with-temp-buffer
    (insert-file-contents unrepl-blob-path)
    (buffer-string)))


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


(defun unrepl--get-network-buffer (type server-host server-port)
  "Return a buffer for a connection of type TYPE.

The buffer will hold a `unrepl-conn-id' variable of the form:
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
        (setq-local unrepl-server-host-port (cons server-host server-port))))
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
         (cmd (unrepl--socket-repl-cmd (unrepl-project-type) port))
         (server-buf (unrepl--get-network-buffer :server "127.0.0.1" port))
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
          (let* ((host (car unrepl-server-host-port))
                 (port (cdr unrepl-server-host-port)))
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
    (unrepl-quit-project conn-id)
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
   :filter (intern (format "unrepl-loop-handle-%s-message" (unrepl--keyword-name type)))))


(defun unrepl--create-connection-pool (host port)
  "Create a new Connection Pool to the Socket REPL in HOST:PORT."
  (message "Upgrading to UNREPL.")
  (let* ((client-proc (unrepl--socket-repl-connect host port :client))
         ;; (side-loader-proc (unrepl--socket-repl-connect host port :side-loader))
         ;; (tooling-proc (unrepl--socket-repl-connect host port :tooling))
         (pool `((:client . ,client-proc)
                 ;; (:side-loader . ,side-loader-proc)
                 ;; (:tooling . ,tooling-proc)
                 )))
    ;; Setup client
    (unrepl--upgrade-connection! client-proc)
    (with-current-buffer (process-buffer client-proc)
      (clojure-mode))
    ;; Setup `unrepl-conn-id' on each proc buffer
    (let ((conn-id (unrepl-process-conn-id client-proc)))
      (mapc (lambda (p-conn)
              (with-current-buffer (process-buffer (cdr p-conn))
                (setq-local unrepl-conn-id conn-id)))
            pool))
    ;; Finally return the pool
    pool))


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

(defun unrepl-connect-to (host port &optional server-proc)
  "Create a new project connection to a socket in HOST:PORT.
An optional SERVER-PROC can be passed, which would be the actual Clojure
Socket REPL process.
Return connected project."
  (interactive "sHost: \nnPort: ")
  (if-let (project (unrepl-projects-get (unrepl--make-conn-id host port)))
      (progn
        (message "Connection to %s already exists. Reusing it."
                 (unrepl--make-conn-id host port))
        project)
    (let* ((conn-pool (unrepl--create-connection-pool host port))
           (conn-id (unrepl-process-conn-id (unrepl--conn-pool-proc conn-pool :client)))
           (new-project (unrepl-create-project conn-id
                                               conn-pool
                                               server-proc)))
      (unrepl-projects-add new-project))))


(defun unrepl-connect ()
  "Create a new Clojure Socket REPL and establish a project connection to it."
  (interactive)
  (unrepl--create-socket-repl!))


(provide 'unrepl)

;;; unrepl.el ends here
