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


(defun unrepl--keyword-name (keyword)
  "Return the name of KEYWORD, without the leading colon `:'."
  (substring (symbol-name keyword) 1))


(defun unrepl--make-conn-id (host port)
  "Return a symbol of the form HOST:PORT."
  (intern (format "%s:%S" host port)))


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
   :name (format "unrepl-%s" (unrepl--keyword-name type))
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

;;;###autoload
(defun unrepl-connect-to (host port &optional server-proc)
  "Create a new project connection to a socket in HOST:PORT.
An optional SERVER-PROC can be passed, which would be the actual Clojure
Socket REPL process.
Return connected project."
  (interactive "sHost: \nnPort: ")
  (when (null unrepl-calling-buffer)
    (setq unrepl-calling-buffer (current-buffer)))
  (if-let (project (unrepl-projects-get (unrepl--make-conn-id host port)))
      (progn
        (message "Connection to %s already exists. Reusing it."
                 (unrepl--make-conn-id host port))
        project)
    ;; We only start with the `:client' connection process in the pool, because
    ;; it's the only one needed.  `:aux' and `:side-loader' are created when
    ;; `:client' gets greeted.
    (let* ((conn-pool `((:client . ,(unrepl-create-connection-process :client host port
                                                                      (unrepl--blob)
                                                                      #'unrepl-loop-client-dispatcher))))
           (conn-id (unrepl-process-conn-id (unrepl--conn-pool-proc conn-pool :client)))
           (new-project (unrepl-create-project conn-id
                                               conn-pool
                                               server-proc)))
      (prog1 (unrepl-projects-add new-project)
        ;; If this is a Clojure REPL, enable unrepl-mode and assign connection
        ;; id right away.
        ;; TODO: Look for all other clojure buffers in the same project and
        ;; enable unrepl-mode on them as well.  This should be a customizable
        ;; behavior.
        (with-current-buffer unrepl-calling-buffer
          (when (derived-mode-p 'clojure-mode)
            (unrepl-mode)
            (setq-local unrepl-conn-id conn-id)
            (setq unrepl-calling-buffer nil)))))))


;;;###autoload
(defun unrepl-connect ()
  "Create a new Clojure Socket REPL and establish a project connection to it."
  (interactive)
  (setq unrepl-calling-buffer (current-buffer))
  (unrepl--create-socket-repl!))


(provide 'unrepl)

;;; unrepl.el ends here
