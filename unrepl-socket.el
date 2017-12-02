;;; unrepl-socket.el --- UNREPL socket connection utilities -*- lexical-binding: t; -*-
;; 
;; Filename: unrepl-socket.el
;; Description:
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Dec  2 13:55:17 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; UNREPL socket connection utilities
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

(require 'find-func)

(require 'unrepl-loop)
(require 'unrepl-mode)
(require 'unrepl-util)


(defcustom unrepl-blob-path nil
  "Path to UNREPL 'blob' file.
When nil, UNREPL.el will try to find the blob file in the same directory
where the unrepl package ins installed."
  :type 'file
  :group 'unrepl)

(defcustom unrepl-default-socket-repl-command 'boot
  "The default command to be used when creating a Clojure Socket REPL."
  :type '(choice (const boot)
                 (const lein)
                 (const gradle))
  :group 'unrepl)

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

(defvar-local unrepl-server-host-port nil
  "Tuple with host and port of a Socket REPL.
Only used when spinning a new Socket REPL and waiting for it to boot so
that its corresponding connection pool can be created.")



;; Utilities
;; -------------------------------------------------------------------

;; project type dependent

(defun unrepl-socket--identify-buildtools-present ()
  "Identify build systems present by their build files in PROJECT-DIR.

BORROWED FROM CIDER."
  (let* ((default-directory (unrepl-clojure-dir))
         (build-files '((lein . "project.clj")
                        (boot . "build.boot")
                        (gradle . "build.gradle"))))
    (delq nil
          (mapcar (lambda (candidate)
                    (when (file-exists-p (cdr candidate))
                      (car candidate)))
                  build-files))))


(defun unrepl-socket--clojure-project-type ()
  "Determine the type of Clojure project.
BORROWED FROM CIDER.
If more than one project file types are present, check for a preferred
build tool in `unrepl-preferred-build-tool', otherwise prompt the user to
choose."
  (let* ((choices (unrepl-socket--identify-buildtools-present))
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


(defun unrepl-socket--repl-cmd (project-type port)
  "Return a string specifying a Socket REPL cmd using PROJECT-TYPE on PORT.

PROJECT-TYPE is a symbol.  It can be either `boot', `leiningen' or
`gradle'."
  (format "%S socket-server --port %S wait" project-type port))


(defun unrepl-socket--server-initialized-p (_project-type output)
  "Return non-nil if OUTPUT can be interpreted as REPL initialization for PROJECT-TYPE."
  (pcase _project-type
    ;; TODO: is this always the first line?
    (_ (string-match "Socket server started on port" output))))

;; blob

(defun unrepl-socket--find-default-blob ()
  "Look for the blob file in the same dir where `unrepl-connect' is defined."
  (let* ((unrepl-file (cdr (find-function-library 'unrepl-connect)))
         (unrepl-dir (file-name-directory unrepl-file)))
    (expand-file-name "blob.clj" unrepl-dir)))


(defun unrepl-socket--blob ()
  "Return the UNREPL blob as a string."
  (let ((blob-path (or unrepl-blob-path
                       (unrepl-socket--find-default-blob))))
    (with-temp-buffer
      (insert-file-contents blob-path)
      (buffer-string))))

;; network

(defun unrepl-socket--issue-new-socket-port ()
  "Generate a random port number between 60100 and 60199.
Avoids using known ports in `unrepl-projects' for localhost."
  (let* ((conn-ids (mapcar (lambda (p) (symbol-name (unrepl-project-id p)))
                           (unrepl-projects-as-list)))
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


(defun unrepl-socket--get-network-buffer (type server-host server-port)
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


(defun unrepl-socket--server-message-handler (project-type connected-callback)
  "Create a proper 'process filter' function for a newly created Socket REPL.
The returned function receives two arguments: PROCESS and OUTPUT, and waits
for the Socket REPL initialization to execute CONNECTED-CALLBACK.
PROJECT-TYPE is used to figure out when has the REPL been initialized."
  (let ((calling-buffer (current-buffer)))
    (lambda (process output)
      (let ((server-buffer (process-buffer process)))
        (when (buffer-live-p server-buffer)
          (with-current-buffer server-buffer
            (when (unrepl-socket--server-initialized-p project-type output)
              (let* ((host (car unrepl-server-host-port))
                     (port (cdr unrepl-server-host-port)))
                (message "Clojure Socket REPL server started on %s:%S." host port)
                (with-current-buffer calling-buffer
                  (funcall connected-callback host port))))
            (goto-char (point-max))
            (insert output)))))))


(defun unrepl-socket--server-sentinel (process event)
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



;; Main connection functions
;; -------------------------------------------------------------------

(defun unrepl-socket-create-server (connected-callback)
  "Create a new Clojure Socket REPL and return its process.
CONNECTED-CALLBACK is a function to be executed when the new Socket REPL
has started, it should receive three parameters: PROCESS, HOST, and PORT."
  (let* ((project-type (unrepl-socket--clojure-project-type))
         (port (unrepl-socket--issue-new-socket-port))
         (cmd (unrepl-socket--repl-cmd project-type port))
         (server-buf (unrepl-socket--get-network-buffer :server "127.0.0.1" port))
         (server-proc (start-file-process-shell-command "unrepl-socket-server"
                                                        server-buf
                                                        cmd)))
    (message "Starting a new Socket REPL via %s"
             (propertize cmd 'face 'font-lock-keyword-face))
    (set-process-coding-system server-proc 'utf-8-unix 'utf-8-unix)
    (set-process-sentinel server-proc #'unrepl-socket--server-sentinel)
    (set-process-filter
     server-proc
     (unrepl-socket--server-message-handler project-type connected-callback))
    server-proc))


(defun unrepl-socket-connect (type host port dispatcher-fn
                                   &optional upgrade-msg)
  "Create a new TYPE connection process to HOST:PORT.
TYPE is a keyword: `:client', `:aux', or `:side-loader'.
UPGRADE-MSG is a STR to be sent to the newly created process.
DISPATCHER-FN is a function that will receive UNREPL EDN messages and will
dispatch to handlers according to the message's tag.

Returns a pair (TYPE . new-process)."
  (let* ((conn-id (unrepl-make-conn-id host port))
         (new-proc (make-network-process
                    :name (format "unrepl-%s" (unrepl-keyword-name type))
                    :buffer (unrepl-socket--get-network-buffer type host port)
                    :host host
                    :service port
                    :filter #'unrepl-loop-handle-proc-message)))
    ;; Setup process
    (process-send-string new-proc (or upgrade-msg (unrepl-socket--blob)))
    (with-current-buffer (process-buffer new-proc)
      (clojure-mode)
      (setq-local unrepl-conn-id conn-id)
      (setq-local unrepl-loop-process-type type)
      (setq-local unrepl-loop-process-dispatcher dispatcher-fn))
    ;; Finally return the process
    new-proc))


(provide 'unrepl-socket)

;;; unrepl-socket.el ends here
