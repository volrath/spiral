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
When there are artifacts from multiple build systems (\"lein\", or
\"boot\") the user is prompted to select one of them.  When non-nil, this
variable will suppress this behavior and will select whatever build system
is indicated by the variable if present.  Note, this is only when CIDER
cannot decide which of many build systems to use and will never override a
command when there is no ambiguity."
  :type '(choice boot
                 lein
                 (const :tag "Always ask" nil))
  :group 'unrepl)

(defcustom unrepl-start-socket-repl-expr
  "(do (require 'clojure.core.server) (let [srv (clojure.core.server/start-server {:name :repl :port 0 :accept 'clojure.core.server/repl})] (println (pr-str [:unrepl.el/server-ready (.getLocalPort srv)]))))"
  "Expression to evaluate to initiate a Socket REPL running on a random port."
  :type 'string
  :group 'unrepl)

(defcustom unrepl-lein-command "lein"
  "The command used to execute Leiningen."
  :type 'string
  :group 'unrepl)

(defcustom unrepl-lein-global-options nil
  "Command global options used to execute Leiningen (e.g.: -o for offline)."
  :type 'string
  :group 'unrepl
  :safe #'stringp)

(defcustom unrepl-lein-parameters "trampoline run"
  "Params passed to Lein to start a Socket REPL server via `unrepl-connect'.
Command resolution will append
\"-m clojure.main -e `unrepl-start-socket-repl-expr'\" at the end of these
 parameters.  See`unrepl-socket--repl-cmd' for more info."
  :type 'string
  :group 'unrepl
  :safe #'stringp)

(defcustom unrepl-boot-command "boot"
  "The command used to execute Boot."
  :type 'string
  :group 'unrepl)

(defcustom unrepl-boot-global-options nil
  "Command global options used to execute Boot (e.g.: -c for checkouts)."
  :type 'string
  :group 'unrepl)

(defcustom unrepl-boot-parameters "wait"
  "Params passed to boot to start a Socket REPL server via `unrepl-connect'.
Command resolution will prepend \"-i `unrepl-start-socket-repl-expr' before
these parameters.  See `unrepl-socket--repl-cmd' for more info."
  :type 'string
  :group 'unrepl)

(defvar-local unrepl-server-host-port nil
  "Tuple with host and port of a Socket REPL.
Only used when spinning a new Socket REPL and waiting for it to boot so
that its corresponding connection pool can be created.")

(defvar-local unrepl-server-started nil
  "Boolean flag that indicates if a server Socket REPL has started yet.")

(define-error 'unrepl-connection-error "There was a problem connecting")



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


(defun unrepl-socket--repl-cmd (project-type)
  "Return a string specifying a Socket REPL cmd using PROJECT-TYPE on PORT.

PROJECT-TYPE is a symbol.  It can be either `boot' or `lein'."
  (pcase project-type
    ('boot (format "%s%s call -e \"%s\" %s"
                   unrepl-boot-command
                   (if unrepl-boot-global-options
                       (concat " " unrepl-boot-global-options)
                     "")
                   unrepl-start-socket-repl-expr
                   unrepl-boot-parameters))
    ('lein (format "%s %s %s -m clojure.main -e \"%s\""
                   unrepl-lein-command
                   (if unrepl-lein-global-options
                       (concat " " unrepl-lein-global-options)
                     " ")
                   unrepl-lein-parameters
                   unrepl-start-socket-repl-expr))
    (_ (user-error (format
                    (concat
                     "UNREPL.el couldn't figure out your build tool (%s).\n"
                     "Maybe it is missing or maybe there is no support for it yet.")
                    project-type)))))

;; blob

(defun unrepl-socket--find-default-blob ()
  "Look for the blob file in the same dir where `unrepl-connect' is defined."
  (expand-file-name "blob.clj" (unrepl-dir)))


(defun unrepl-socket--blob ()
  "Return the UNREPL blob as a string."
  (let ((blob-path (or unrepl-blob-path
                       (unrepl-socket--find-default-blob))))
    (with-temp-buffer
      (insert-file-contents blob-path)
      (buffer-string))))

;; network

(defun unrepl-socket--server-initialized-p (output)
  "Check if OUTPUT can be interpreted as a Socket REPL initialization.
Return new Socket REPL port"
  (when-let (server-ready-match (string-match "\\[:unrepl\.el/server-ready \\([0-9]+\\)\\]" output))
    (string-to-number (match-string 1 output))))


(defun unrepl-socket--get-network-buffer (type server-host &optional server-port)
  "Return a buffer for a connection of type TYPE.

The buffer will hold a `unrepl-conn-id' variable of the form:
SERVER-HOST:SERVER-PORT.

TYPE is a keyword, it can either be `:client', `:side-loader' or
`:aux', and it is only used to generate a correct name for the buffer.

This function makes sure to get or create a buffer to be used for the
output of a network connection process."
  (let* ((buff-name (if (eql type :server)
                        "*unrepl-server[init]*"
                      (format "*unrepl-%s[%s:%S]*"
                              (unrepl-keyword-name type)
                              server-host server-port)))
         (buff (get-buffer-create buff-name)))
    (when (and server-host server-port)
      (with-current-buffer buff
        (setq-local unrepl-server-host-port (cons server-host server-port))))
    buff))


(defun unrepl-socket--server-message-handler (connected-callback)
  "Create a proper 'process filter' function for a newly created Socket REPL.
The returned function receives two arguments: PROCESS and OUTPUT, and waits
for the Socket REPL initialization to execute CONNECTED-CALLBACK.
PROJECT-TYPE is used to figure out when has the REPL been initialized."
  (let ((calling-buffer (current-buffer)))
    (lambda (process output)
      (let ((server-buffer (process-buffer process)))
        (when (and (buffer-live-p server-buffer)
                   (not (buffer-local-value 'unrepl-server-started server-buffer)))
          (with-current-buffer server-buffer
            (when-let (port (unrepl-socket--server-initialized-p
                             (concat (buffer-substring-no-properties (point-min) (point-max))
                                     output)))
              (message "Clojure Socket REPL server started on %d." port)
              (rename-buffer
               (replace-regexp-in-string "init" (number-to-string port) (buffer-name))
               t)
              (setq-local unrepl-server-started t)
              (with-current-buffer calling-buffer
                (funcall connected-callback process "localhost" port)))
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
    (unless (string-match-p "^killed\\|^interrupt\\|^hangup\\|^broken" event)
      (error "Could not start Socket REPL server: %s" problem))))


(defun unrepl-socket--client-sentinel (process event)
  "Handle a Socket REPL client PROCESS EVENT.
When a 'disconnect' EVENT comes from PROCESS, quit the PROCESS' project and
shares the message with the user."
  (when (string-match-p "^killed\\|^interrupt\\|^hangup\\|broken" event)
    (unrepl-project-quit (buffer-local-value 'unrepl-conn-id (process-buffer process))
                         event)))



;; Main connection functions
;; -------------------------------------------------------------------

(defun unrepl-socket-create-server (connected-callback)
  "Create a new Clojure Socket REPL and return its process.
CONNECTED-CALLBACK is a function to be executed when the new Socket REPL
has started, it should receive three parameters: PROCESS, HOST, and PORT."
  (let* ((default-directory (unrepl-clojure-dir))
         (project-type (unrepl-socket--clojure-project-type))
         (cmd (unrepl-socket--repl-cmd project-type))
         (server-buf (unrepl-socket--get-network-buffer :server "localhost"))
         (server-proc (start-file-process-shell-command "unrepl-socket-server"
                                                        server-buf
                                                        cmd)))
    (message "Starting a new Socket REPL via %s"
             (propertize cmd 'face 'font-lock-keyword-face))
    (set-process-coding-system server-proc 'utf-8-unix 'utf-8-unix)
    (set-process-sentinel server-proc #'unrepl-socket--server-sentinel)
    (set-process-filter
     server-proc
     (unrepl-socket--server-message-handler connected-callback))
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
         (new-proc (condition-case err
                       (make-network-process
                        :name (format "unrepl-%s" (unrepl-keyword-name type))
                        :buffer (unrepl-socket--get-network-buffer type host port)
                        :host host
                        :service port
                        :filter #'unrepl-loop-handle-proc-message)
                     (file-error
                      (signal 'unrepl-connection-error
                              (list conn-id
                                    (cadr (cdr err))))))))
    ;; Setup process
    (process-send-string new-proc (or upgrade-msg (unrepl-socket--blob)))
    (when (eql type :client)
      (set-process-sentinel new-proc #'unrepl-socket--client-sentinel))
    (with-current-buffer (process-buffer new-proc)
      (clojure-mode)
      (setq-local unrepl-conn-id conn-id)
      (setq-local unrepl-loop-process-type type)
      (setq-local unrepl-loop-process-dispatcher dispatcher-fn))
    ;; Finally return the process
    new-proc))


(provide 'unrepl-socket)

;;; unrepl-socket.el ends here
