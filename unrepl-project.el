;;; unrepl-project.el --- Abstraction over multiple socket connections -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-project.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Nov 11 01:55:58 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Abstraction over multiple socket connections
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
(require 'dash)
(require 'map)

(require 'unrepl-util)


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

(defvar unrepl-projects nil
  "AList containing all projects identified by a Connection ID \"host:port\".

Each element of this AList is also another AList containing a 'Connection
Pool', and optionally the `project-dir', `project-type', and `socket-repl'
process.")


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


(defun unrepl--current-dir ()
  "Return the directory of the current buffer."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))


;; Pending Evaluations
;; -------------------------------------------------------------------
;; Projects store a queue of pending evaluations.  Each pending evaluation is an
;; associative data structure that contains the following:
;; - `:status': either `:sent', `:read', `:started-eval', `:eval', or
;;   `:exception'.
;; - `:group-id': (optional) an UNREPL group id.
;; - `:repl-history-idx': (optional) only if the input was sent from the REPL,
;;    this would be the index in history.
;; - `:prompt-marker': (optional) a REPL buffer position to which print either
;;    evaluation outputs or `:out' strings.
;; - `actions': (optional) evaluation actions as provided by the
;;    `started-eval' UNREPL message.
;;
;; Pending evaluations' life cycle start when an input string is sent to the
;; UNREPL server (either by using the human REPL interface, or by evaluating
;; clojure buffer code).  At this very moment, a pending evaluation is created
;; with only a status `:sent', and it will be put in the pending evaluations
;; queue.  Any other input sent while processing this pending evaluation, will
;; generate new pending evaluation entries that will be added to the queue and
;; processed in order.
;; An input string sent to the UNREPL server will generate ideally 1 `:read'
;; message back from the server, but in general, it can generate 0 or more of
;; them.
;;
;; The first `:read' message received after sending input stream will be used to
;; update the pending evaluation status, add a group id, and, if the input came
;; from the REPL, update its prompt marker.
;; `:start-eval' messages will be used to add a set of actions to the pending
;; evaluation structure.
;; When `:eval' messages are received (or `:exception's), we will display them
;; according to how the input was generated in the first place (REPL or buffer
;; eval)
;; Subsequent `:read' messages received for the same input (or put in a
;; different way, not interrupted by another `:prompt' message) will modify the
;; same pending evaluation as their predecessors, making sure to from it the
;; actions and group-id information.
;;
;; When a `:prompt' is received again, the top of the queue (`:eval'ed pending
;; evaluation) will be taken out, and the process start again.

(defun unrepl-project-pending-eval (conn-id)
  "Return the beginning of CONN-ID's `:pending-evals' queue."
  (car (unrepl-project-pending-evals conn-id)))


(defun unrepl-project-pending-eval-add (conn-id &rest kwargs)
  "Add a pending evaluation to the end of the CONN-ID'S `:pending-evals' queue.
KWARGS are key-values used to create the pending evaluation entry."
  (let* ((project (unrepl-projects-get conn-id))
         (pending-evals (unrepl-project-pending-evals project))
         (entry (mapcar (lambda (pair)
                          (cons (car pair) (cadr pair)))
                        (-partition 2 kwargs))))
    (unrepl-project-set-in conn-id
                           :pending-evals (nconc pending-evals `(,entry)))))


(defun unrepl-project-pending-eval-update (conn-id &rest kwargs)
  "Update the entry at the beginning of the CONN-ID's `:pending-evals' queue.
KWARGS are the key-values to update the pending evaluation entry."
  (let* ((project (unrepl-projects-get conn-id))
         (pending-evals (unrepl-project-pending-evals project))
         (entry (car pending-evals)))
    (mapc (lambda (kv) (map-put entry (car kv) (cadr kv)))
          (-partition 2 kwargs))
    (unrepl-project-set-in conn-id
                           :pending-evals (cons entry (cdr pending-evals)))))


(defun unrepl-project-pending-eval-history-idx (conn-id)
  "Return the `:repl-history-idx' from the top of the CONN-ID's queue."
  (-> conn-id
      (unrepl-projects-get)
      (unrepl-project-pending-evals)
      (car)
      (map-elt :repl-history-idx)))


(defun unrepl-project-pending-eval-callback (conn-id)
  "Return the `:eval-callback' from the top of the CONN-ID's `:pending-evals' queue."
  (-> conn-id
      (unrepl-projects-get)
      (unrepl-project-pending-evals)
      (car)
      (map-elt :eval-callback)))


(defun unrepl-project-pending-eval-actions (conn-id)
  "Return `:actions' form the top of the CONN-ID's `:pending-evals' queue."
  (-> conn-id
      (unrepl-projects-get)
      (unrepl-project-pending-evals)
      (car)
      (map-elt :actions)))


(defun unrepl-project-pending-evals-shift (conn-id)
  "Shift the CONN-ID's `:pending-evals' queue and return the shifted entry."
  (let* ((project (unrepl-projects-get conn-id))
         (pending-evals (unrepl-project-pending-evals project))
         (entry (car pending-evals)))
    (unrepl-project-set-in conn-id :pending-evals (cdr pending-evals))
    entry))


(defun unrepl-project-pending-eval-entry-history-idx (entry)
  "Return the `:repl-history-idx' from a pending eval ENTRY."
  (map-elt entry :repl-history-idx))


(defun unrepl-project-pending-eval-entry-actions (entry)
  "Return the `:actions' from a pending eval ENTRY."
  (map-elt entry :actions))


;; UNREPL Projects
;; -------------------------------------------------------------------
;; `unrepl-projects' is an associative data structure where keys are Connection
;; IDs and values are Project data structures.
;; A Project is an associative data structure that holds:
;; - `:id': A Connection ID.
;; - `:conn-pool': An AList with the 3 UNREPL connections for this project.
;; - `:pending-evals': A Pending Evals data structure.
;; - `:repl-buffer': A buffer that holds human-focused REPL interaction.
;; - `:project-dir': An optional stringn pointing to the project's dir.
;; - `:project-type': An optional string referring to the type of project.
;; - `:socket-repl': An optional process referring to the Socket REPL server.

(defun unrepl-project-type ()
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


(declare-function unrepl-repl-create-buffer "unrepl-repl")
(defun unrepl-create-project (conn-id conn-pool server-proc)
  "Create a new project structure with id CONN-ID for a CONN-POOL.

The returned data structure is meant to be placed in `unrepl-projects'.

SERVER-PROC is an optional process representing the Clojure Socket REPL."
  (let ((project-dir (clojure-project-dir (unrepl--current-dir))))
    `((:id . ,conn-id)
      (:namespace . nil)
      (:project-dir . ,project-dir)
      (:socket-repl . ,server-proc)
      (:repl-buffer . ,(unrepl-repl-create-buffer conn-id))
      (:conn-pool . ,conn-pool)
      (:pending-evals . nil))))


(declare-function unrepl--conn-pool-procs "unrepl")
(declare-function unrepl-connected-buffers "unrepl-mode")
(defun unrepl-project-quit (conn-id)
  "Kill and remove project with CONN-ID."
  (interactive)
  (let* ((proj (unrepl-projects-get conn-id))
         (repl-buf (unrepl-project-repl-buffer proj))
         (server-proc (unrepl-project-socket-repl proj))
         (server-buf (when server-proc (process-buffer server-proc)))
         (pool (unrepl-project-conn-pool proj)))
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
    ;; Kill the REPL buffer
    (when repl-buf
      (kill-buffer repl-buf))
    ;; Search for all buffers connected to this project and unbind their connection.
    (mapc (lambda (unrepl-buf)
            (with-current-buffer unrepl-buf
              (kill-local-variable 'unrepl-conn-id)))
          (unrepl-connected-buffers conn-id))
    ;; Remove the entry from `unrepl-projects'
    (setq unrepl-projects (map-delete unrepl-projects conn-id))))


(defun unrepl-project-id (proj)
  "Return the ID of the given PROJ."
  (map-elt proj :id))


(defun unrepl-project-port (proj)
  "Return the Socket REPL port for the given PROJ."
  (cdr (unrepl-conn-host-port (unrepl-project-id proj))))


(defun unrepl-project-pending-evals (proj)
  "Return the PROJ's pending evaluations data structure."
  (map-elt proj :pending-evals))


(defun unrepl-project-repl-buffer (proj)
  "Return the REPL buffer for the given PROJ."
  (map-elt proj :repl-buffer))


(defun unrepl-project-host (proj)
  "Return the Socket REPL host for the given PROJ."
  (car (unrepl-conn-host-port (unrepl-project-id proj))))


(defun unrepl-project-namespace (proj)
  "Return the current namespace used in PROJ."
  (map-elt proj :namespace))


(defun unrepl-project-dir (proj)
  "Return the directory of the given PROJ."
  (map-elt proj :project-dir))


(defun unrepl-project-socket-repl (proj)
  "Return a plist with the `:host' `:port' kv pairs for the PROJ's Socket REPL."
  (map-elt proj :socket-repl))


(defun unrepl-project-conn-pool (proj)
  "Return the PROJ's 'Connection Pool'."
  (map-elt proj :conn-pool))


(defun unrepl-project-conn-pool-get-process (proj type)
  "Return the TYPE network process for the given PROJ."
  (map-elt (unrepl-project-conn-pool proj) type))


(defun unrepl-project-conn-pool-set-in (conn-id &rest kwargs)
  "Set new key-vals in CONN-ID's `:conn-pool', provided by KWARGS.
KWARGS is expected to be pairs of keywords and processes."
  (let* ((proj (unrepl-projects-get conn-id))
         (conn-pool (unrepl-project-conn-pool proj)))
    (mapc (lambda (pair)
            (map-put conn-pool (car pair) (cadr pair)))
          (-partition 2 kwargs))
    (unrepl-project-set-in conn-id :conn-pool conn-pool)))


(defun unrepl-projects-add (proj)
  "Add PROJ to `unrepl-projects'."
  (map-put unrepl-projects (unrepl-project-id proj) proj))


(defun unrepl-projects-get (conn-id &optional raise-not-found)
  "Return the project with CONN-ID, or nil.
When RAISE-NOT-FOUND is nil, raises an `error' if CONN-ID is not found in
`unrepl-projects'."
  (let ((proj (map-elt unrepl-projects conn-id)))
    (when (and raise-not-found
               (not proj))
      (error "No project connected to %s can be found" conn-id))
    proj))


(defun unrepl-projects-find-by-file ()
  "Try to guess the project to which the current buffer belongs too.
This function looks at the buffer's file path and searches through
`unrepl-projects' that have a true-ish `:project-dir' for a match."
  (error "Not implemented"))


(defun unrepl-project-set-in (conn-id key val)
  "Set an attribute in the `unrepl-projects' project with key CONN-ID.
KEY is expected to be a keyword, VAL is its corresponding value."
  (let ((proj (unrepl-projects-get conn-id t)))
    (map-put proj key val)
    (map-put unrepl-projects conn-id proj)
    unrepl-projects))


(provide 'unrepl-project)

;;; unrepl-project.el ends here
