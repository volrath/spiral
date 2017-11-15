;;; unrepl-project.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-project.el
;; Description:
;; Author: Daniel Barreto
;; Maintainer:
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sat Nov 11 01:55:58 2017 (+0100)
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
(require 'dash)
(require 'map)


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
;; An associative data structure that holds all pending evaluations for a
;; project, indexed by their corresponding UNREPL group ids.
;; An entry in the pending evaluations data structure is also an associative
;; data structure that contains the following:
;; - `:status': either `:read', `:started-eval', `:eval', or `:exception'.
;; - `:prompt-marker': (optional) a buffer position to which print either
;;    evaluation outputs or `:out' strings.
;; - `actions': (optional) evaluation actions as provided by the
;;   `started-eval' UNREPL message.

(defun unrepl-project--pending-evals-get (pending-evals group-id)
  "Return a pending evaluation entry from PENDING-EVALS for GROUP-ID."
  (map-elt pending-evals group-id))


(defun unrepl-project--pending-evals-remove (pending-evals group-id)
  "Remove GROUP-ID entry from PENDING-EVALS."
  (map-delete pending-evals group-id))


(defun unrepl-project--pending-evals-add (pending-evals group-id entry)
  "Use GROUP-ID to add ENTRY in PENDING-EVALS."
  (map-put pending-evals group-id entry))


(defun unrepl-project--pending-eval-history (entry)
  "Return the history entry id for pending eval ENTRY."
  (map-elt entry :history-entry-id))


(defun unrepl-project--pending-eval-update (pending-evals group-id &rest kwargs)
  "Update a pending evaluation for GROUP-ID with KWARGS in PENDING-EVALS.
Return the pending-evals data structure."
  (let ((entry (unrepl-project--pending-evals-get pending-evals group-id)))
    (mapc (lambda (kv) (map-put entry (car kv) (cadr kv)))
          (-partition 2 kwargs))
    (map-put pending-evals group-id entry)
    pending-evals))


(defun unrepl-project-pending-eval-update (conn-id group-id status &rest kwargs)
  "Update a pending eval entry for GROUP-ID in CONN-ID.
All the pending evaluations are stored in PROJECTS `:pending-evals' key,
this function always expect to update their STATUS, and optionally it can
update more key-val provided by KWARGS."
  (let* ((project (unrepl-projects-get conn-id))
         (pending-evals (unrepl-project-pending-evals project)))
    (if (memq status '(:eval :exception))
        (setq pending-evals
              (unrepl-project--pending-evals-remove pending-evals group-id))
      (setq pending-evals
            (apply #'unrepl-project--pending-eval-update pending-evals group-id :status status kwargs)))
    (unrepl-project-set-in conn-id :pending-evals pending-evals)))


(defun unrepl-project-pending-evals-get-history-id (conn-id group-id)
  "Return a history entry id for GROUP-ID in CONN-ID, if any."
  (let* ((project (unrepl-projects-get conn-id))
         (pending-evals (unrepl-project-pending-evals project))
         (pending-eval-entry (unrepl-project--pending-evals-get pending-evals group-id)))
    (when pending-eval-entry
      (unrepl-project--pending-eval-history pending-eval-entry))))


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
(defun unrepl-project-quit (proj)
  "Kill and remove project PROJ."
  (interactive)
  (let* ((repl-buf (unrepl-project-repl-buffer proj))
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
    ;; Remove the entry from `unrepl-projects'
    (setq unrepl-projects (map-delete unrepl-projects (unrepl-project-id proj)))))


(defun unrepl-project-id (proj)
  "Return the ID of the given PROJ."
  (map-elt proj :id))


(declare-function unrepl--conn-host-port "unrepl")
(defun unrepl-project-port (proj)
  "Return the Socket REPL port for the given PROJ."
  (cdr (unrepl--conn-host-port (unrepl-project-id proj))))


(defun unrepl-project-pending-evals (proj)
  "Return the PROJ's pending evaluations data structure."
  (map-elt proj :pending-evals))


(defun unrepl-project-repl-buffer (proj)
  "Return the REPL buffer for the given PROJ."
  (map-elt proj :repl-buffer))


(defun unrepl-project-host (proj)
  "Return the Socket REPL host for the given PROJ."
  (car (unrepl--conn-host-port (unrepl-project-id proj))))


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


(defun unrepl-project-client-proc (proj)
  "Return the client network process for the given PROJ."
  (map-elt (unrepl-project-conn-pool proj) :client))


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
