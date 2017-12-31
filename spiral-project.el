;;; spiral-project.el --- Abstraction over multiple socket connections -*- lexical-binding: t; -*-
;;
;; Filename: spiral-project.el
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
(require 'map)
(require 'subr-x)

(require 'spiral-ast)
(require 'spiral-util)


(defcustom spiral-classpath '()
  "Global classpath for SPIRAL aux connections.
Ideal for REPL tooling."
  :type 'list
  :group 'spiral)

(defvar spiral-projects nil
  "AList containing all projects identified by a Connection ID \"host:port\".

Each element of this AList is also another AList containing a 'Connection
Pool', and optionally the `project-dir', `project-type', and `socket-repl'
process.")

(defvar-local spiral-pending-evals nil
  "Queue of pending evaluations.
This variable is meant to be set on network buffers for `:client' and
`:aux' interactions with an UNREPL server.")


;; Pending Evaluations
;; -------------------------------------------------------------------
;; Projects store a queue of pending evaluations in each of their connection
;; process' buffers.  Each pending evaluation is an associative data structure
;; that contains the following:
;; - `:status': either `:sent', `:read', `:started-eval', `:eval', or
;;   `:exception'.
;; - `:group-id': An UNREPL group id.  Set after the pending evaluation gets
;;    `:read'.
;; - `actions': Evaluation actions as provided by the `started-eval' UNREPL
;;    message.  Set after the pending has `started-eval'.
;; - `:repl-history-idx': (optional) only if the input was sent from the REPL,
;;    this would be the index in REPL history.
;; - `:prompt-marker': (optional) a REPL buffer position to which print either
;;    evaluation outputs or `:out' strings.
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
;; `:started-eval' messages will be used to add a set of actions to the pending
;; evaluation structure.
;; When `:eval' messages are received (or `:exception's), we will display them
;; according to how the input was generated in the first place (REPL or buffer
;; eval)
;; Subsequent `:read' messages received for the same input (or put in a
;; different way, not interrupted by another `:prompt' message) will modify the
;; same pending evaluation as their predecessors, making sure to delete from it
;; the actions and group-id information.
;;
;; When a `:prompt' is received again, the top of the queue (`:eval'ed pending
;; evaluation) will be taken out, and the process start again.

(defmacro with-process-buffer (conn-id type &rest body)
  "Execute BODY in the buffer for the TYPE connection of CONN-ID."
  (declare (indent 2))
  `(let* ((project (spiral-projects-get ,conn-id))
          (proc (spiral-project-conn-pool-get-process project ,type)))
     (with-current-buffer (process-buffer proc)
       ,@body)))


(defun spiral-pending-eval (type conn-id)
  "Return the beginning of CONN-ID TYPE's pending-evals queue."
  (with-process-buffer conn-id type
    (car spiral-pending-evals)))


(defun spiral-pending-eval-add (type conn-id &rest kwargs)
  "Add a pending evaluation to CONN-ID TYPE'S pending-evals queue.
KWARGS are key-values used to create the pending evaluation entry."
  (with-process-buffer conn-id type
    (let* ((entry (mapcar (lambda (pair)
                            (cons (car pair) (cadr pair)))
                          (seq-partition kwargs 2))))
      (setq spiral-pending-evals
            (nconc spiral-pending-evals `(,entry))))))


(defun spiral-pending-eval-update (type conn-id &rest kwargs)
  "Update the first entry at CONN-ID TYPE's pending-evals queue.
KWARGS are the key-values to update the pending evaluation entry."
  (with-process-buffer conn-id type
    (when-let (entry (car spiral-pending-evals))
      (mapc (lambda (kv) (map-put entry (car kv) (cadr kv)))
            (seq-partition kwargs 2))
      (setq spiral-pending-evals
            (cons entry (cdr spiral-pending-evals))))))


(defun spiral-pending-eval-history-idx (type conn-id)
  "Return the `:repl-history-idx' from the top of the CONN-ID TYPE's queue."
  (with-process-buffer conn-id type
    (thread-first spiral-pending-evals
      (car)
      (map-elt :repl-history-idx))))


(defun spiral-pending-eval-callback (type conn-id)
  "Return the `:eval-callback' from the top of the CONN-ID TYPE's pending-evals queue."
  (with-process-buffer conn-id type
    (thread-first spiral-pending-evals
      (car)
      (map-elt :eval-callback))))


(defun spiral-pending-eval-stdout-callback (type conn-id)
  "Return the `:stdout-callback' from the top of the CONN-ID TYPE's pending-evals queue."
  (with-process-buffer conn-id type
    (thread-first spiral-pending-evals
      (car)
      (map-elt :stdout-callback))))


(defun spiral-pending-eval-actions (type conn-id)
  "Return `:actions' from the top of the CONN-ID TYPE's pending-evals queue."
  (with-process-buffer conn-id type
    (thread-first spiral-pending-evals
      (car)
      (map-elt :actions))))


(defun spiral-pending-eval-group-id (type conn-id)
  "Return `:group-id' from the top of the CONN-ID TYPE's pending-evals queue."
  (with-process-buffer conn-id type
    (thread-first spiral-pending-evals
      (car)
      (map-elt :group-id))))


(defun spiral-pending-evals-shift (type conn-id)
  "Shift the CONN-ID TYPE's `:pending-evals' queue and return the shifted entry."
  (with-process-buffer conn-id type
    (let* ((entry (car spiral-pending-evals)))
      (setq spiral-pending-evals (cdr spiral-pending-evals))
      entry)))


(defun spiral-pending-eval-entry-status (entry)
  "Return the `:status' from a pending eval ENTRY."
  (map-elt entry :status))


(defun spiral-pending-eval-entry-history-idx (entry)
  "Return the `:repl-history-idx' from a pending eval ENTRY."
  (map-elt entry :repl-history-idx))


(defun spiral-pending-eval-entry-buffer (entry)
  "Return the `:buffer' from a pending eval ENTRY."
  (map-elt entry :buffer))


(defun spiral-pending-eval-entry-group-id (entry)
  "Return the `:group-id' from a pending eval ENTRY."
  (map-elt entry :group-id))


(defun spiral-pending-eval-entry-input (entry)
  "Return the `:input' from a pending eval ENTRY."
  (map-elt entry :input))


(defun spiral-pending-eval-entry-payload (entry)
  "Return the `:payload' from a pending eval ENTRY."
  (map-elt entry :payload))


(defun spiral-pending-eval-entry-actions (entry)
  "Return the `:actions' from a pending eval ENTRY."
  (map-elt entry :actions))


;; SPIRAL Projects
;; -------------------------------------------------------------------
;; `spiral-projects' is an associative data structure where keys are Connection
;; IDs and values are Project data structures.
;; A Project is an associative data structure that holds:
;; - `:id': A Connection ID.
;; - `:conn-pool': An AList with the 3 UNREPL connections for this project.
;; - `:pending-evals': A Pending Evals data structure.
;; - `:repl-buffer': A buffer that holds human-focused REPL interaction.
;; - `:project-dir': An optional stringn pointing to the project's dir.
;; - `:project-type': An optional string referring to the type of project.
;; - `:socket-repl': An optional process referring to the Socket REPL server.
;; - `:actions': UNREPL session actions represented as a Clojure map AST.
;; - `:print-settings': UNREPL print settings represented as an alist.

(declare-function spiral-repl-create-buffer "spiral-repl")
(defun spiral-create-project (conn-id project-dir conn-pool server-proc)
  "Create a new project structure with id CONN-ID.
PROJECT-DIR is the Clojure project's directory, it can be nil.
CONN-POOL is the connection pool, as described in the documentation.
SERVER-PROC is an optional process representing the Clojure Socket REPL.

The returned data structure is meant to be placed in `spiral-projects'."
  `((:id . ,conn-id)
    (:created . ,(current-time))
    (:namespace . ,nil)
    (:project-dir . ,project-dir)
    (:socket-repl . ,server-proc)
    (:conn-pool . ,conn-pool)
    (:repl-buffer . ,(spiral-repl-create-buffer
                      conn-id
                      (when server-proc (process-buffer server-proc))))))


(declare-function spiral--conn-pool-procs "spiral")
(defun spiral-project-quit (project)
  "Kill and remove PROJECT.
Removes all PROJECT's related buffers except the REPL buffer.  Quits all
processes in the connection pool."
  (interactive)
  (let* ((server-proc (spiral-project-socket-repl project))
         (server-buf (when server-proc (process-buffer server-proc)))
         (pool (spiral-project-conn-pool project)))
    ;; Kill the main Socket REPL, if any.
    (when server-proc
      (delete-process server-proc))
    (when server-buf
      (kill-buffer server-buf))
    ;; Kill the pool connection processes.
    (mapc (lambda (p-conn)
            (let* ((p-conn-proc (cdr p-conn))
                   (p-conn-buf (process-buffer p-conn-proc)))
              (when p-conn-proc
                (delete-process p-conn-proc))
              (when p-conn-buf
                (kill-buffer p-conn-buf))))
          pool)
    ;; Search for all buffers connected to this project and unbind their connection.
    (mapc (lambda (spiral-buf)
            (with-current-buffer spiral-buf
              (kill-local-variable 'spiral-conn-id)))
          (spiral-project-buffers project))
    ;; Remove the entry from `spiral-projects'
    (setq spiral-projects (map-delete spiral-projects (spiral-project-id project)))))


(defun spiral-project-id (proj)
  "Return the ID of the given PROJ."
  (map-elt proj :id))


(defun spiral-project-repr (proj)
  "Return a human focused string representation of PROJ."
  (let* ((dir (spiral-project-dir proj)))
    (if-let (name (when dir
                    (file-name-nondirectory (substring dir 0 -1))))
        (format "%s [%s]" name (spiral-project-id proj))
      (format "%s" (spiral-project-id proj)))))

(defun spiral-project-created (proj)
  "Return the created time for PROJ."
  (map-elt proj :created))


(defun spiral-project-port (proj)
  "Return the Socket REPL port for the given PROJ."
  (cdr (spiral-conn-host-port (spiral-project-id proj))))


(defun spiral-project-repl-buffer (proj)
  "Return the REPL buffer for the given PROJ."
  (map-elt proj :repl-buffer))


(defun spiral-project-host (proj)
  "Return the Socket REPL host for the given PROJ."
  (car (spiral-conn-host-port (spiral-project-id proj))))


(defun spiral-project-namespace (proj)
  "Return the current namespace used in PROJ."
  (map-elt proj :namespace))


(defun spiral-project-dir (proj)
  "Return the directory of the given PROJ."
  (map-elt proj :project-dir))


(defun spiral-project-socket-repl (proj)
  "Return a plist with the `:host' `:port' kv pairs for the PROJ's Socket REPL."
  (map-elt proj :socket-repl))


(defun spiral-project-classpath (proj)
  "Return the global `spiral-classpath' list appended to PROJ's classpath.
This function ensures that every path/file in the returned classpath exists
and its expanded."
  (mapcar
   #'file-truename
   (seq-filter
    (lambda (path) (when path (file-exists-p path)))
    (append (list (spiral-project-dir proj))
            (map-elt proj :classpath)
            (spiral-classpath)))))


(defun spiral-project-conn-pool (proj)
  "Return the PROJ's 'Connection Pool'."
  (map-elt proj :conn-pool))


(defun spiral-project-conn-pool-get-process (proj type)
  "Return the TYPE network process for the given PROJ."
  (map-elt (spiral-project-conn-pool proj) type))


(defun spiral-project-conn-pool-set-in (conn-id &rest kwargs)
  "Set new key-vals in CONN-ID's `:conn-pool', provided by KWARGS.
KWARGS is expected to be pairs of keywords and processes."
  (let* ((proj (spiral-projects-get conn-id))
         (conn-pool (spiral-project-conn-pool proj)))
    (mapc (lambda (pair)
            (map-put conn-pool (car pair) (cadr pair)))
          (seq-partition kwargs 2))
    (spiral-project-set-in conn-id :conn-pool conn-pool)))


(defun spiral-project-actions (project)
  "Return all `:actions' in PROJECT."
  (map-elt project :actions))


(defun spiral-project-print-settings (project)
  "Return `:print-settings' in PROJECT."
  (map-elt project :print-settings))


(defun spiral-project-actions-get (project action)
  "Return ACTION in PROJECT's `:actions'.
ACTION should be a key in the UNREPL session-actions map."
  (spiral-ast-map-elt (spiral-project-actions project) action))


(defun spiral-project-templated-action (project action &rest params)
  "Return a string created out of PROJECT's ACTION template, using PARAMS."
  (let ((template (spiral-project-actions-get project action))
        (template-params (mapcar (lambda (kv) (cons (car kv) (cadr kv)))
                                 (seq-partition params 2))))
    (spiral-command-template template template-params)))


(defun spiral-project-buffers (project)
  "Return a list of buffers that belong to this PROJECT's directory."
  (let ((conn-id (spiral-project-id project)))
    (seq-filter (lambda (b)
                  (with-current-buffer b
                    (and (bound-and-true-p spiral-conn-id)
                         (eql spiral-conn-id conn-id))))
                (buffer-list))))


(defun spiral-projects-as-list ()
  "Return all available projects as a list, sorted by creation date."
  (seq-sort (lambda (p1 p2)
              (time-less-p (spiral-project-created p2)
                           (spiral-project-created p1)))
            (map-values spiral-projects)))


(defun spiral-projects-add (proj)
  "Add PROJ to `spiral-projects'."
  (map-put spiral-projects (spiral-project-id proj) proj))


(defun spiral-projects-get (conn-id &optional raise-not-found)
  "Return the project with CONN-ID, or nil.
When RAISE-NOT-FOUND is nil, raises an `error' if CONN-ID is not found in
`spiral-projects'."
  (let ((proj (map-elt spiral-projects conn-id)))
    (when (and raise-not-found
               (not proj))
      (error "No project connected to %s can be found" conn-id))
    proj))


(defun spiral-projects-get-by-dir (project-dir)
  "Find a project in `spiral-projects' for PROJECT-DIR.
If more than one project matches with PROJECT-DIR, return the most recently
created.
Return matching project or nil"
  (seq-find (lambda (p) (string= (spiral-project-dir p) project-dir))
            (spiral-projects-as-list)))


(defun spiral-project-set-in (conn-id key val)
  "Set an attribute in the `spiral-projects' project with key CONN-ID.
KEY is expected to be a keyword, VAL is its corresponding value."
  (let ((proj (spiral-projects-get conn-id t)))
    (map-put proj key val)
    (map-put spiral-projects conn-id proj)
    spiral-projects))


(provide 'spiral-project)

;;; spiral-project.el ends here
