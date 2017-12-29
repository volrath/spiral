;;; spiral-stacktrace.el --- Stacktraces display -*- lexical-binding: t; -*-
;;
;; Filename: spiral-stacktrace.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov 16 01:09:49 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Stacktrace utilities.
;; Currently, only about rendering stacktrace.  More to come.
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

(require 'parseclj)

(require 'spiral-ast)
(require 'spiral-mode)


(defcustom spiral-stacktrace-print-length 50
  "Set the maximum length of frames in displayed trace data.

This sets the value of UNREPL session's `*print-length*` when pretty
printing the `ex-data` map for exception causes in the stacktrace that are
instances of `IExceptionInfo`.

Set to nil for no limit."
  :type '(choice integer (const nil))
  :group 'spiral)

(defcustom spiral-stacktrace-print-level 50
  "Set the maximum level of nesting in displayed cause data.

This sets the value of UNREPL session's `*print-level*` when pretty
printing the `ex-data` map for exception causes in the stacktrace that are
instances of `IExceptionInfo`.

Be advised that setting this to `nil` will cause the attempted printing of
cyclical data structures."
  :type '(choice integer (const nil))
  :group 'spiral)

(defvar spiral-stacktrace-detail-max nil
  "The maximum detail level for causes.  When nil, there's no limit.")

(defvar spiral-stacktrace--calm-down-indent
  (concat (propertize " "
                      'font-lock-face 'spiral-font-exception-prompt-face
                      'rear-nonsticky '(font-lock-face))
          " ")
  "This is mostly here to temporarily calm down aggressive-indent-mode.")


;; Faces
;; -------------------------------------------------------------------

(defface spiral-font-exception-title-face
  '((t (:inherit font-lock-warning-face)))
  "Face for an exception's title."
  :group 'spiral)

(defface spiral-font-stacktrace-cause-class-face
  '((t (:inherit compilation-error)))
  "Face for an exception's cause class."
  :group 'spiral)

(defface spiral-font-stacktrace-frame-file-face
  '((t (:inherit compilation-common-part)))
  "Face for a file name in a frame."
  :group 'spiral)

(defface spiral-font-stacktrace-frame-lineno-face
  '((t (:inherit compilation-mode-line-exit)))
  "Face for a line number in a frame."
  :group 'spiral)

(defface spiral-font-stacktrace-frame-where-face
  '((t (:inherit compilation-info)))
  "Face for a fn/class name in a frame."
  :group 'spiral)


;; Rendering
;; -------------------------------------------------------------------

(defun spiral-stacktrace--insert-title (phase)
  "Insert an exception title for PHASE."
  (insert
   (propertize
    (pcase phase
      (:eval "Unhandled Exception")
      (:read "UNREPL could not read this input")
      (:print "Expression computed successfully but UNREPL failed to print")
      (_ (format "Exception during %s phase" (spiral-keyword-name phase))))
    'font-lock-face 'spiral-font-exception-title-face))
  (spiral-repl-newline-and-scroll))


(declare-function spiral-repl-newline-and-scroll "spiral-repl")
(defun spiral-stacktrace--insert-cause (cause format-spacing)
  "Insert a prettified version of an exception CAUSE map.
FORMAT-SPACING is a number of char spaces to be left blank to the left of
the inserted text.  For more information, see
`spiral-stacktrace--insert-causes'."
  (insert spiral-stacktrace--calm-down-indent)
  ;; Insert the actual cause
  (let ((padded-format (concat "%" (format "%ds: " format-spacing)))
        (type (spiral-ast-map-elt cause :type))
        (cause-msg (spiral-ast-map-elt cause :message)))
    (insert (propertize (format padded-format (spiral-ast-unparse-to-string type))
                        'font-lock-face 'spiral-font-stacktrace-cause-class-face))
    (unless (spiral-ast-nil-p cause-msg)
      (spiral-ast-unparse-stdout-string cause-msg))
    (spiral-repl-newline-and-scroll)))


(defun spiral-stacktrace--insert-causes (via-node)
  "Insert causes for an exception provided by VIA-NODE.
VIA-NODE is an AST node of a vector, as provided by
`clojure.core/Throwable->map's `:via'."
  (let* ((causes (parseclj-ast-children via-node))
         (cause-type-length (lambda (c)
                              (thread-first c
                                (spiral-ast-map-elt :type)
                                (parseclj-ast-value)
                                (symbol-name)
                                (length))))
         (longest-type-length (apply #'max (mapcar cause-type-length
                                                   causes))))
    (mapc (lambda (cause)
            (spiral-stacktrace--insert-cause cause
                                             longest-type-length))
          (reverse causes))
    (spiral-repl-newline-and-scroll)))


(defun spiral-stacktrace--insert-trace (trace-node &optional paddings)
  "Insert a pretty representation of TRACE-NODE.
TRACE-NODE is an AST vector node as provided by
`clojure.core/Throwable->map's `:trace'.
PADDINGS is a cons tuple of two numbers, each representing the paddings to
be used for each trace entry's file name and line number"
  (let* ((frames (parseclj-ast-children trace-node))
         (get-file (lambda (frame)
                     (thread-first frame
                       (parseclj-ast-children)
                       (cl-caddr)
                       (parseclj-ast-value))))
         (get-lineno (lambda (frame)
                       (thread-first frame
                         (parseclj-ast-children)
                         (cl-cadddr)
                         (parseclj-ast-value))))
         (get-where (lambda (frame)
                      (format "%s/%s"
                              (parseclj-ast-value (car (parseclj-ast-children frame)))
                              (parseclj-ast-value (cadr (parseclj-ast-children frame))))))
         (get-paddings (lambda (trace-list)
                         (seq-reduce (lambda (acc te)
                                       (if (eql (parseclj-ast-node-type te) :tag)
                                           acc
                                         (cons (max (car acc) (length (funcall get-file te)))
                                               (max (cdr acc) (length (number-to-string
                                                                       (funcall get-lineno te)))))))
                                     trace-list
                                     '(0 . 0))))
         (paddings (or paddings (funcall get-paddings frames)))
         (file-format (format "%%%ds: " (car paddings)))
         (lineno-format (format "%%%dd " (cdr paddings))))
    (mapc (lambda (frame)
            (if (spiral-ast-elision-p frame)
                (spiral-ast-elision-tag-unparse
                 frame nil nil
                 (lambda (eval-payload &rest _args)
                   (let ((inhibit-read-only t)
                         (buffer-read-only nil))
                     (spiral-stacktrace--insert-trace eval-payload paddings))))
              (if (eql (parseclj-ast-node-type frame) :vector)  ;; clojure 1.9.0
                  (insert
                   spiral-stacktrace--calm-down-indent
                   (concat
                    (propertize (format file-format (funcall get-file frame))
                                'font-lock-face 'spiral-font-stacktrace-frame-file-face)
                    (propertize (format lineno-format (funcall get-lineno frame))
                                'font-lock-face 'spiral-font-stacktrace-frame-lineno-face)
                    (propertize (format "- %s" (funcall get-where frame))
                                'font-lock-face 'spiral-font-stacktrace-frame-where-face)))
                (spiral-ast-unparse frame))  ;; clojure 1.8.0
              (spiral-repl-newline-and-scroll)))
          frames)))


(defun spiral-stacktrace--insert-trace-button (trace-node)
  "Insert a button for displaying a trace with TRACE-NODE."
  (spiral-button-throwaway-insert "[Show Trace]"
                                  (lambda (_button)
                                    (spiral-stacktrace--insert-trace trace-node)))
  (spiral-repl-newline-and-scroll 2))



(defun spiral-stacktrace-insert-error (error-tag-node &optional show-trace)
  "Insert a pretty rendering of an ERROR-TAG-NODE.
This function is deliberately not used as a tag reader in order to be able
to only use it on demand, so that error tags that come from the REPL are
displayed literally by default.

ERROR-TAG-NODE is an AST node.  SHOW-TRACE is a boolean flag that indicates
whether to automatically show the error's stack `:trace'.  When nil, it's
hidden and a 'Show Trace' button is inserted in its place."
  (let* ((ex-data (spiral-ast-tag-child error-tag-node))
         (ex-via (spiral-ast-map-elt ex-data :via))
         (ex-trace (spiral-ast-map-elt ex-data :trace)))
    (spiral-stacktrace--insert-causes ex-via)
    (if show-trace
        (spiral-stacktrace--insert-trace ex-trace)
      (spiral-stacktrace--insert-trace-button ex-trace))))


(defun spiral-stacktrace-insert (ex-message-node &optional show-trace)
  "Insert a pretty rendering of EX-MESSAGE-NODE data.
EX-MESSAGE-NODE is an AST node as provided by UNREPL's `:exception'
message.

SHOW-TRACE is a boolean flag that indicates whether to automatically show
the exception stack trace.  When nil, it's hidden and a 'Show Trace' button
will be inserted in its place."
  (let* ((ex-phase (thread-first ex-message-node
                     (spiral-ast-map-elt :phase)
                     (parseclj-ast-value)))
         (error-tag (thread-first ex-message-node
                      (spiral-ast-map-elt :ex))))
    (let ((electric-indent-inhibit t))
      (spiral-stacktrace--insert-title ex-phase)
      (spiral-stacktrace-insert-error error-tag show-trace))))


(defun spiral-stacktrace-popup (conn-id ex-message-node)
  "Create a pop to a temporary buffer to show stacktrace in EX-MESSAGE-NODE.
CONN-ID is a connection id to be associated to the error buffer, so that
elisions how to expand.
EX-MESSAGE-NODE is an AST node as provided by UNREPL's `:exception'
message."
  (let ((err-buffer-name "*clojure-error*"))
    (when (get-buffer err-buffer-name)
      (kill-buffer err-buffer-name))
    (let ((err-buffer (get-buffer-create err-buffer-name)))
      (with-current-buffer err-buffer
        (spiral-stacktrace-mode)
        (setq-local spiral-conn-id conn-id)
        (let ((inhibit-read-only t))
          (spiral-stacktrace-insert ex-message-node 'show-trace))
        (pop-to-buffer err-buffer)))))


(define-derived-mode spiral-stacktrace-mode special-mode "Stacktrace"
  "Major mode for navigating SPIRAL stacktraces.

\\{spiral-stacktrace-mode-map}"
  (setq-local electric-indent-chars nil))


(provide 'spiral-stacktrace)

;;; spiral-stacktrace.el ends here
