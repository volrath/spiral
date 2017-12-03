;;; unrepl-stacktrace.el --- Stacktraces display -*- lexical-binding: t; -*-
;; 
;; Filename: unrepl-stacktrace.el
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

(require 'unrepl-ast)
(require 'unrepl-mode)


(defcustom unrepl-stacktrace-print-length 50
  "Set the maximum length of sequences in displayed trace data.

This sets the value of UNREPL session's `*print-length*` when pretty
printing the `ex-data` map for exception causes in the stacktrace that are
instances of `IExceptionInfo`.

Set to nil for no limit."
  :type '(choice integer (const nil))
  :group 'unrepl)

(defcustom unrepl-stacktrace-print-level 50
  "Set the maximum level of nesting in displayed cause data.

This sets the value of UNREPL session's `*print-level*` when pretty
printing the `ex-data` map for exception causes in the stacktrace that are
instances of `IExceptionInfo`.

Be advised that setting this to `nil` will cause the attempted printing of
cyclical data structures."
  :type '(choice integer (const nil))
  :group 'unrepl)

(defvar unrepl-stacktrace-detail-max 2
  "The maximum detail level for causes.")

(defvar unrepl-stacktrace--calm-down-indent
  (concat (propertize " "
                      'font-lock-face 'unrepl-font-exception-prompt-face
                      'rear-nonsticky '(font-lock-face))
          " ")
  "This is mostly here to temporarily calm down aggressive-indent-mode.")


;; Faces
;; -------------------------------------------------------------------

(defface unrepl-font-exception-title-face
  '((t (:inherit font-lock-warning-face)))
  "Face for an exception's title."
  :group 'unrepl)

(defface unrepl-font-stacktrace-cause-class-face
  '((t (:inherit compilation-error)))
  "Face for an exception's cause class."
  :group 'unrepl)

(defface unrepl-font-stacktrace-frame-file-face
  '((t (:inherit compilation-common-part)))
  "Face for a file name in a frame."
  :group 'unrepl)

(defface unrepl-font-stacktrace-frame-lineno-face
  '((t (:inherit compilation-mode-line-exit)))
  "Face for a line number in a frame."
  :group 'unrepl)

(defface unrepl-font-stacktrace-frame-where-face
  '((t (:inherit compilation-info)))
  "Face for a fn/class name in a frame."
  :group 'unrepl)


;; Rendering
;; -------------------------------------------------------------------

(defun unrepl-stacktrace--insert-title (phase)
  "Insert an exception title for PHASE."
  (insert
   (propertize
    (pcase phase
      (:eval "Unhandled Exception")
      (:read "UNREPL could not read this input")
      (:print "Expression computed successfully but UNREPL failed to print")
      (_ (format "Exception during %s phase" (unrepl-keyword-name phase))))
    'font-lock-face 'unrepl-font-exception-title-face)
   "\n"))


(defun unrepl-stacktrace--insert-cause (cause format-spacing)
  "Insert a prettified version of an exception CAUSE map.
FORMAT-SPACING is a number of char spaces to be left blank to the left of
the inserted text.  For more information, see
`unrepl-stacktrace--insert-causes'."
  (insert unrepl-stacktrace--calm-down-indent)
  ;; Insert the actual cause
  (let ((padded-format (concat "%" (format "%ds: " format-spacing)))
        (type (parseclj-ast-value (unrepl-ast-map-elt cause :type)))
        (cause-msg (unrepl-ast-map-elt cause :message)))
    (insert (propertize (format padded-format type)
                        'font-lock-face 'unrepl-font-stacktrace-cause-class-face))
    (unrepl-ast-unparse-stdout-string cause-msg)
    (insert "\n")))


(defun unrepl-stacktrace--insert-causes (via-node)
  "Insert causes for an exception provided by VIA-NODE.
VIA-NODE is an AST node of a vector, as provided by
`clojure.core/Throwable->map's `:via'."
  (let* ((causes (parseclj-ast-children via-node))
         (cause-type-length (lambda (c)
                              (-> c
                                  (unrepl-ast-map-elt :type)
                                  (parseclj-ast-value)
                                  (symbol-name)
                                  (length))))
         (longest-type-length (apply #'max (mapcar cause-type-length
                                                   causes))))
    (mapc (lambda (cause)
            (unrepl-stacktrace--insert-cause cause
                                             longest-type-length))
          (reverse causes))
    (insert "\n")))


(defun unrepl-stacktrace--insert-trace (trace-node &optional paddings)
  "Insert a pretty representation of TRACE-NODE.
TRACE-NODE is an AST vector node as provided by
`clojure.core/Throwable->map's `:trace'.
PADDINGS is a cons tuple of two numbers, each representing the paddings to
be used for each trace entry's file name and line number"
  (let* ((frames (parseclj-ast-children trace-node))
         (get-file (lambda (frame)
                     (-> frame
                         (parseclj-ast-children)
                         (cl-caddr)
                         (parseclj-ast-value))))
         (get-lineno (lambda (frame)
                       (-> frame
                           (parseclj-ast-children)
                           (cl-cadddr)
                           (parseclj-ast-value))))
         (get-where (lambda (frame)
                      (format "%s/%s"
                              (parseclj-ast-value (car (parseclj-ast-children frame)))
                              (parseclj-ast-value (cadr (parseclj-ast-children frame))))))
         (get-paddings (lambda (trace-list)
                         (-reduce-from (lambda (acc te)
                                         (if (eql (parseclj-ast-node-type te) :tag)
                                             acc
                                           (cons (max (car acc) (length (funcall get-file te)))
                                                 (max (cdr acc) (length (number-to-string
                                                                         (funcall get-lineno te)))))))
                                       '(0 . 0)
                                       trace-list)))
         (paddings (or paddings (funcall get-paddings frames)))
         (file-format (format "%%%ds: " (car paddings)))
         (lineno-format (format "%%%dd " (cdr paddings))))
    (mapc (lambda (frame)
            (if (eql (parseclj-ast-node-type frame) :tag)
                (unrepl-ast-elision-tag-unparse
                 frame nil nil
                 (lambda (eval-payload &rest _args)
                   (unrepl-stacktrace--insert-trace eval-payload paddings)))
              (insert
               unrepl-stacktrace--calm-down-indent
               (concat
                (propertize (format file-format (funcall get-file frame))
                            'font-lock-face 'unrepl-font-stacktrace-file-face)
                (propertize (format lineno-format (funcall get-lineno frame))
                            'font-lock-face 'unrepl-font-stacktrace-lineno-face)
                (propertize (format "- %s" (funcall get-where frame))
                            'font-lock-face 'unrepl-font-stacktrace-where-face)
                "\n"))))
          frames)))


(defun unrepl-stacktrace--insert-trace-button (trace-node)
  "Insert a button for displaying a trace with TRACE-NODE."
  (unrepl-button-throwaway-insert "[Show Trace]"
                                  (lambda (_button)
                                    (unrepl-stacktrace--insert-trace trace-node)))
  (insert "\n\n"))


(defun unrepl-stacktrace-insert (ex-message-node &optional show-trace)
  "Insert a pretty rendering of EX-MESSAGE-NODE data.
EX-MESSAGE-NODE is a an AST node as provided by UNREPL's `:exception'
message.

SHOW-TRACE is a boolean flag that indicates whether to automatically
show the exception stack trace.  When nil, it's hidden and a 'Show
Stacktrace' button will be inserted in its place."
  (let* ((ex-phase (-> ex-message-node
                       (unrepl-ast-map-elt :phase)
                       (parseclj-ast-value)))
         (ex-data (-> ex-message-node
                      (unrepl-ast-map-elt :ex)
                      (unrepl-ast-tag-child)))
         (ex-via (unrepl-ast-map-elt ex-data :via))
         (ex-trace (unrepl-ast-map-elt ex-data :trace)))
    (let ((electric-indent-inhibit t))
      (save-excursion
        (unrepl-stacktrace--insert-title ex-phase)
        (unrepl-stacktrace--insert-causes ex-via)
        (if show-trace
            (unrepl-stacktrace--insert-trace ex-trace)
          (unrepl-stacktrace--insert-trace-button ex-trace)))
      (when (and (get-buffer-window (current-buffer))
                 (eql (current-buffer) (window-buffer (selected-window))))
        (recenter -1)))))

(provide 'unrepl-stacktrace)

;;; unrepl-stacktrace.el ends here
