;;; unrepl-util.el --- Common utility functions -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-util.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Thu Nov 16 01:09:49 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Common utility functions
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
(require 'parseclj)
(require 'treepy)


(defun unrepl-conn-host-port (conn-id)
  "Return a tuple of host<string>, port<integer> from CONN-ID."
  (let* ((s-host-port (split-string (symbol-name conn-id) ":"))
         (host (car s-host-port))
         (port (string-to-number (cadr s-host-port))))
    (cons host port)))


(defun unrepl-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead.

BORROWED FROM CIDER."
  (apply (if bounds #'list #'buffer-substring-no-properties)
         (save-excursion
           (clojure-backward-logical-sexp 1)
           (list (point)
                 (progn (clojure-forward-logical-sexp 1)
                        (skip-chars-forward "[:blank:]")
                        (when (looking-at-p "\n") (forward-char 1))
                        (point))))))


;; AST Utilities
;; -------------------------------------------------------------------

(defun unrepl-ast-zip (ast-node)
  "Create a treepy zipper for AST-NODE."
  (treepy-zipper #'parseclj-ast-branch-node-p
                 #'parseclj-ast-children
                 #'parseclj-ast-node
                 ast-node))

(defun unrepl-ast-map-elt (map-node key)
  "Traverse MAP-NODE in look for KEY, and return corresponding value.
Value is returned as an AST node."
  (cadr (seq-find (lambda (kv-pair)
                    (eql key (parseclj-ast-value (car kv-pair))))
                  (-partition 2 (parseclj-ast-children map-node)))))


;; template management

(defun unrepl-ast--generate-replace-param-tags-fn (params)
  "Create a closure fn for replacing param tag nodes with PARAMS.
Return a function that receives a param tag AST node, gets its keyword
children (the actual param name) and compares against the PARAMS alist to
see if there's a valid replacement for it."
  (lambda (param-tag-node)
    (let* ((param-keyword (-> param-tag-node
                              (parseclj-ast-children)
                              (car)
                              (parseclj-ast-value)))
           (replacement (seq-find (lambda (p-kv)
                                    (eql (car p-kv) param-keyword))
                                  params)))
      (if replacement
          ;; TODO: transform this `replacement' to an AST node.  Initially
          ;; `replacement' will be an elisp value, like an integer, a string, a
          ;; seq, or whatever.  This should be turned into a node, because it
          ;; will be later passed to `parseclj-unparse-clojure-to-string', which
          ;; only knows how to traverse/convert AST nodes into strings.
          (error "Not implemented! check code for details")
        param-tag-node))))


(defun unrepl-ast--replace-param-tags (root params)
  "Traverse ROOT and replace 'param tag' nodes with PARAMS.
For each param tag, PARAMS alist is checked to see if there's a
corresponding replacement.
Return ROOT with all available param tags replaced."
  (let ((loc (unrepl-ast-zip root)))
    (if (treepy-end-p loc)
        root
      (let ((replace-param-tag-fn (unrepl-ast--generate-replace-param-tags-fn params)))
        (while (not (treepy-end-p (treepy-next loc)))
          (let ((node (treepy-node loc)))
            (setq loc (treepy-next
                       (if (eql (parseclj-ast-node-attr node :tag) 'unrepl/param)
                           (treepy-edit loc replace-param-tag-fn)
                         loc)))))
        (treepy-root loc)))))


(defun unrepl-command-template (template-ast &rest params)
  "Process TEMPLATE-AST with PARAMS and return a string."
  (let ((cmd (unrepl-ast--replace-param-tags template-ast params)))
    (format "%s\n"
            (parseclj-unparse-clojure-to-string cmd))))


;; Debugging
;; -------------------------------------------------------------------

(defmacro comment (&rest _body)
  "A wannabe 'clojure-like' comment macro."
  nil)


(comment  ;; For debugging purposes.
 (defun unrepl-retry ()
   "Reload everything and connect again."
   (interactive)
   (let ((base-dir (file-name-directory buffer-file-name)))
     (add-to-list 'load-path base-dir)
     (add-to-list 'load-path (expand-file-name "parseclj" base-dir))
     (load (expand-file-name "unrepl-util.el" base-dir))
     (load (expand-file-name "unrepl-project.el" base-dir))
     (load (expand-file-name "unrepl-mode.el" base-dir))
     (load (expand-file-name "unrepl-loop.el" base-dir))
     (load (expand-file-name "unrepl-repl.el" base-dir))
     (load (expand-file-name "unrepl.el" base-dir)))
   (unrepl-project-quit '127.0.0.1:5555)
   (unrepl-connect-to "localhost" 5555))
 (global-set-key (kbd "C-c C-y") #'unrepl-retry)
 )


(provide 'unrepl-util)

;;; unrepl-util.el ends here
