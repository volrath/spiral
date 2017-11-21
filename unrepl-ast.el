;;; unrepl-ast.el --- AST related helpers -*- lexical-binding: t; -*-
;; 
;; Filename: unrepl-ast.el
;; Author: Daniel Barreto <daniel@barreto.tech>
;; Maintainer: Daniel Barreto <daniel@barreto.tech>
;; Copyright (C) 2017 Daniel Barreto
;; Created: Mon Nov 20 12:33:28 2017 (+0100)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;; A set of utility helpers for parseclj AST manipulation.
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

(require 'dash)
(require 'parseclj)
(require 'treepy)

(require 'unrepl-util)


;; Utilities
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


;; Unparsing
;; -------------------------------------------------------------------

(declare-function unrepl-aux-send "unrepl-loop")

(defvar unrepl-elision-label "..."
  "Label used to represent elisions.")

(defvar unrepl-ast-tag-readers
  '((unrepl/string . unrepl-ast--string-tag-unparse))
  "A set of tag readers for common UNREPL tagged literals.")


(defun unrepl-ast--elision-tag-map (elision-tag)
  "Return template map associated with ELISION-TAG."
  (-> elision-tag
      (parseclj-ast-children)
      (car)))


(defun unrepl-ast--string-tag-unparse (string-tag-node &optional disallow-ui stdout-str)
  "Insert a string representation of STRING-TAG-NODE.
By default, elisions will be represented with buttons from the `button'
package.  If DISALLOW-UI is non-nil, a simple '...' will be used instead.
STDOUT-STR is a boolean value that indicates whether or not the
STRING-TAG-NODE represents a stdout (`:out') message from UNREPL.  In case
that STDOUT-STR is non-nil, surronding quotes for this string will be
ommited."
  (let* ((insert-fn (if stdout-str
                        #'unrepl-ast-unparse-stdout-string
                      #'unrepl-ast-unparse))
         (string-tag-vector-elems (-> string-tag-node
                                      (parseclj-ast-children)
                                      (car)
                                      (parseclj-ast-children)))
         (string-node (car string-tag-vector-elems))
         (elision-actions (unrepl-ast--elision-tag-map
                           (cadr string-tag-vector-elems))))
    (funcall insert-fn string-node)
    (insert " ")
    (if disallow-ui
        (insert unrepl-elision-label)
      (let* ((elision-get-templ (unrepl-ast-map-elt elision-actions :get))
             (button-action (lambda (button)
                              (let ((bstart-marker (make-marker)))
                                (set-marker bstart-marker (button-start button))
                                (unrepl-aux-send (unrepl-command-template elision-get-templ)
                                                 (lambda (eval-payload)
                                                   (with-current-buffer (marker-buffer bstart-marker)
                                                     (save-excursion
                                                       (goto-char bstart-marker)
                                                       (backward-char (if stdout-str 1 2))
                                                       (kill-line)
                                                       (funcall insert-fn eval-payload)
                                                       (goto-char bstart-marker)
                                                       (unless stdout-str (delete-char 1))))))))))
        (insert-button unrepl-elision-label
                       'action button-action
                       'mouse-action button-action)))))


(defun unrepl-ast--generic-tag-unparse (node)
  "Insert a string representation of the given AST tag NODE into buffer."
  (insert "#")
  (insert (symbol-name (parseclj-ast-node-attr node :tag)))
  (insert " ")
  (unrepl-ast-unparse (car (parseclj-ast-children node))))


(defun unrepl-ast--unparse-collection (node)
  "Insert a string representation of the given AST branch NODE into buffer."
  (let* ((token-type (parseclj-ast-node-type node))
         (delimiters (cl-case token-type
                       (:root (cons "" ""))
                       (:list (cons "(" ")"))
                       (:vector (cons "[" "]"))
                       (:set (cons "#{" "}"))
                       (:map (cons "{" "}")))))
    (insert (car delimiters))
    (let ((nodes (parseclj-ast-children node)))
      (when-let (node (car nodes))
        (unrepl-ast-unparse node))
      (seq-doseq (child (cdr nodes))
        (when (null (parseclj-ast-node-attr node :lexical-preservation))  ;; hack, read below.
          (insert " "))
        (unrepl-ast-unparse child)))
    (insert (cdr delimiters))))
;; Here we're basically accessing parseclj's internal API to check if a node has
;; lexical preservation.  It would be better if parseclj would expose a function
;; for this, something like `parseclj-ast-node-lexical-preservation-p'.  Another
;; possible solution would be to have `parseclj-unparse-clojure' take tag
;; readers as an argument, but I'm not really sure that would be part of its
;; scope.


(defun unrepl-ast-unparse (ast-node &optional raise-on-missing-tags)
  "Unparse a parseclj AST-NODE into a human-friendly representation.
For the most parts, this function behaves exactly as
`parseclj-unparse-clojure-to-string', but it will also consume all tagged
literals in AST-NODE using `unrepl-ast-tag-readers' to generate a elisp
code for better UI.
If AST-NODE contains a tagged literal not represented in TAG-READERS-MAP,
an error will be raised.  RAISE-ON-MISSING-TAGS overrides this behavior.
Return a string that may or may not be propertized, and may or may not
include other UI elements."
  (if (parseclj-ast-leaf-node-p ast-node)
      (insert (parseclj-ast-node-attr ast-node :form))
    (if (eql (parseclj-ast-node-type ast-node) :tag)
        (let* ((tag-symbol (parseclj-ast-node-attr ast-node :tag))
               (reader (map-elt unrepl-ast-tag-readers tag-symbol)))
          (if reader
              (funcall reader ast-node)
            (if raise-on-missing-tags
                (error "Missing tag %s" tag-symbol)
              (unrepl-ast--generic-tag-unparse ast-node))))
      (unrepl-ast--unparse-collection ast-node))))


(defun unrepl-ast-unparse-to-string (ast-node &optional raise-on-missing-tags)
  "Unparse a parseclj AST-NODE to a string representation.
This function works like `unrepl-ast-unparse' but avoids adding UI
elements (like buttons) to the returned string.
If AST-NODE contains tagged literals not represented in TAG-READERS-MAP, an
error will be raised.  RAISE-ON-MISSING-TAGS overrides this behavior.
The returned string will be automatically font-locked as clojure code."
  (with-current-buffer (unrepl--make-buffer-for-mode 'clojure-mode)
    ;; TODO: add a flag to mute UI elements
    (erase-buffer)
    (unrepl-ast-unparse ast-node raise-on-missing-tags)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))


(defun unrepl-ast-unparse-stdout-string (string-node)
  "Unparse STRING-NODE, which can be a simple string or a #unrepl/string tag."
  (if (eql (parseclj-ast-node-type string-node) :string)
      (-> string-node
          (parseclj-ast-value)
          (propertize 'font-lock-face 'unrepl-font-stdout-face)
          (insert))
    (unrepl-ast--string-tag-unparse string-node nil t)))


;; Template Management
;; -------------------------------------------------------------------

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


(provide 'unrepl-ast)

;;; unrepl-ast.el ends here