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
  '((clojure/var . unrepl-ast--var-tag-unparse)
    (unrepl/... . unrepl-ast--elision-tag-unparse)
    (unrepl/object . unrepl-ast--object-tag-unparse)
    (unrepl/string . unrepl-ast--string-tag-unparse)
    (unrepl.java/class . unrepl-ast--class-tag-unparse))
  "A set of tag readers for common UNREPL tagged literals.")


(defun unrepl-ast--tag-child (tag-node)
  "Return the child node of TAG-NODE."
  (-> tag-node (parseclj-ast-children) (car)))


(defun unrepl-ast--insert-elision-button (get-more-action eval-callback &optional kill-from kill-to)
  "Insert an elision button to send GET-MORE-ACTION to UNREPL through `:aux'.
A callback function will be created for said `:aux' evaluation, and it will
internally call EVAL-CALLBACK with the evaluation payload.  This callback
will also automatically kill the region where the button is, which should
be defined by KILL-FROM and KILL-TO.
KILL-FROM default value is =(point)=.  KILL-TO is the end of the new
created button.
EVAL-CALLBACK can safely assume that the cursor will be at KILL-FROM, and
it is responsible to decide where to leave the cursor when the contents of
the evaluation payload have been inserted."
  (let* ((kill-from-marker (make-marker))
         (kill-to-marker (make-marker))
         (eval-callback (lambda (eval-payload)
                          (with-current-buffer (marker-buffer kill-from-marker)
                            ;; Kill the button region
                            (goto-char kill-from-marker)
                            (delete-region kill-from-marker kill-to-marker)
                            ;; Run the actual callback
                            (funcall eval-callback eval-payload))))
         (button-action (lambda (_button)
                          (unrepl-aux-send get-more-action eval-callback))))
    (set-marker kill-from-marker (or kill-from (point)))
    (insert " ")
    (insert-text-button unrepl-elision-label
                        'follow-link t
                        'action button-action
                        'help-echo "mouse-1, RET: Expand")
    (set-marker kill-to-marker (or kill-to (point)))))


(defun unrepl-ast--elision-tag-unparse (elision-tag-node &optional with-delimiters)
  "Insert a generic elision button for ELISION-TAG-NODE.
WITH-DELIMITERS is a boolean value that indicate whether to keep or remove
the opening and closing tokens for the retrieved
collection (i.e. opening/closing parens for lists, opening/closing brackets
for vectors etc.)"
  (let ((elision-actions (unrepl-ast--tag-child elision-tag-node)))
    (pcase (parseclj-ast-node-type elision-actions)
      (:nil (delete-char -1))  ;; delete the trailing space leading to this node.
      (:map (let ((get-more-action (-> elision-actions
                                       (unrepl-ast-map-elt :get)
                                       (unrepl-command-template))))
              (unrepl-ast--insert-elision-button
               get-more-action
               (lambda (eval-payload)
                 (unrepl-ast-unparse eval-payload (not with-delimiters))))))
      (_ (when unrepl-debug
           (error "Unrecognized elision tagged form %S" elision-tag-node))))))


(defun unrepl-ast--object-tag-unparse (object-tag-node)
  "Insert a string representation of OBJECT-TAG-NODE.
OBJECT-TAG-NODE's child is a vector that has 4 elements, these are:
- Class name, as a #unrepl.java/class tagged literal.
- Identity hash code, as a string node.
- Object representation, as a symbol node, a string node, or another tagged
  literal.
- A map node with extra information.

By default, this function will create tooltips for the object
representation with additional information.  DISABLE-UI overrides this
behavior."
  (let* ((obj-attrs (parseclj-ast-children (unrepl-ast--tag-child object-tag-node)))
         (class-name (unrepl-ast-unparse-to-string (car obj-attrs)))
         (id-hash (parseclj-ast-value (cadr obj-attrs)))
         (object-rep-node (cl-caddr obj-attrs))
         (create-object-repr (lambda (&optional obj-rep)
                               (format "#object[%s %S%s]"
                                       class-name id-hash
                                       (if obj-rep
                                           (format " %s" obj-rep)
                                         "")))))
    (if (eql (parseclj-ast-node-type object-rep-node) :symbol)
        ;; Representation for a "demunged" function.
        (progn
          (insert "#function[")
          (unrepl-ast-unparse object-rep-node)
          (insert " "
                  (propertize id-hash
                              'font-lock-face 'unrepl-font-tooltip-face
                              'help-echo (funcall create-object-repr))
                  "]"))
      ;; Regular object representation
      (insert
       (funcall create-object-repr (unrepl-ast-unparse-to-string object-rep-node))))))


(defun unrepl-ast--string-tag-unparse (string-tag-node &optional stdout-str)
  "Insert a string representation of STRING-TAG-NODE.
STDOUT-STR is a boolean value that indicates whether or not the
STRING-TAG-NODE represents a stdout (`:out') message from UNREPL.  In case
that STDOUT-STR is non-nil, surronding quotes for this string will be
ommited."
  (let* ((insert-fn (if stdout-str
                        #'unrepl-ast-unparse-stdout-string
                      #'unrepl-ast-unparse))
         (string-tag-vector-elems (-> string-tag-node
                                      (unrepl-ast--tag-child)
                                      (parseclj-ast-children)))
         (string-node (car string-tag-vector-elems))
         (elision-actions (unrepl-ast--tag-child (cadr string-tag-vector-elems))))
    ;; insert string
    (funcall insert-fn string-node)
    ;; and then elision button
    (unrepl-ast--insert-elision-button
     (-> elision-actions
         (unrepl-ast-map-elt :get)
         (unrepl-command-template))
     (lambda (eval-payload)
       (let ((p (point)))
         (funcall insert-fn eval-payload)
         (unless stdout-str  ;; Delete the opening quote
           (delete-region p (1+ p)))))
     (unless stdout-str (1- (point))))))


(defun unrepl-ast--var-tag-unparse (var-tag-node)
  "Insert a string representation of VAR-TAG-NODE."
  (unrepl-ast--generic-tag-child-unparse var-tag-node))


(defun unrepl-ast--class-tag-unparse (class-tag-node)
  "Insert a string representation of a java class CLASS-TAG-NODE."
  (unrepl-propertize-region '(font-lock-face unrepl-font-class-name-face)
    (unrepl-ast--generic-tag-child-unparse class-tag-node)))


(defun unrepl-ast--generic-tag-child-unparse (tag-node)
  "Skip TAG-NODE's name and unparse its child."
  (-> tag-node
      (unrepl-ast--tag-child)
      (unrepl-ast-unparse)))


(defun unrepl-ast--generic-tag-unparse (tag-node)
  "Insert a string representation of the given AST TAG-NODE into buffer."
  (insert "#" (symbol-name (parseclj-ast-node-attr tag-node :tag)) " ")
  (unrepl-ast--generic-tag-child-unparse tag-node))


(defun unrepl-ast--unparse-collection (node no-delimiters)
  "Insert a string representation of the given AST branch NODE into buffer.
When NO-DELIMITERS is non-nil, avoid inserting delimiters for this
collection."
  (let* ((token-type (parseclj-ast-node-type node))
         (delimiters (cl-case token-type
                       (:root (cons "" ""))
                       (:list (cons "(" ")"))
                       (:vector (cons "[" "]"))
                       (:set (cons "#{" "}"))
                       (:map (cons "{" "}")))))
    (unless no-delimiters (insert (car delimiters)))
    (let ((nodes (parseclj-ast-children node)))
      (when-let (node (car nodes))
        (unrepl-ast-unparse node))
      (seq-doseq (child (cdr nodes))
        (when (null (parseclj-ast-node-attr node :lexical-preservation))  ;; hack, read below.
          (insert " "))
        (unrepl-ast-unparse child)))
    (unless no-delimiters (insert (cdr delimiters)))))
;; Here we're basically accessing parseclj's internal API to check if a node has
;; lexical preservation.  It would be better if parseclj would expose a function
;; for this, something like `parseclj-ast-node-lexical-preservation-p'.  Another
;; possible solution would be to have `parseclj-unparse-clojure' take tag
;; readers as an argument, but I'm not really sure that would be part of its
;; scope.


(defun unrepl-ast-unparse (ast-node &optional no-delimiters raise-on-missing-tags)
  "Unparse a parseclj AST-NODE into a human-friendly representation.
For the most parts, this function behaves exactly as
`parseclj-unparse-clojure-to-string', but it will also consume all tagged
literals in AST-NODE using `unrepl-ast-tag-readers' to generate a elisp
code for better UI.
NO-DELIMITERS is a boolean value that indicate whether to delete/remove
the opening and closing tokens for the retrieved
collection (i.e. opening/closing parens for lists, opening/closing brackets
for vectors etc.)
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
      (unrepl-ast--unparse-collection ast-node no-delimiters))))


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
    (unrepl-ast--string-tag-unparse string-node t)))


;; Template Management
;; -------------------------------------------------------------------

(defun unrepl-ast--generate-replace-param-tags-fn (params)
  "Create a closure fn for replacing param tag nodes with PARAMS.
Return a function that receives a param tag AST node, gets its keyword
children (the actual param name) and compares against the PARAMS alist to
see if there's a valid replacement for it."
  (lambda (param-tag-node)
    (let* ((param-keyword (-> param-tag-node
                              (unrepl-ast--tag-child)
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
        (error "Parameter %S not set in %S" param-keyword params)))))


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


(defun unrepl-command-template (template-ast &optional params)
  "Process TEMPLATE-AST with PARAMS and return a string.
PARAMS should be an alist of tagged literal symbols and readers."
  (let ((cmd (unrepl-ast--replace-param-tags template-ast params)))
    (format "%s\n"
            (parseclj-unparse-clojure-to-string cmd))))


(provide 'unrepl-ast)

;;; unrepl-ast.el ends here
