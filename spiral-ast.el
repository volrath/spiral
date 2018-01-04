;;; spiral-ast.el --- AST related helpers -*- lexical-binding: t; -*-
;;
;; Filename: spiral-ast.el
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

(require 'parseclj)
(require 'subr-x)
(require 'treepy)

(require 'spiral-button)
(require 'spiral-util)
(require 'spiral-attachment)


(defcustom spiral-enable-pretty-object-representations t
  "Whether or not to enable pretty object representations."
  :type 'boolean
  :group 'spiral)


;; Utilities
;; -------------------------------------------------------------------

(defun spiral-ast--make-node (node children)
  "Helper parseclj function to create nodes 'a la treepy'.
NODE is an AST node.  CHILDREN is a list of AST nodes."
  (mapcar (lambda (pair)
            (if (eql (car pair) :children)
                (cons :children children)
              pair))
          node))


(defun spiral-ast-zip (ast-node)
  "Create a treepy zipper for AST-NODE."
  (treepy-zipper #'parseclj-ast-branch-node-p
                 #'parseclj-ast-children
                 #'spiral-ast--make-node
                 ast-node))


(defun spiral-ast-to-elisp (ast-node &optional tag-readers)
  "Transform AST-NODE into an elisp data structure.
TAG-READERS is an associative data structure that maps tag symbols to tag
readers.
This algorithm tries to follow the same logic `parseedn' uses, but will
ignore tagged elements and not raise on missing tag readers."
  (let ((node-type (parseclj-ast-node-type ast-node))
        (children (parseclj-ast-children ast-node))
        (to-elisp (lambda (n) (spiral-ast-to-elisp n tag-readers))))
    (cl-case node-type
      (:root (mapcar to-elisp children))
      (:list (mapcar to-elisp children))
      (:vector (apply #'vector (mapcar to-elisp children)))
      (:set (mapcar to-elisp children))
      (:map (mapcar (lambda (kv)
                      (cons (spiral-ast-to-elisp (car kv) tag-readers)
                            (spiral-ast-to-elisp (cadr kv) tag-readers)))
                    (seq-partition children 2)))
      (:tag (when-let (reader (map-elt tag-readers (parseclj-ast-node-attr ast-node :tag)))
              (funcall reader (spiral-ast-tag-child ast-node))))
      (t (parseclj-ast-value ast-node)))))


(defun spiral-ast-map-elt (map-node key)
  "Traverse MAP-NODE in look for KEY, and return corresponding value.
Value is returned as an AST node."
  (cadr (seq-find (lambda (kv-pair)
                    (eql key (parseclj-ast-value (car kv-pair))))
                  (seq-partition (parseclj-ast-children map-node) 2))))


(defun spiral-ast-nil-p (node)
  "Predicate to identify if NODE is a nil node."
  (eql (parseclj-ast-node-type node) :nil))


(defun spiral-ast-elision-p (node)
  "Predicate to identify if NODE is an elision."
  (and (eql (parseclj-ast-node-type node) :tag)
       (eql (parseclj-ast-node-attr node :tag) 'unrepl/...)
       (member (parseclj-ast-node-type (spiral-ast-tag-child node))
               '(:map :nil))))



;; Tag Readers
;; -------------------------------------------------------------------

(defvar spiral-ast-tag-readers
  `((clojure/var . spiral-ast--var-tag-unparse)
    (unrepl/... . spiral-ast-elision-tag-unparse)
    (unrepl/lazy-error . spiral-ast--lazy-error-tag-unparse)
    (unrepl/mime . spiral-ast--mime-tag-unparse)
    (unrepl/object . spiral-ast--object-tag-unparse)
    (unrepl/ratio . spiral-ast--ratio-tag-unparse)
    (unrepl/string . spiral-ast--string-tag-unparse)
    (unrepl.java/class . spiral-ast--class-tag-unparse))
  "A set of tag readers for common UNREPL tagged literals.")


(defun spiral-ast--find-tag-reader (tag-symbol mute-ui raise-on-missing-tags)
  "Find a tag reader for TAG-SYMBOL.
This function looks for TAG-SYMBOL in `spiral-ast-tag-readers'.
MUTE-UI is a flag that indicates whether to return the main reader or the
'no-ui' reader.  This flag get passed to each reader, so that it can decide
what to do with the information.
RAISE-ON-MISSING-TAGS is a flag that indicates whether to raise an error
when no readers are found for TAG-SYMBOL."
  (if-let (reader (map-elt spiral-ast-tag-readers tag-symbol))
      (lambda (tag)
        (funcall reader tag mute-ui))
    (when raise-on-missing-tags
      (error "Missing tag: %S" tag-symbol))))


(defun spiral-ast-elision-tag-unparse (elision-tag-node mute-ui
                                                        &optional with-delimiters unparse-fn)
  "Insert a generic elision button for ELISION-TAG-NODE.
MUTE-UI is a flag that indicates whether or not to insert UI elements like
buttons.
WITH-DELIMITERS is a boolean value that indicate whether to keep or remove
the opening and closing tokens for the retrieved
collection (i.e. opening/closing parens for lists, opening/closing brackets
for vectors etc.)
UNPARSE-FN is an optional function to unparse the resulting evaluation
payload from calling the elision action, if not given, uses
`spiral-ast-unparse'."
  (let ((elision-actions (spiral-ast-tag-child elision-tag-node)))
    (pcase (parseclj-ast-node-type elision-actions)
      (:nil (delete-char -1))  ;; delete the trailing space leading to this node.
      (:map (if mute-ui
                (insert (propertize "..." 'category 'spiral-elision))
              (let ((get-more-action (thread-first elision-actions
                                       (spiral-ast-map-elt :get)
                                       (spiral-command-template))))
                (spiral-button-aux-action-throwaway-insert
                 spiral-button-elision-label
                 get-more-action
                 (lambda (eval-payload)
                   (funcall (or unparse-fn #'spiral-ast-unparse)
                            eval-payload
                            (not with-delimiters))
                   ;; Look back and see if there's another elision button, and
                   ;; if so, we place the cursor over it.
                   (when (get-char-property (1- (point)) 'button)
                     (left-char)))))))
      (_ (when spiral-debug
           (error "Unrecognized elision tagged form %S" elision-tag-node))))))


(declare-function spiral-repl-move-to-next-prompt "spiral-repl")
(declare-function spiral-repl-newline-and-scroll "spiral-repl")
(declare-function spiral-stacktrace-insert-error "spiral-stacktrace")
(defun spiral-ast--error-resume (error-tag-node mute-ui)
  "Insert a brief description of ERROR-TAG-NODE, using its `:cause'.
This function also inserts a button for further error inspection, unless
MUTE-UI is non-nil."
  (thread-first error-tag-node
    (spiral-ast-tag-child)
    (spiral-ast-map-elt :cause)
    (spiral-ast-unparse))
  (if mute-ui
      (insert (thread-first " %s"
                (format spiral-button-elision-label)
                (propertize 'category 'spiral-elision)))
    (spiral-button-throwaway-insert
     "[Inspect]"
     (lambda (_button)
       (spiral-repl-move-to-next-prompt)
       (spiral-stacktrace-insert-error error-tag-node 'show-trace)
       (spiral-repl-newline-and-scroll)
       (forward-line -1)))))


(defun spiral-ast--lazy-error-tag-unparse (lazy-error-tag-node mute-ui)
  "Insert a short representation of a lazy error, from LAZY-ERROR-TAG-NODE.
MUTE-UI is a flag that indicates whether or not to insert UI elemnts like
buttons."
  (insert (propertize "~lazy-error" 'font-lock-face 'spiral-font-exception-title-face) " ")
  (spiral-ast--error-resume (spiral-ast-tag-child lazy-error-tag-node) mute-ui)
  (insert (propertize "~" 'font-lock-face 'spiral-font-exception-title-face)))


(defun spiral-ast--mime-tag-unparse (mime-tag-node mute-ui)
  "Insert a button to download any media in MIME-TAG-NODE.
If MUTE-UI is non-nil, don't do anything."
  (when (and spiral-handle-rich-media
             (not mute-ui))
    (let* ((mime-spec (spiral-ast-tag-child mime-tag-node))
           (content-type (parseclj-ast-value (spiral-ast-map-elt mime-spec :content-type)))
           (attachment-handler (spiral-attachment-find-handler content-type)))
      (when attachment-handler
        (spiral-attachment-insert-button attachment-handler
                                         (thread-first mime-spec
                                           (spiral-ast-map-elt :content)
                                           (spiral-ast-tag-child)
                                           (spiral-ast-map-elt :get)
                                           (spiral-command-template)))))))


(defun spiral-ast--insert-object-representation (type class id-hash object-repr
                                                      &optional pretty-repr)
  "Return a prettified object representation as a string.
TYPE is a symbol that indicates the type of object (i.e object, function,
atom, etc.)
CLASS is the object's java class as a string.
ID-HASH is the object's id hash as a string.
OBJECT-REPR is the object's actual representation (as presented by
clojure.main/repl) as a string.
PRETTY-REPR is how UNREPL represents said object, i.e. functions come
demunged from UNREPL."
  (let* ((pretty-repr (or pretty-repr class))
         (actual-repr (format "#object[%s %S %s]" class id-hash object-repr)))
    (if (eql type 'object)
        (insert actual-repr)
      (when (eql (current-buffer) (spiral--make-buffer-for-mode 'clojure-mode))
        (erase-buffer))
      (insert
       (format "#%s[%s %s]"
               type pretty-repr
               (propertize id-hash
                           'font-lock-face 'spiral-font-tooltip-face
                           'help-echo actual-repr))))))


(defun spiral-ast--object-tag-unparse (object-tag-node mute-ui)
  "Insert a string representation of OBJECT-TAG-NODE.
OBJECT-TAG-NODE's child is a vector that has 4 elements, these are:
- Class name, as a #unrepl.java/class tagged literal.
- Identity hash code, as a string node.
- Object representation, as a symbol node, a string node, or another tagged
  literal.
- A map node with extra information.

By default, this function will create tooltips for the object
representation with additional information.  MUTE-UI overrides this
behavior."
  (let* ((obj-attrs (parseclj-ast-children (spiral-ast-tag-child object-tag-node)))
         (class-name (spiral-ast-unparse-to-string (car obj-attrs)))
         (id-hash (parseclj-ast-value (cadr obj-attrs)))
         (object-repr "--"))  ;; TODO: placeholder for actual object repr
    (if (not spiral-enable-pretty-object-representations)
        (spiral-ast--insert-object-representation 'object class-name id-hash object-repr)
      (let* ((spiral-repr-node (cl-caddr obj-attrs))
             (spiral-repr (spiral-ast-unparse-to-string spiral-repr-node)))
        (cond
         ;; Representation for a "demunged" function.
         ((eql (parseclj-ast-node-type spiral-repr-node) :symbol)
          (spiral-ast--insert-object-representation 'function
                                                    class-name
                                                    id-hash
                                                    object-repr
                                                    spiral-repr))
         ;; Image
         ((and (eql (parseclj-ast-node-type spiral-repr-node) :map)
               (spiral-ast-map-elt spiral-repr-node :attachment)
               (spiral-ast-map-elt spiral-repr-node :width))  ;; hack
          (let* ((width (thread-first spiral-repr-node
                          (spiral-ast-map-elt :width)
                          (spiral-ast-unparse-to-string)))
                 (height (thread-first spiral-repr-node
                           (spiral-ast-map-elt :height)
                           (spiral-ast-unparse-to-string)))
                 (attachment (spiral-ast-map-elt spiral-repr-node :attachment)))
            (spiral-ast--insert-object-representation 'image class-name id-hash object-repr
                                                      (format "%s <%sx%s>" class-name width height))
            (unless mute-ui
              (spiral-ast-unparse attachment))))
         ;; Any other type of object
         (t (spiral-ast--insert-object-representation
             'object class-name id-hash spiral-repr)))))))


(defun spiral-ast--ratio-tag-unparse (ratio-tag-node _mute-ui)
  "Insert a ratio representation of RATIO-TAG-NODE."
  (seq-let [num denom] (parseclj-ast-children (spiral-ast-tag-child ratio-tag-node))
    (spiral-ast-unparse num)
    (insert "/")
    (spiral-ast-unparse denom)))


(defun spiral-ast--string-tag-unparse (string-tag-node mute-ui &optional stdout-str)
  "Insert a string representation of STRING-TAG-NODE.
STDOUT-STR is a boolean value that indicates whether or not the
STRING-TAG-NODE represents a stdout (`:out') message from UNREPL.  In case
that STDOUT-STR is non-nil, surronding quotes for this string will be
ommited.
MUTE-UI is a flag that indicates whether or not to insert UI elements like
buttons."
  (let* ((insert-fn (if stdout-str
                        #'spiral-ast-unparse-stdout-string
                      #'spiral-ast-unparse))
         (string-tag-vector-elems (thread-first string-tag-node
                                    (spiral-ast-tag-child)
                                    (parseclj-ast-children)))
         (string-node (car string-tag-vector-elems))
         (elision-actions (spiral-ast-tag-child (cadr string-tag-vector-elems))))
    ;; insert string
    (funcall insert-fn string-node)
    ;; and then elision button
    (if mute-ui
        (insert (propertize "..." 'category 'spiral-elision))
      (spiral-button-aux-action-throwaway-insert
       spiral-button-elision-label
       (thread-first elision-actions
         (spiral-ast-map-elt :get)
         (spiral-command-template))
       (lambda (eval-payload)
         (let ((p (point)))
           (funcall insert-fn eval-payload)
           (unless stdout-str  ;; Delete the opening quote
             (delete-region p (1+ p)))))
       nil
       (unless stdout-str (1- (point)))))))


(defun spiral-ast--var-tag-unparse (var-tag-node _mute-ui)
  "Insert a string representation of VAR-TAG-NODE."
  (spiral-ast--generic-tag-child-unparse var-tag-node nil))


(defun spiral-ast--class-tag-unparse (class-tag-node _mute-ui)
  "Insert a string representation of a java class CLASS-TAG-NODE."
  (spiral-propertize-region '(font-lock-face spiral-font-class-name-face)
    (spiral-ast--generic-tag-child-unparse class-tag-node nil)))


(defun spiral-ast--generic-tag-child-unparse (tag-node mute-ui)
  "Skip TAG-NODE's name and unparse its child.
MUTE-UI is a flag that indicates whether or not to insert UI elements like
buttons."
  (thread-first tag-node
    (spiral-ast-tag-child)
    (spiral-ast-unparse nil mute-ui)))


(defun spiral-ast--generic-tag-unparse (tag-node mute-ui)
  "Insert a string representation of the given AST TAG-NODE into buffer.
MUTE-UI is a flag that indicates whether or not to insert UI elements like
buttons."
  (insert "#" (symbol-name (parseclj-ast-node-attr tag-node :tag)) " ")
  (spiral-ast--generic-tag-child-unparse tag-node mute-ui))



;; Unparsing
;; -------------------------------------------------------------------

(defun spiral-ast-tag-child (tag-node)
  "Return the child node of TAG-NODE."
  (thread-first tag-node (parseclj-ast-children) (car)))


(defun spiral-ast--unparse-collection (node no-delimiters mute-ui raise-on-missing-tags)
  "Insert a string representation of the given AST branch NODE into buffer.
When NO-DELIMITERS is non-nil, avoid inserting delimiters for this
collection.
MUTE-UI and RAISE-ON-MISSING-TAGS are both flags, as described in
`spiral-ast-unparse'."
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
        (spiral-ast-unparse node nil mute-ui raise-on-missing-tags))
      (seq-doseq (child (cdr nodes))
        (when (null (parseclj-ast-node-attr node :lexical-preservation))  ;; hack, read below.
          (insert " "))
        (spiral-ast-unparse child nil mute-ui raise-on-missing-tags)))
    (unless no-delimiters (insert (cdr delimiters)))))
;; Here we're basically accessing parseclj's internal API to check if a node has
;; lexical preservation.  It would be better if parseclj would expose a function
;; for this, something like `parseclj-ast-node-lexical-preservation-p'.  Another
;; possible solution would be to have `parseclj-unparse-clojure' take tag
;; readers as an argument, but I'm not really sure that would be part of its
;; scope.


(defun spiral-ast-unparse (ast-node &optional no-delimiters mute-ui raise-on-missing-tags)
  "Unparse a parseclj AST-NODE into a human-friendly representation.
For the most parts, this function behaves exactly as
`parseclj-unparse-clojure-to-string', but it will also consume all tagged
literals in AST-NODE using `spiral-ast-tag-readers' to generate a elisp
code for better UI.
NO-DELIMITERS is a boolean value that indicate whether to delete/remove
the opening and closing tokens for the retrieved
collection (i.e. opening/closing parens for lists, opening/closing brackets
for vectors etc.)
MUTE-UI is a boolean-value to indicate whether or not to include UI
elements like buttons into the unparsing.
If AST-NODE contains a tagged literal not represented in TAG-READERS-MAP,
an error will be raised.  RAISE-ON-MISSING-TAGS overrides this behavior.
Return a string that may or may not be propertized, and may or may not
include other UI elements."
  (if (parseclj-ast-leaf-node-p ast-node)
      (insert (parseclj-ast-node-attr ast-node :form))
    (if (eql (parseclj-ast-node-type ast-node) :tag)
        (let* ((tag-symbol (parseclj-ast-node-attr ast-node :tag))
               (reader (spiral-ast--find-tag-reader tag-symbol mute-ui raise-on-missing-tags)))
          (if reader
              (funcall reader ast-node)
            (spiral-ast--generic-tag-unparse ast-node mute-ui)))
      (spiral-ast--unparse-collection ast-node no-delimiters mute-ui raise-on-missing-tags))))


(defun spiral-ast-unparse-to-string (ast-node &optional with-ui raise-on-missing-tags)
  "Unparse a parseclj AST-NODE to a string representation.
This function works like `spiral-ast-unparse' but returns a string.
WITH-UI is a flag to indicate whether or not to include UI elements like
buttons in the result.  It's defined as the complement of
`spiral-ast-unparse's NO-UI flag, since it is commonly expected to not want
UI elements when rendering to a string.
If AST-NODE contains tagged literals not represented in TAG-READERS-MAP, an
error will be raised.  RAISE-ON-MISSING-TAGS overrides this behavior.
The returned string will be automatically font-locked as clojure code."
  (with-current-buffer (spiral--make-buffer-for-mode 'clojure-mode)
    (erase-buffer)
    (spiral-ast-unparse ast-node nil (not with-ui) raise-on-missing-tags)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))


(defun spiral-ast-unparse-stdout-string (string-node &optional mute-ui)
  "Unparse STRING-NODE, which can be a simple string or a #unrepl/string tag.
MUTE-UI is a flag to indicate whether or not insert UI elements like
buttons."
  (cond ((eql (parseclj-ast-node-type string-node) :string)
         (thread-first string-node
           (parseclj-ast-value)
           (propertize 'font-lock-face 'spiral-font-stdout-face)
           (insert)))
        ((parseclj-ast-node-p string-node)
         (spiral-ast--string-tag-unparse string-node mute-ui t))))


;; Template Management
;; -------------------------------------------------------------------

(defun spiral-ast--generate-replace-param-tags-fn (params)
  "Create a closure fn for replacing param tag nodes with PARAMS.
Return a function that receives a param tag AST node, gets its keyword
children (the actual param name) and compares against the PARAMS alist to
see if there's a valid replacement for it."
  (lambda (param-tag-node)
    (let* ((param-keyword (thread-first param-tag-node
                            (spiral-ast-tag-child)
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
          (let* ((replacement (cdr replacement))
                 (replacement-str (if (symbolp replacement)
                                      (format "%s" replacement)
                                    (format "%S" replacement))))
            (thread-first replacement-str
              (parseclj-parse-clojure)
              (parseclj-ast-children)
              (car)))
        (error "Parameter %S not set in %S" param-keyword params)))))


(defun spiral-ast--replace-param-tags (root params)
  "Traverse ROOT and replace 'param tag' nodes with PARAMS.
For each param tag, PARAMS alist is checked to see if there's a
corresponding replacement.
Return ROOT with all available param tags replaced."
  (let ((loc (spiral-ast-zip root)))
    (if (treepy-end-p loc)
        root
      (let ((replace-param-tag-fn (spiral-ast--generate-replace-param-tags-fn params)))
        (while (not (treepy-end-p (treepy-next loc)))
          (let ((node (treepy-node loc)))
            (setq loc (treepy-next
                       (if (eql (parseclj-ast-node-attr node :tag) 'unrepl/param)
                           (treepy-edit loc replace-param-tag-fn)
                         loc)))))
        (treepy-root loc)))))


(defun spiral-command-template (template-ast &optional params)
  "Process TEMPLATE-AST with PARAMS and return a string.
PARAMS should be an alist of tagged literal symbols and readers."
  (let ((cmd (spiral-ast--replace-param-tags template-ast params)))
    (parseclj-unparse-clojure-to-string cmd)))


(provide 'spiral-ast)

;;; spiral-ast.el ends here
