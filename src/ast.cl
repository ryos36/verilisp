;;;; src/ast.cl — AST helpers for verilisp Phase 1a
;;;; AST nodes are S-expression lists tagged with a keyword symbol:
;;;;   (:module name params stmt...)
;;;;   (:wire decl...)
;;;;   etc.

(defun ast-node-p (x)
  "Return T if X is an AST node (a list whose car is a keyword)."
  (and (consp x)
       (keywordp (car x))))

(defun ast-tag (node)
  "Return the tag (keyword) of an AST node."
  (car node))

(defun ast-arg (node n)
  "Return the Nth argument of an AST node (0-indexed, skipping the tag)."
  (nth (1+ n) node))

(defun ast-args (node)
  "Return all arguments of an AST node (everything after the tag)."
  (cdr node))

(defun walk-ast (fn node)
  "Walk an AST tree, calling FN on each AST node (pre-order)."
  (when (ast-node-p node)
    (funcall fn node)
    (dolist (child (cdr node))
      (walk-ast fn child))))

(defun transform-ast (fn node)
  "Transform an AST tree by applying FN to each AST node (post-order).
   FN receives a node and should return a replacement node."
  (if (ast-node-p node)
      (let ((transformed-children
              (mapcar (lambda (child) (transform-ast fn child))
                      (cdr node))))
        (funcall fn (cons (car node) transformed-children)))
      node))
