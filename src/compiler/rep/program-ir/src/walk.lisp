;;;; walk.lisp --- Tree walkers, folders, replacers.
;;;;
;;;; All traversals here are pre-order (visit the node, then recurse
;;;; into its children).  Post-order variants are not part of the 0.1
;;;; surface; they may be added in 0.2 once a sister library demands
;;;; them.
;;;;
;;;; The walkers tolerate two kinds of node: a PROGRAM instance and a
;;;; raw atom (which may appear in compound ASTs constructed by hand
;;;; rather than via LISP->PROGRAM).  Raw atoms are visited as leaves.

(in-package #:rosette-program-ir)

(defun walk-ast (node visitor)
  "Pre-order traversal of NODE, invoking (VISITOR child) on every node.

NODE may be a PROGRAM instance or a raw atom appearing inside an AST.
The return value of VISITOR is ignored; use FOLD-AST when you need an
accumulator, or MAP-AST when you need a transformed copy.

Returns NODE."
  (declare (type function visitor))
  (funcall visitor node)
  (when (programp node)
    (let ((ast (program-ast node)))
      (when (consp ast)
        (dolist (child (cdr ast))
          (walk-ast child visitor)))))
  node)

(defun for-each-node (node visitor)
  "Alias for WALK-AST.  Stylistic preference for imperative call sites."
  (walk-ast node visitor))

(defun fold-ast (node init fn)
  "Pre-order fold of FN over NODE, threading INIT as the accumulator.

FN is called as (FN ACC CHILD) and must return the next accumulator.
CHILD is the raw node — a PROGRAM instance or an atom — exactly as
WALK-AST visits it."
  (declare (type function fn))
  (let ((acc init))
    (walk-ast node
              (lambda (n) (setf acc (funcall fn acc n))))
    acc))

(defun count-nodes (node)
  "Return the total number of nodes in NODE, including the root.

A PROGRAM whose AST is an atom counts as 1.  A PROGRAM whose AST is
(OP . CHILDREN*) counts as 1 + sum-of-COUNT-NODES over CHILDREN*.  Raw
atoms appearing as children also count as 1."
  (fold-ast node 0 (lambda (acc _node)
                     (declare (ignore _node))
                     (1+ acc))))

(defun tree-depth (node)
  "Return the depth of NODE, where a leaf has depth 1.

A PROGRAM whose AST is an atom has depth 1.  A PROGRAM with children
has depth (1 + max child-depth)."
  (cond
    ((not (programp node)) 1)
    (t (let ((ast (program-ast node)))
         (cond
           ((atom ast) 1)
           ((null (cdr ast)) 1)
           (t (1+ (reduce #'max (cdr ast) :key #'tree-depth))))))))

(defun map-ast (node fn)
  "Return a new PROGRAM tree where every node has been replaced by (FN node).

FN is called pre-order with the original node and must return either:
  - a PROGRAM instance (which becomes the replacement, including its
    own children — children of the original are NOT recursed into in
    that case), or
  - the symbol :RECURSE (which preserves the node and recurses into
    its children, copying each).

This signalling style mirrors a Scheme tree-walker; a richer rewriter
will land in rosette-mdl-parsimony."
  (declare (type function fn))
  (let ((result (funcall fn node)))
    (cond
      ((eq result :recurse)
       (cond
         ((not (programp node)) node)
         (t (let ((ast (program-ast node)))
              (cond
                ((atom ast) (make-program ast
                                          :metadata (program-metadata node)))
                (t (make-program
                    (cons (car ast)
                          (mapcar (lambda (child) (map-ast child fn))
                                  (cdr ast)))
                    :metadata (program-metadata node))))))))
      (t result))))

(defun replace-node (root predicate replacement)
  "Return a copy of ROOT with every node satisfying PREDICATE replaced
by REPLACEMENT.

PREDICATE is called with each node and returns generalised boolean.
REPLACEMENT may be a PROGRAM (used as-is) or a function of one
argument (the original node) returning the substitute.  Children of a
replaced node are not recursed into; the replacement stands alone."
  (declare (type function predicate))
  (map-ast root
           (lambda (node)
             (cond
               ((funcall predicate node)
                (if (functionp replacement)
                    (funcall replacement node)
                    replacement))
               (t :recurse)))))
