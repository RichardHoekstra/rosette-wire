;;;; rosette-program-ir/src/package.lisp --- Public API.

(in-package #:cl-user)

(defpackage #:rosette-program-ir
  (:use #:cl)
  (:export
   #:program
   #:programp
   #:make-program
   #:program-ast
   #:program-children
   #:program-arity
   #:program-metadata
   #:program-parent

   #:walk-ast
   #:map-ast
   #:fold-ast
   #:replace-node
   #:for-each-node
   #:count-nodes
   #:tree-depth

   #:lisp->program
   #:program->lisp
   #:program-equal?
   #:canonical-form))

