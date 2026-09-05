;;;; package.lisp --- package definition for rosette-egraph.

(defpackage #:rosette-egraph
  (:use #:cl)
  (:import-from #:rosette-form-core
                #:form
                #:form-p
                #:form-value
                #:intern-form)
  (:export
   #:egraph
   #:egraph-p
   #:make-egraph
   #:egraph-add
   #:egraph-find
   #:egraph-merge
   #:egraph-equivalent-p
   #:egraph-class-members
   #:unify-forms))

(in-package #:rosette-egraph)
