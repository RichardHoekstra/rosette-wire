;;;; package.lisp --- package definition for rosette-unification.

(in-package #:cl-user)

(defpackage #:rosette-unification
  (:use #:cl)
  (:documentation
   "First-order logic programming core. Terms are Lisp data: an LVAR struct is a
logic variable, a CONS is a compound (car = functor symbol, cdr = argument
terms), any other atom is a constant. Robinson UNIFY (occurs-check on) computes
the most general unifier; SLD-RESOLVE proves goals against a Horn-clause
knowledge base with fresh-variable renaming. The symbolic engine of the
neurosymbolic loop.")
  (:export
   ;; terms
   #:lvar #:lvar-p #:lvar-name #:make-var
   #:var-p #:compound-p #:constant-p
   #:parse-term #:term->sexp
   ;; substitution / unification
   #:*occurs-check*
   #:walk #:occurs-p #:unify #:unify-failure-p #:mgu #:resolve
   ;; anti-unification (LGG) / matching -- the dual of unification
   #:term-equal #:anti-unify #:lgg #:anti-unify-terms
   #:match #:subsumes-p
   ;; clauses / resolution
   #:rule #:rule-p #:rule-head #:rule-body #:make-rule #:make-fact
   #:rename-rule #:sld-resolve #:prove #:query #:provable-p))
