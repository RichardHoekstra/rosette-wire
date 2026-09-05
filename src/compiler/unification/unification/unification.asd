#.(progn (require :asdf) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((self-dir (make-pathname :defaults *load-pathname* :name nil :type nil))
         (root (loop for dir = self-dir
                     then (make-pathname :directory (butlast (pathname-directory dir)) :defaults dir)
                     while (cdr (pathname-directory dir))
                     when (probe-file (merge-pathnames ".rosette-wire-root" dir)) return dir)))
    (if root
        (asdf:initialize-source-registry `(:source-registry (:tree ,root) :ignore-inherited-configuration))
        (pushnew self-dir asdf:*central-registry* :test #'equal))))

(in-package :asdf-user)


(defsystem #:unification
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "First-order logic programming core: terms, substitutions, Robinson unification with occurs-check (most general unifier), SLD resolution over a Horn clause knowledge base with fresh-variable renaming, and the DUAL operation -- Plotkin-Reynolds anti-unification (least general generalization) with one-sided matching, so UNIFY (common instance / meet) and ANTI-UNIFY (common generalization / join) are the two operations of the subsumption lattice. The symbolic half of the neurosymbolic loop (Prolog's engine as a zero-dependency kernel)."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core") (:file "anti-unify"))
  :in-order-to
  ((test-op (test-op #:unification/tests))))


(defsystem #:unification/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for unification."
  :depends-on
  (#:unification)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-unification/tests :run-all-tests)))
