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


(defsystem #:sound-self-rewrite
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A closed, sound self-improvement loop over s-expression programs:
metacircular unification-driven rewrite rules, each accepted only on an
rosette-claim-referee :confirmed verdict over a re-runnable behavioural-equivalence
certificate AND a strict op-count improvement; loops to fixpoint and provably
refuses any behaviour-changing rewrite."
  :depends-on
  (#:egraph-saturate #:unification #:claim-referee)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:sound-self-rewrite/tests))))


(defsystem #:sound-self-rewrite/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for sound-self-rewrite."
  :depends-on
  (#:sound-self-rewrite)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-sound-self-rewrite/tests :run-all-tests)))
