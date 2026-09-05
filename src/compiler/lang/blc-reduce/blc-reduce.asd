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


(defsystem #:blc-reduce
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "BLC EXECUTION (rung 1): a lazy combinator-graph reducer for rosette-blc terms -- the make-it-run side, paired with rosette-blc's measure-it-in-bits side. A faithful port of John Tromp's gblc.c ION machine: Kiselyov bracket abstraction (lambda->combinators S/K/I/B/C/R/T/D/M/Y/F/:) + lazy left-spine reduction with in-place redex update (sharing) + a Cheney copying GC. API: lambda->combinators (bracket abstraction), reduce-whnf, reduce-to-normal-form, reduce-church / reduce-bool (oracle decoders), combinators->term readback, last-step-count. Correctness is oracle-gated against ground-truth arithmetic/logic (Church ADD/MUL/POW/SUCC, S/K/I identities, AND/OR/NOT). rosette-sharing-reduction is rung 4 (interaction-net optimal / GPU-ready reduction); this is rung 1, the combinator graph reducer."
  :version
  "0.1.0"
  :depends-on
  (#:blc)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:blc-reduce/tests))))


(defsystem #:blc-reduce/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for blc-reduce."
  :depends-on
  (#:blc-reduce #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-blc-reduce/tests :run-all-tests)))
