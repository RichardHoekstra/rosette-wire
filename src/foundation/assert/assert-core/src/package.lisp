;;;; package.lisp --- package definition for rosette-assert-core.

(defpackage #:rosette-assert-core
  (:use #:cl)
  (:import-from #:rosette-scalar-core
                #:approx=)
  (:export
   #:assert-true
   #:assert=
   #:assert-close
   #:assert-array-close
   #:test-run-name
   #:test-run-assertions
   #:test-run-failures
   #:make-test-run
   #:record-test
   #:check
   #:finish-test-run
   #:with-test-run))
