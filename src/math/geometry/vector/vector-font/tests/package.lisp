;;;; tests/package.lisp --- package for rosette-vector-font tests.

(defpackage #:rosette-vector-font/tests
  (:use #:cl #:rosette-vector-font #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-vector-font/tests)
