;;;; tests/package.lisp --- package definition for rosette-union-find-core tests.

(defpackage #:rosette-union-find-core/tests
  (:use #:cl #:rosette-union-find-core #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-union-find-core/tests)
