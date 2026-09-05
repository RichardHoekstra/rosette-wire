;;;; tests/package.lisp --- package definition for rosette-json tests.

(defpackage #:rosette-json/tests
  (:use #:cl #:rosette-json #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-json/tests)
