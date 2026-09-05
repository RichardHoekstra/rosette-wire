;;;; tests/package.lisp --- package for rosette-confluence-frontier tests.

(defpackage #:rosette-confluence-frontier/tests
  (:use #:cl #:rosette-confluence-frontier #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-confluence-frontier/tests)
