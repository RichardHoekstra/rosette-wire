;;;; tests/package.lisp --- package for rosette-causal-frontier tests.

(defpackage #:rosette-causal-frontier/tests
  (:use #:cl #:rosette-causal-frontier #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-causal-frontier/tests)
