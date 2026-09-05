;;;; package.lisp --- test package for rosette-graph-core.

(defpackage #:rosette-graph-core/tests
  (:use #:cl #:rosette-graph-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-graph-core/tests)
