;;;; tests/package.lisp --- package for rosette-causal-repartition tests.

(defpackage #:rosette-causal-repartition/tests
  (:use #:cl #:rosette-causal-repartition #:rosette-assert-core)
  (:local-nicknames (#:cf #:rosette-causal-frontier))
  (:export #:run-all-tests))
(in-package #:rosette-causal-repartition/tests)
