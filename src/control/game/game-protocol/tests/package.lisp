;;;; tests/package.lisp --- package for rosette-game-protocol tests.

(defpackage #:rosette-game-protocol/tests
  (:use #:cl #:rosette-game-protocol #:rosette-assert-core)
  (:export #:run-all-tests))
(in-package #:rosette-game-protocol/tests)
