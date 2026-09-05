;;;; package.lisp --- Public API for rosette-causal-frontier.
;;;;
;;;; The causal partial order of which rosette-game-protocol's total order is the
;;;; quotient: hybrid logical clocks, happens-before, a deterministic
;;;; linearization, per-arbiter adjudication, and the partition-invariance law
;;;; (physical repartition is a gauge).

(defpackage #:rosette-causal-frontier
  (:use #:cl)
  (:local-nicknames (#:proto #:rosette-game-protocol))
  (:export
   ;; hybrid logical clock
   #:hlc
   #:hlc-p
   #:hlc-logical
   #:hlc-counter
   #:make-hlc
   #:hlc<
   #:hlc-tick
   #:hlc-merge
   ;; events (a command in the causal log)
   #:event
   #:event-p
   #:make-event
   #:event-id
   #:event-actor
   #:event-hlc
   #:event-deps
   #:event-target
   #:event-kind
   #:event-value
   ;; the partial order
   #:happens-before-p
   #:concurrent-p
   #:linearize
   ;; adjudication
   #:cell-state
   #:state-hash
   #:causal-final-state
   #:adjudicate
   #:one-target-one-arbiter-p
   ;; the laws
   #:partition-invariant-p
   #:quotient-consistent-p
   ;; game-protocol conformance (the total-ordered shadow)
   #:cf-world
   #:cf-world-p
   #:make-cf-world
   #:cf-world-tick
   #:cf-world-cells))
(in-package #:rosette-causal-frontier)
