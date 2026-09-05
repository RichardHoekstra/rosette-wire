;;;; package.lisp --- Public API for rosette-confluence-frontier.
;;;;
;;;; The boundary between order-free (symbolic / verifiable) and measure-requiring
;;;; (neuro / measurement) computation: a redex needs an external measure iff its
;;;; outcome depends on the order its concurrent ops resolve in.  Confluence is a
;;;; sufficient condition for not needing one; a forgetting reset is another.

(defpackage #:rosette-confluence-frontier
  (:use #:cl)
  (:local-nicknames (#:cf #:rosette-causal-frontier))
  (:export
   #:op
   #:order-result
   #:commuting-p
   #:confluent-p
   #:order-independent-p
   ;; the frontier
   #:measurement-needed-p
   ;; the symbolic side (confluent redex: unique, verify-only)
   #:unique-result
   #:verify-resolution
   ;; the neuro side (non-confluent redex: a measure collapses it)
   #:resolve-with-measure))
(in-package #:rosette-confluence-frontier)
