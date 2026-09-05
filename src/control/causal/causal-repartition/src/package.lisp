;;;; package.lisp --- Public API for rosette-causal-repartition.
;;;;
;;;; The physical layer's rebalancer: min-cut on the causal-interaction graph
;;;; with a price on the cut (the network-vs-local latency ratio), a located
;;;; phase transition, local per-seam measurement, and a hysteresis controller
;;;; that turns the sharp transition into a flap-free switch.

(defpackage #:rosette-causal-repartition
  (:use #:cl)
  (:local-nicknames (#:cf #:rosette-causal-frontier))
  (:export
   ;; the causal-interaction graph (edge weight = causal-event-rate coupling)
   #:interaction-graph
   #:interaction-graph-p
   #:make-interaction-graph
   #:graph-nodes
   #:edge-weight
   #:internal-weight
   #:cut-weight
   ;; the cost model / phase transition
   #:net-gain
   #:best-cut
   #:should-split-p
   #:critical-bridge
   ;; the hysteresis controller
   #:repartition-event
   #:repartition-event-p
   #:repartition-event-kind
   #:repartition-event-hlc
   #:repartition-event-gain
   #:repartition-controller
   #:repartition-controller-p
   #:make-repartition-controller
   #:controller-state
   #:controller-step
   #:run-controller
   #:count-naive-flaps))
(in-package #:rosette-causal-repartition)
