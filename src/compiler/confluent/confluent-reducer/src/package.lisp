;;;; package.lisp --- public API for rosette-confluent-reducer.
;;;;
;;;; The HVM-class runtime for the substrate: fire all order-independent
;;;; (confluent) redexes concurrently, PROVE the parallel schedule is
;;;; gauge-equivalent to the serial normal form, and inherit rosette-core-term's
;;;; eval == normalize == compile coincidence.  Parallelism is a CERTIFIED
;;;; GAUGE (provable order-independence), not a hope.

(defpackage #:rosette-confluent-reducer
  (:use #:cl)
  (:local-nicknames (#:ct  #:rosette-core-term)
                    (#:cf  #:rosette-confluence-frontier)
                    (#:caf #:rosette-causal-frontier)
                    (#:rep #:rosette-causal-repartition)
                    (#:pw  #:rosette-proof-witness))
  (:export
   ;; --- the extracted op the reducer schedules ------------------------------
   #:cred-op #:cred-op-p #:make-cred-op
   #:cred-op-cell #:cred-op-kind #:cred-op-value
   #:extract-ops
   ;; --- classification: confluent batches vs non-confluent frontier ---------
   #:classify #:reduction-plan #:reduction-plan-p
   #:reduction-plan-cell #:reduction-plan-batches
   #:reduction-plan-frontier
   #:parallel-width #:non-confluent-frontier
   ;; --- the reducer: reduce == eval, sealed as a gauge ----------------------
   #:confluent-reduce
   #:reduce-result #:serial-normal-form
   #:parallel-schedule-value
   #:partition-gauge-certificate
   #:repartition-report
   ;; --- the master proof-witness certificate --------------------------------
   #:confluent-reduce-certificate
   #:verify-confluent-reduce-certificate))

(in-package #:rosette-confluent-reducer)
