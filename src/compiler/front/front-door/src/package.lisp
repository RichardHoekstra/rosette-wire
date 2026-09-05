;;;; package.lisp --- public API for rosette-front-door.

(defpackage #:rosette-front-door
  (:use #:cl)
  (:shadow #:eval)
  (:export
   ;; The deliberately small, named pipeline.
   #:parse
   #:eval
   #:lower
   #:gate
   #:certify
   #:run
   ;; ProgramIR is a representation of the same program, not a second parser.
   #:surface->program-ir
   #:program-ir->surface
   #:core-program->surface
   ;; End-to-end result.
   #:front-door-result
   #:front-door-result-p
   #:front-door-result-program
   #:front-door-result-program-ir
   #:front-door-result-eval-value
   #:front-door-result-lowered-program
   #:front-door-result-oracle-value
   #:front-door-result-gate-passed
   #:front-door-result-certificate
   ;; The common closed STLC fragment and its three independent readouts.
   #:core-term->ccc
   #:core-term->blc
   #:gauge-term
   #:ccc-gauge-cost
   #:term-gauge-report
   #:term-gauge-report-p
   #:term-gauge-report-term
   #:term-gauge-report-ccc-term
   #:term-gauge-report-blc-term
   #:term-gauge-report-core-value
   #:term-gauge-report-ccc-value
   #:term-gauge-report-blc-value
   #:term-gauge-report-blc-length
   #:term-gauge-report-ccc-cost
   #:term-gauge-report-ccc-skipped
   #:term-gauge-report-passed
   #:term-gauge-report-certificate
   ;; Exact, admitted subset of expression-core.
   #:cf-expression->core-term
   #:run-cf-expression
   ;; Located refusal rather than accidental partial semantics.
   #:front-door-refusal
   #:front-door-refusal-stage
   #:front-door-refusal-input
   #:front-door-refusal-reason))

(in-package #:rosette-front-door)

;; Defined with the base package file so the optional system can add its
;; implementation without making the base package export undefined functions.
;; No optional-package symbols are imported here: loading rosette-front-door alone
;; neither loads nor depends on rosette-backend-agreement.
(defpackage #:rosette-front-door/backends
  (:use #:cl)
  (:export
   #:gate-backends
   #:backend-gate-report
   #:backend-gate-report-p
   #:backend-gate-report-source
   #:backend-gate-report-direct-value
   #:backend-gate-report-cpu-oracle-value
   #:backend-gate-report-verdict
   #:backend-gate-report-exercised-backends
   #:backend-gate-report-skipped-backends
   #:backend-gate-report-check-count
   #:backend-gate-report-cycle-rank
   #:backend-gate-report-passed
   #:backend-gate-report-certificate))
