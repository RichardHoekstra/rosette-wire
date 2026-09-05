;;;; package.lisp --- Public API for rosette-work-scheduler.

(defpackage #:rosette-work-scheduler
  (:use #:cl)
  (:import-from #:rosette-scalar-core
                #:as-f64
                #:f64-ratio
                #:f64-distance<)
  (:export
   #:work-item
   #:make-work-item
   #:work-item-name
   #:work-item-reads
   #:work-item-writes
   #:work-item-cost
   #:work-item-depends-on
   #:work-item-commutes-with
   #:work-item-metadata
   #:work-conflict
   #:work-conflict-p
   #:work-conflict-left
   #:work-conflict-right
   #:work-conflict-resource
   #:work-conflict-kind
   #:work-conflicts
   #:work-independent-p
   #:parallel-batches
   #:parallel-plan
   #:parallel-plan-batches
   #:parallel-plan-serial-cost
   #:parallel-plan-parallel-cost
   #:parallel-plan-speedup
   #:parallel-plan-profitable-p
   #:parallel-plan-reasons
   #:worker-lanes
   #:work-dag-analysis
   #:work-dag-analysis-p
   #:work-dag-analysis-total-cost
   #:work-dag-analysis-critical-path-cost
   #:work-dag-analysis-available-parallelism
   #:work-dag-analysis-max-frontier-width
   #:work-dag-analysis-frontier-widths
   #:work-dag-analysis-task-slack
   #:work-causal-cone
   #:work-causal-cone-p
   #:work-causal-cone-targets
   #:work-causal-cone-tasks
   #:work-causal-cone-boundary
   #:work-causal-cone-excluded
   #:work-causal-cone-total-cost
   #:work-causal-cone-critical-path-cost
   #:work-causal-cone-frontier-widths
   #:work-affected-cone
   #:work-affected-cone-p
   #:work-affected-cone-sources
   #:work-affected-cone-resources
   #:work-affected-cone-tasks
   #:work-affected-cone-prerequisites
   #:work-affected-cone-boundary
   #:work-affected-cone-excluded
   #:work-affected-cone-total-cost
   #:work-affected-cone-critical-path-cost
   #:work-affected-cone-frontier-widths
   #:work-cone-metrics
   #:work-cone-metrics-p
   #:work-cone-metrics-kind
   #:work-cone-metrics-roots
   #:work-cone-metrics-boundary
   #:work-cone-metrics-task-count
   #:work-cone-metrics-exterior-count
   #:work-cone-metrics-total-cost
   #:work-cone-metrics-critical-path-cost
   #:work-cone-metrics-available-parallelism
   #:work-cone-metrics-max-frontier-width
   #:work-cone-metrics-frontier-widths
   #:work-cone-metrics-serial-fraction
   #:work-action-score
   #:work-action-score-p
   #:work-action-score-value
   #:work-action-score-compute-cost
   #:work-action-score-critical-path-cost
   #:work-action-score-communication-cost
   #:work-action-score-parallelism-pressure
   #:work-action-score-weights
   #:work-action-score-metadata
   #:work-action-choice
   #:work-action-choice-p
   #:work-action-choice-name
   #:work-action-choice-score
   #:work-action-choice-items
   #:work-action-choice-cone
   #:work-action-choice-metadata
   #:work-action-ensemble
   #:work-action-ensemble-p
   #:work-action-ensemble-temperature
   #:work-action-ensemble-choices
   #:work-action-ensemble-probabilities
   #:work-action-ensemble-free-energy
   #:work-action-ensemble-log-partition
   #:work-action-ensemble-winner
   #:work-action-ensemble-metadata
   #:plan-work-schedule
   #:analyze-work-dag
   #:causal-cone
   #:causal-cone-plan
   #:affected-cone
   #:affected-cone-plan
   #:score-work-action
   #:score-causal-cone-action
   #:rank-work-actions
   #:minimize-work-action
   #:rank-causal-cone-actions
   #:minimize-causal-cone-action
   #:causal-cone-action-ensemble
   #:action-choice-certificate
   #:verify-action-choice-certificate
   #:action-ensemble-certificate
   #:verify-action-ensemble-certificate
   #:normalize-effect-word
   #:work-item-effect-word
   #:normalize-work-item-effects
   #:algebraic-commute-p
   #:apply-algebraic-commutes
   #:effect-word-certificate
   #:verify-effect-word-certificate
   #:schedule-bottlenecks
   #:schedule-optimization-hints
   #:verify-parallel-plan
   #:schedule-certificate
   #:verify-schedule-certificate
   #:explain-parallel-plan
   ;; rosette-graph-core interop
   #:work-items->csr
   #:work-items-dependency-cycles))

(defpackage #:rosette-work-scheduler/items
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:work-item
                #:make-work-item
                #:work-item-name
                #:work-item-reads
                #:work-item-writes
                #:work-item-cost
                #:work-item-depends-on
                #:work-item-commutes-with
                #:work-item-metadata
                #:work-conflict
                #:work-conflict-p
                #:work-conflict-left
                #:work-conflict-right
                #:work-conflict-resource
                #:work-conflict-kind
                #:work-conflicts
                #:work-independent-p)
  (:export
   #:work-item #:make-work-item #:work-item-name #:work-item-reads
   #:work-item-writes #:work-item-cost #:work-item-depends-on
   #:work-item-commutes-with #:work-item-metadata
   #:work-conflict #:work-conflict-p #:work-conflict-left
   #:work-conflict-right #:work-conflict-resource #:work-conflict-kind
   #:work-conflicts #:work-independent-p))

(defpackage #:rosette-work-scheduler/planning
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:parallel-batches
                #:parallel-plan
                #:parallel-plan-batches
                #:parallel-plan-serial-cost
                #:parallel-plan-parallel-cost
                #:parallel-plan-speedup
                #:parallel-plan-profitable-p
                #:parallel-plan-reasons
                #:worker-lanes
                #:plan-work-schedule
                #:schedule-bottlenecks
                #:schedule-optimization-hints
                #:verify-parallel-plan
                #:explain-parallel-plan)
  (:export
   #:parallel-batches #:parallel-plan #:parallel-plan-batches
   #:parallel-plan-serial-cost #:parallel-plan-parallel-cost
   #:parallel-plan-speedup #:parallel-plan-profitable-p
   #:parallel-plan-reasons #:worker-lanes #:plan-work-schedule
   #:schedule-bottlenecks #:schedule-optimization-hints
   #:verify-parallel-plan #:explain-parallel-plan))

(defpackage #:rosette-work-scheduler/cones
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:work-dag-analysis
                #:work-dag-analysis-p
                #:work-dag-analysis-total-cost
                #:work-dag-analysis-critical-path-cost
                #:work-dag-analysis-available-parallelism
                #:work-dag-analysis-max-frontier-width
                #:work-dag-analysis-frontier-widths
                #:work-dag-analysis-task-slack
                #:work-causal-cone
                #:work-causal-cone-p
                #:work-causal-cone-targets
                #:work-causal-cone-tasks
                #:work-causal-cone-boundary
                #:work-causal-cone-excluded
                #:work-causal-cone-total-cost
                #:work-causal-cone-critical-path-cost
                #:work-causal-cone-frontier-widths
                #:work-affected-cone
                #:work-affected-cone-p
                #:work-affected-cone-sources
                #:work-affected-cone-resources
                #:work-affected-cone-tasks
                #:work-affected-cone-prerequisites
                #:work-affected-cone-boundary
                #:work-affected-cone-excluded
                #:work-affected-cone-total-cost
                #:work-affected-cone-critical-path-cost
                #:work-affected-cone-frontier-widths
                #:work-cone-metrics
                #:work-cone-metrics-p
                #:work-cone-metrics-kind
                #:work-cone-metrics-roots
                #:work-cone-metrics-boundary
                #:work-cone-metrics-task-count
                #:work-cone-metrics-exterior-count
                #:work-cone-metrics-total-cost
                #:work-cone-metrics-critical-path-cost
                #:work-cone-metrics-available-parallelism
                #:work-cone-metrics-max-frontier-width
                #:work-cone-metrics-frontier-widths
                #:work-cone-metrics-serial-fraction
                #:analyze-work-dag
                #:causal-cone
                #:causal-cone-plan
                #:affected-cone
                #:affected-cone-plan)
  (:export
   #:work-dag-analysis #:work-dag-analysis-p #:work-dag-analysis-total-cost
   #:work-dag-analysis-critical-path-cost
   #:work-dag-analysis-available-parallelism
   #:work-dag-analysis-max-frontier-width
   #:work-dag-analysis-frontier-widths #:work-dag-analysis-task-slack
   #:work-causal-cone #:work-causal-cone-p #:work-causal-cone-targets
   #:work-causal-cone-tasks #:work-causal-cone-boundary
   #:work-causal-cone-excluded #:work-causal-cone-total-cost
   #:work-causal-cone-critical-path-cost
   #:work-causal-cone-frontier-widths
   #:work-affected-cone #:work-affected-cone-p #:work-affected-cone-sources
   #:work-affected-cone-resources #:work-affected-cone-tasks
   #:work-affected-cone-prerequisites #:work-affected-cone-boundary
   #:work-affected-cone-excluded #:work-affected-cone-total-cost
   #:work-affected-cone-critical-path-cost
   #:work-affected-cone-frontier-widths
   #:work-cone-metrics #:work-cone-metrics-p #:work-cone-metrics-kind
   #:work-cone-metrics-roots #:work-cone-metrics-boundary
   #:work-cone-metrics-task-count #:work-cone-metrics-exterior-count
   #:work-cone-metrics-total-cost #:work-cone-metrics-critical-path-cost
   #:work-cone-metrics-available-parallelism
   #:work-cone-metrics-max-frontier-width
   #:work-cone-metrics-frontier-widths #:work-cone-metrics-serial-fraction
   #:analyze-work-dag #:causal-cone #:causal-cone-plan
   #:affected-cone #:affected-cone-plan))

(defpackage #:rosette-work-scheduler/actions
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:work-action-score
                #:work-action-score-p
                #:work-action-score-value
                #:work-action-score-compute-cost
                #:work-action-score-critical-path-cost
                #:work-action-score-communication-cost
                #:work-action-score-parallelism-pressure
                #:work-action-score-weights
                #:work-action-score-metadata
                #:work-action-choice
                #:work-action-choice-p
                #:work-action-choice-name
                #:work-action-choice-score
                #:work-action-choice-items
                #:work-action-choice-cone
                #:work-action-choice-metadata
                #:work-action-ensemble
                #:work-action-ensemble-p
                #:work-action-ensemble-temperature
                #:work-action-ensemble-choices
                #:work-action-ensemble-probabilities
                #:work-action-ensemble-free-energy
                #:work-action-ensemble-log-partition
                #:work-action-ensemble-winner
                #:work-action-ensemble-metadata
                #:score-work-action
                #:score-causal-cone-action
                #:rank-work-actions
                #:minimize-work-action
                #:rank-causal-cone-actions
                #:minimize-causal-cone-action
                #:causal-cone-action-ensemble)
  (:export
   #:work-action-score #:work-action-score-p #:work-action-score-value
   #:work-action-score-compute-cost
   #:work-action-score-critical-path-cost
   #:work-action-score-communication-cost
   #:work-action-score-parallelism-pressure #:work-action-score-weights
   #:work-action-score-metadata
   #:work-action-choice #:work-action-choice-p #:work-action-choice-name
   #:work-action-choice-score #:work-action-choice-items
   #:work-action-choice-cone #:work-action-choice-metadata
   #:work-action-ensemble #:work-action-ensemble-p
   #:work-action-ensemble-temperature #:work-action-ensemble-choices
   #:work-action-ensemble-probabilities #:work-action-ensemble-free-energy
   #:work-action-ensemble-log-partition #:work-action-ensemble-winner
   #:work-action-ensemble-metadata
   #:score-work-action #:score-causal-cone-action #:rank-work-actions
   #:minimize-work-action #:rank-causal-cone-actions
   #:minimize-causal-cone-action #:causal-cone-action-ensemble))

(defpackage #:rosette-work-scheduler/certificates
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:action-choice-certificate
                #:verify-action-choice-certificate
                #:action-ensemble-certificate
                #:verify-action-ensemble-certificate
                #:schedule-certificate
                #:verify-schedule-certificate)
  (:export
   #:action-choice-certificate #:verify-action-choice-certificate
   #:action-ensemble-certificate #:verify-action-ensemble-certificate
   #:schedule-certificate #:verify-schedule-certificate))

(defpackage #:rosette-work-scheduler/effects
  (:use #:cl)
  (:import-from #:rosette-work-scheduler
                #:normalize-effect-word
                #:effect-word-certificate
                #:verify-effect-word-certificate)
  (:export
   #:normalize-effect-word #:effect-word-certificate
   #:verify-effect-word-certificate))
