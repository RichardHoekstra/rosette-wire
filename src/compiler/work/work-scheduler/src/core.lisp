;;;; core.lisp --- Static work scheduler.

(in-package #:rosette-work-scheduler)

(defun %make-equal-table ()
  "Return the standard name/resource lookup table used by scheduler passes."
  (make-hash-table :test #'equal))

(defun %finish-reasons (reasons)
  "Return verifier values from a reverse-collected REASONS list."
  (let ((ordered (nreverse reasons)))
    (values (null ordered) ordered)))

(defstruct (work-item (:constructor %make-work-item)
                      (:copier nil))
  "A pure scheduling descriptor.

READS and WRITES are resource names.  DEPENDS-ON lists predecessor task
names.  COMMUTES-WITH names tasks whose write/write or read/write overlap is
known order-independent by a domain certificate."
  name
  (reads nil :type list)
  (writes nil :type list)
  (cost 1d0 :type real)
  (depends-on nil :type list)
  (commutes-with nil :type list)
  (metadata nil :type list))

(defun make-work-item (&key name reads writes (cost 1d0) depends-on commutes-with metadata)
  "Create a work descriptor used by the static scheduler."
  (unless name
    (error "make-work-item: NAME is required."))
  (unless (and (realp cost) (not (minusp cost)))
    (error "make-work-item: COST must be a non-negative real."))
  (%make-work-item :name name
                   :reads (%canonical-set reads)
                   :writes (%canonical-set writes)
                   :cost cost
                   :depends-on (%canonical-set depends-on)
                   :commutes-with (%canonical-set commutes-with)
                   :metadata metadata))

(defstruct (work-conflict (:constructor %make-work-conflict)
                          (:predicate work-conflict-p)
                          (:copier nil))
  left
  right
  resource
  kind)

(defstruct (parallel-plan (:constructor %make-parallel-plan)
                          (:copier nil))
  batches
  serial-cost
  parallel-cost
  speedup
  profitable-p
  reasons)

(defstruct (work-dag-analysis (:constructor %make-work-dag-analysis)
                              (:copier nil))
  "Dependency critical-path analysis for a set of work items."
  total-cost
  critical-path-cost
  available-parallelism
  max-frontier-width
  frontier-widths
  task-slack)

(defstruct (work-causal-cone (:constructor %make-work-causal-cone)
                             (:copier nil))
  "Backward dependency cone for a target set of tasks."
  targets
  tasks
  boundary
  excluded
  total-cost
  critical-path-cost
  frontier-widths)

(defstruct (work-affected-cone (:constructor %make-work-affected-cone)
                               (:predicate work-affected-cone-p)
                               (:copier nil))
  "Forward dependency cone that must be recomputed after a change."
  sources
  resources
  tasks
  prerequisites
  boundary
  excluded
  total-cost
  critical-path-cost
  frontier-widths)

(defstruct (work-cone-metrics (:constructor %make-work-cone-metrics)
                              (:predicate work-cone-metrics-p)
                              (:copier nil))
  "Compact light-cone metrics for planning and lowering decisions."
  kind
  roots
  boundary
  task-count
  exterior-count
  total-cost
  critical-path-cost
  available-parallelism
  max-frontier-width
  frontier-widths
  serial-fraction)

(defstruct (work-action-score (:constructor %make-work-action-score)
                              (:copier nil))
  "Scalar action score for a work graph or causal cone."
  value
  compute-cost
  critical-path-cost
  communication-cost
  parallelism-pressure
  weights
  metadata)

(defstruct (work-action-choice (:constructor %make-work-action-choice)
                               (:predicate work-action-choice-p)
                               (:copier nil))
  "A ranked candidate under the work action functional."
  name
  score
  items
  cone
  metadata)

(defstruct (work-action-ensemble (:constructor %make-work-action-ensemble)
                                 (:predicate work-action-ensemble-p)
                                 (:copier nil))
  "Boltzmann ensemble over action candidates.

The same action score used for hard minimization becomes an energy.  Lower
action receives higher probability at finite temperature; as temperature tends
toward zero, the ensemble collapses to MINIMIZE-WORK-ACTION."
  temperature
  choices
  probabilities
  free-energy
  log-partition
  winner
  metadata)

