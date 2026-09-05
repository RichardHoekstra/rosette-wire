;;;; planning.lisp --- parallel planning, DAG analysis, and cone APIs.

(in-package #:rosette-work-scheduler)

(defun work-conflicts (a b)
  "Return concrete footprint conflicts between two work items.

A returned empty list means their read/write footprints can run together
unless an explicit dependency orders them.  Declared commutativity suppresses
write conflicts: this is how algebraic laws from Hecke/Yang-Baxter/shadow
arithmetic enter without coupling this library to those domains."
  (let ((commuting (%commuting-p a b))
        (conflicts nil))
    (dolist (resource (intersection (work-item-writes a)
                                    (work-item-writes b)
                                    :test #'equal))
      (unless commuting
        (push (%make-work-conflict :left (work-item-name a)
                                   :right (work-item-name b)
                                   :resource resource
                                   :kind :write-write)
              conflicts)))
    (dolist (resource (intersection (work-item-writes a)
                                    (work-item-reads b)
                                    :test #'equal))
      (unless commuting
        (push (%make-work-conflict :left (work-item-name a)
                                   :right (work-item-name b)
                                   :resource resource
                                   :kind :write-read)
              conflicts)))
    (dolist (resource (intersection (work-item-reads a)
                                    (work-item-writes b)
                                    :test #'equal))
      (unless commuting
        (push (%make-work-conflict :left (work-item-name a)
                                   :right (work-item-name b)
                                   :resource resource
                                   :kind :read-write)
              conflicts)))
    (nreverse conflicts)))

(defun work-independent-p (a b)
  "Return T iff A and B can occupy the same parallel frontier."
  (and (not (%depends-on-p a b))
       (not (%depends-on-p b a))
       (null (work-conflicts a b))))

(defun parallel-batches (items)
  "Partition ITEMS into deterministic dependency-safe parallel batches.

The scheduler is greedy but stable: at each frontier it scans remaining work
in input order and admits an item if all dependencies are already scheduled
and it is independent of the current batch."
  (let ((remaining (copy-list items))
        (done nil)
        (batches nil))
    (%assert-unique-names remaining)
    (loop while remaining do
      (let ((frontier-done done)
            (batch nil)
            (deferred nil)
            (progress nil))
        (dolist (item remaining)
          (if (and (%dependencies-done-p item frontier-done)
                   (every (lambda (other) (work-independent-p item other)) batch))
              (progn
                (push item batch)
                (setf progress t))
              (push item deferred)))
        (unless progress
          (error "parallel-batches: dependency cycle or missing dependency among ~S"
                 (mapcar #'work-item-name remaining)))
        (dolist (item batch)
          (push (work-item-name item) done))
        (push (nreverse batch) batches)
        (setf remaining (nreverse deferred))))
    (nreverse batches)))

(defun plan-work-schedule (items &key (spawn-overhead 0d0) (min-speedup 1.05d0) max-workers)
  "Return a PARALLEL-PLAN plus a profitability decision.

The cost model is intentionally small: serial cost is sum(cost).  With no
MAX-WORKERS, a batch costs max(cost in batch) plus SPAWN-OVERHEAD when it has
more than one task.  With MAX-WORKERS, each batch is packed into greedy worker
waves and costs the resulting makespan plus the same overhead.  A plan is
profitable when it has at least one multi-item batch and its estimated speedup
is at least MIN-SPEEDUP."
  (when (and max-workers
             (or (not (integerp max-workers))
                 (not (plusp max-workers))))
    (error "plan-work-schedule: MAX-WORKERS must be NIL or a positive integer."))
  (let* ((batches (parallel-batches items))
         (serial (reduce #'+ items :key #'work-item-cost :initial-value 0d0))
         (parallel (reduce #'+ batches
                           :key (lambda (batch)
                                  (+ (%batch-makespan batch max-workers)
                                     (if (> (length batch) 1)
                                         spawn-overhead
                                         0d0)))
                           :initial-value 0d0))
         (speedup (if (zerop parallel)
                      1d0
                      (f64-ratio serial parallel)))
         (has-parallel-batch (some (lambda (batch) (> (length batch) 1)) batches))
         (profitable-p (and has-parallel-batch (>= speedup min-speedup)))
         (reasons (%plan-reasons batches serial parallel speedup profitable-p
                                 spawn-overhead min-speedup max-workers)))
    (%make-parallel-plan :batches batches
                         :serial-cost serial
                         :parallel-cost parallel
                         :speedup speedup
                         :profitable-p profitable-p
                         :reasons reasons)))

(defun worker-lanes (items &key max-workers)
  "Return bounded-worker lane assignments for each parallel frontier.

The result is a list with one entry per frontier.  Each frontier entry is a
list of lanes, and each lane is a serial list of work items assigned to one
worker.  With MAX-WORKERS NIL, every item in a frontier receives its own lane."
  (when (and max-workers
             (or (not (integerp max-workers))
                 (not (plusp max-workers))))
    (error "worker-lanes: MAX-WORKERS must be NIL or a positive integer."))
  (mapcar (lambda (batch)
            (%batch-lanes batch max-workers))
          (parallel-batches items)))

(defun analyze-work-dag (items)
  "Return critical-path and slack analysis for ITEMS.

The analysis uses declared dependencies for the critical path and the same
conflict-aware frontiers as PARALLEL-BATCHES for width.  TASK-SLACK entries
are plists containing `:TASK`, `:EARLIEST-START`, `:EARLIEST-FINISH`,
`:LATEST-START`, and `:SLACK`."
  (let* ((batches (parallel-batches items))
         (topo (apply #'append batches))
         (total (reduce #'+ items :key #'work-item-cost :initial-value 0d0))
         (earliest-starts (%make-equal-table))
         (earliest-finishes (%make-equal-table))
         (successors (%successors-by-name items)))
    (dolist (item topo)
      (let* ((start (reduce #'max (work-item-depends-on item)
                            :key (lambda (dep)
                                   (or (gethash dep earliest-finishes) 0d0))
                            :initial-value 0d0))
             (finish (+ start (work-item-cost item))))
        (setf (gethash (work-item-name item) earliest-starts) start)
        (setf (gethash (work-item-name item) earliest-finishes) finish)))
    (let* ((critical (reduce #'max topo
                             :key (lambda (item)
                                    (gethash (work-item-name item)
                                             earliest-finishes))
                             :initial-value 0d0))
           (latest-starts (%make-equal-table))
           (task-slack nil))
      (dolist (item (reverse topo))
        (let* ((name (work-item-name item))
               (succs (gethash name successors))
               (finish-bound (if succs
                                 (reduce #'min succs
                                         :key (lambda (succ)
                                                (gethash succ latest-starts))
                                         :initial-value critical)
                                 critical))
               (latest-start (- finish-bound (work-item-cost item)))
               (earliest-start (gethash name earliest-starts))
               (earliest-finish (gethash name earliest-finishes)))
          (setf (gethash name latest-starts) latest-start)
          (push (list :task name
                      :earliest-start earliest-start
                      :earliest-finish earliest-finish
                      :latest-start latest-start
                      :slack (- latest-start earliest-start))
                task-slack)))
      (%make-work-dag-analysis
       :total-cost total
       :critical-path-cost critical
       :available-parallelism (if (zerop critical)
                                  1d0
                                  (f64-ratio total critical))
       :max-frontier-width (reduce #'max batches :key #'length :initial-value 0)
       :frontier-widths (mapcar #'length batches)
       :task-slack (nreverse task-slack)))))

(defun causal-cone (items targets)
  "Return the backward dependency cone needed to compute TARGETS.

TARGETS is a task name or list of task names.  The returned WORK-CAUSAL-CONE
contains only tasks that can causally affect those targets through declared
dependencies.  Tasks outside the cone are listed under EXCLUDED, so callers can
prune unrelated work before planning or execution."
  (%assert-unique-names items)
  (let* ((target-names (%target-list targets))
         (items-by-name (%items-by-name items))
         (seen nil))
    (labels ((visit (name)
               (unless (gethash name items-by-name)
                 (error "causal-cone: unknown target or dependency ~S" name))
               (unless (member name seen :test #'equal)
                 (push name seen)
                 (dolist (dep (work-item-depends-on (gethash name items-by-name)))
                   (visit dep)))))
      (dolist (target target-names)
        (visit target)))
    (let* ((cone-items (remove-if-not (lambda (item)
                                        (%item-name-member-p item seen))
                                      items))
           (excluded (remove-if (lambda (item)
                                  (%item-name-member-p item seen))
                                items))
           (analysis (analyze-work-dag cone-items))
           (boundary (%causal-boundary cone-items seen)))
      (%make-work-causal-cone
       :targets target-names
       :tasks cone-items
       :boundary boundary
       :excluded excluded
       :total-cost (work-dag-analysis-total-cost analysis)
       :critical-path-cost (work-dag-analysis-critical-path-cost analysis)
       :frontier-widths (work-dag-analysis-frontier-widths analysis)))))

(defun %item-name-member-p (item names)
  (member (work-item-name item) names :test #'equal))

(defun causal-cone-plan (items targets &key (spawn-overhead 0d0)
                                      (min-speedup 1.05d0)
                                      max-workers)
  "Plan only the causal cone required for TARGETS.

Returns two values: the PARALLEL-PLAN for the cone and the WORK-CAUSAL-CONE
object used to build it."
  (let ((cone (causal-cone items targets)))
    (values (plan-work-schedule
             (work-causal-cone-tasks cone)
             :spawn-overhead spawn-overhead
             :min-speedup min-speedup
            :max-workers max-workers)
            cone)))

(defun affected-cone (items changes &key (change-kind :task))
  "Return the forward light cone affected by CHANGES.

With `:CHANGE-KIND :TASK`, CHANGES is a task name or list of task names.
With `:CHANGE-KIND :RESOURCE`, CHANGES is a resource name or list of resource
names, and every task reading or writing those resources becomes an initial
source.  The returned tasks are scheduleable in isolation: dependencies that
point outside the affected cone are removed from the task copies and recorded
under PREREQUISITES."
  (%assert-unique-names items)
  (let* ((items-by-name (%items-by-name items))
         (successors (%successors-by-name items))
         (change-list (%target-list changes))
         (source-names (%affected-source-names items items-by-name change-list change-kind))
         (seen nil))
    (labels ((visit (name)
               (unless (gethash name items-by-name)
                 (error "affected-cone: unknown affected task ~S" name))
               (unless (member name seen :test #'equal)
                 (push name seen)
                 (dolist (succ (gethash name successors))
                   (visit succ)))))
      (dolist (source source-names)
        (visit source)))
    (let* ((raw-tasks (remove-if-not (lambda (item)
                                       (%item-name-member-p item seen))
                                     items))
           (prerequisites (%external-prerequisites raw-tasks seen))
           (cone-items (%relax-external-dependencies raw-tasks seen))
           (excluded (remove-if (lambda (item)
                                  (%item-name-member-p item seen))
                                items))
           (analysis (analyze-work-dag cone-items))
           (boundary (%affected-boundary raw-tasks successors seen)))
      (%make-work-affected-cone
       :sources source-names
       :resources (if (eq change-kind :resource) change-list nil)
       :tasks cone-items
       :prerequisites prerequisites
       :boundary boundary
       :excluded excluded
       :total-cost (work-dag-analysis-total-cost analysis)
       :critical-path-cost (work-dag-analysis-critical-path-cost analysis)
       :frontier-widths (work-dag-analysis-frontier-widths analysis)))))

(defun work-cone-metrics (cone-or-items &key (kind :schedule) roots boundary)
  "Return compact light-cone metrics for a cone or raw work item list.

The result records enough structure for tensor lowering, control planners, and
diagnostics to decide whether a region is narrow, wide, serial, or worth
parallel scheduling without reimplementing cone analysis."
  (cond
    ((work-causal-cone-p cone-or-items)
     (%work-cone-metrics-from-items
      (work-causal-cone-tasks cone-or-items)
      :kind :causal
      :roots (work-causal-cone-targets cone-or-items)
      :boundary (work-causal-cone-boundary cone-or-items)
      :exterior-count (length (work-causal-cone-excluded cone-or-items))))
    ((work-affected-cone-p cone-or-items)
     (%work-cone-metrics-from-items
      (work-affected-cone-tasks cone-or-items)
      :kind :affected
      :roots (or (work-affected-cone-sources cone-or-items)
                 (work-affected-cone-resources cone-or-items))
      :boundary (work-affected-cone-boundary cone-or-items)
      :exterior-count (length (work-affected-cone-excluded cone-or-items))))
    ((listp cone-or-items)
     (%work-cone-metrics-from-items
      cone-or-items
      :kind kind
      :roots roots
      :boundary boundary
      :exterior-count 0))
    (t
     (error "work-cone-metrics: expected a cone or work item list, got ~S."
            cone-or-items))))

(defun affected-cone-plan (items changes &key (change-kind :task)
                                             (spawn-overhead 0d0)
                                             (min-speedup 1.05d0)
                                             max-workers)
  "Plan only the forward recomputation cone affected by CHANGES.

Returns two values: the PARALLEL-PLAN for the affected cone and the
WORK-AFFECTED-CONE object used to build it."
  (let ((cone (affected-cone items changes :change-kind change-kind)))
    (values (plan-work-schedule
             (work-affected-cone-tasks cone)
             :spawn-overhead spawn-overhead
             :min-speedup min-speedup
             :max-workers max-workers)
            cone)))

