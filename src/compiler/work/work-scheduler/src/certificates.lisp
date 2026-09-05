;;;; certificates.lisp --- schedule bottlenecks, hints, verification, and certificates.

(in-package #:rosette-work-scheduler)

(defun schedule-bottlenecks (items &key max-count)
  "Return ranked task bottlenecks for a dependency-safe schedule.

Rows are plists containing `:TASK`, `:COST`, `:CRITICAL-P`, `:SLACK`,
`:FANOUT`, `:DOWNSTREAM-COST`, and `:SCORE`.  Critical zero-slack tasks rank
first, then tasks with larger downstream cost and own cost.  MAX-COUNT limits
the returned rows without changing their deterministic order."
  (when (and max-count
             (or (not (integerp max-count))
                 (minusp max-count)))
    (error "schedule-bottlenecks: MAX-COUNT must be NIL or a non-negative integer."))
  (let* ((analysis (analyze-work-dag items))
         (slack-by-name (%make-equal-table))
         (successors (%successors-by-name items))
         (rows nil))
    (dolist (row (work-dag-analysis-task-slack analysis))
      (setf (gethash (getf row :task) slack-by-name)
            (getf row :slack)))
    (dolist (item items)
      (let* ((name (work-item-name item))
             (slack (or (gethash name slack-by-name) 0d0))
             (critical-p (zerop slack))
             (downstream-cost (%downstream-cost item successors (%items-by-name items)))
             (fanout (length (gethash name successors)))
             (score (+ (if critical-p
                           (work-dag-analysis-critical-path-cost analysis)
                           0d0)
                       downstream-cost
                       (work-item-cost item))))
        (push (list :task name
                    :cost (work-item-cost item)
                    :critical-p critical-p
                    :slack slack
                    :fanout fanout
                    :downstream-cost downstream-cost
                    :score score)
              rows)))
    (let ((ranked (sort (nreverse rows) #'%bottleneck-row<)))
      (if max-count
          (subseq ranked 0 (min max-count (length ranked)))
          ranked))))

(defun schedule-optimization-hints (items &key max-count)
  "Return deterministic optimization hints derived from schedule bottlenecks.

Each row is a plist containing `:TASK`, `:ACTION`, `:REASON`, and the
underlying bottleneck metrics.  The function is intentionally advisory: it
does not rewrite work descriptors, but it gives callers a stable first pass
for deciding whether to split, cache, optimize, defer, or batch work."
  (mapcar #'%bottleneck->hint
          (schedule-bottlenecks items :max-count max-count)))

(defun verify-parallel-plan (items plan-or-batches)
  "Verify that PLAN-OR-BATCHES is a valid schedule for ITEMS.

Returns two values: a boolean and a list of reasons.  Reasons are plists with
`:KIND` naming the violated invariant.  The verifier intentionally accepts
either a PARALLEL-PLAN or raw batches so external planners can emit a compact
certificate and use this library only as the checker."
  (let* ((batches (if (typep plan-or-batches 'parallel-plan)
                      (parallel-plan-batches plan-or-batches)
                      plan-or-batches))
         (items-by-name (%items-by-name items))
         (scheduled nil)
         (done nil)
         (reasons nil))
    (labels ((fail (kind &rest plist)
               (push (list* :kind kind plist) reasons)))
      (dolist (batch batches)
        (let ((batch-names nil))
          (dolist (item batch)
            (let ((name (work-item-name item)))
              (cond
                ((not (gethash name items-by-name))
                 (fail :unknown-task :task name))
                ((member name scheduled :test #'equal)
                 (fail :duplicate-task :task name)))
              (when (member name batch-names :test #'equal)
                (fail :duplicate-task-in-batch :task name))
              (push name batch-names)
              (push name scheduled)
              (dolist (dep (work-item-depends-on item))
                (unless (member dep done :test #'equal)
                  (fail :dependency-not-ready :task name :dependency dep)))))
          (loop for tail on batch do
            (loop for other in (rest tail)
                  for item = (first tail)
                  do (dolist (conflict (work-conflicts item other))
                       (fail :same-batch-conflict
                             :left (work-conflict-left conflict)
                             :right (work-conflict-right conflict)
                             :resource (work-conflict-resource conflict)
                             :conflict-kind (work-conflict-kind conflict)))))
          (dolist (name batch-names)
            (pushnew name done :test #'equal))))
      (dolist (item items)
        (unless (member (work-item-name item) scheduled :test #'equal)
          (fail :unscheduled-task :task (work-item-name item)))))
    (%finish-reasons reasons)))

(defun schedule-certificate (items plan-or-batches &key (name :work-schedule) metadata)
  "Return a proof-witness certificate for PLAN-OR-BATCHES over ITEMS.

The certificate payload stores only task names, costs, and verifier reasons,
so it is stable enough for logs and external planners.  Verification still
reconstructs the schedule from ITEMS and reruns VERIFY-PARALLEL-PLAN; the
PASSED bit is not trusted as proof."
  (multiple-value-bind (ok reasons)
      (verify-parallel-plan items plan-or-batches)
    (let* ((plan-p (typep plan-or-batches 'parallel-plan))
           (batches (if plan-p
                        (parallel-plan-batches plan-or-batches)
                        plan-or-batches))
           (payload (append
                     (list :items (mapcar #'%work-item-signature items)
                           :batches (%batches->names batches)
                           :reasons reasons)
                     (when plan-p
                       (list :serial-cost (parallel-plan-serial-cost plan-or-batches)
                             :parallel-cost (parallel-plan-parallel-cost plan-or-batches)
                             :speedup (parallel-plan-speedup plan-or-batches)
                             :profitable-p (parallel-plan-profitable-p plan-or-batches)
                             :bottlenecks (schedule-bottlenecks items))))))
      (rosette-proof-witness:make-certificate
       :name name
       :kind :work-schedule
       :claim :valid-parallel-schedule
       :payload payload
       :passed ok
       :metadata metadata))))

(defun verify-schedule-certificate (items certificate)
  "Recheck a schedule certificate against ITEMS.

Returns two values: a boolean and a list of verifier reasons.  This function
does not trust CERTIFICATE-PASSED; it treats the stored batch-name payload as
the claim and verifies that claim against the current work descriptors."
  (check-type certificate rosette-proof-witness:certificate)
  (let* ((payload (rosette-proof-witness:certificate-payload certificate))
         (batch-names (getf payload :batches)))
    (unless batch-names
      (return-from verify-schedule-certificate
        (values nil (list (list :kind :missing-payload :field :batches)))))
    (multiple-value-bind (signature-ok signature-reasons)
        (%verify-item-signatures items (getf payload :items))
      (unless signature-ok
        (return-from verify-schedule-certificate
          (values nil signature-reasons))))
    (multiple-value-bind (batches reasons)
        (%batches-from-names items batch-names)
      (if reasons
          (values nil reasons)
          (verify-parallel-plan items batches)))))

