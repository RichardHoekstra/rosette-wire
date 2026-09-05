;;;; scheduler-internals.lisp --- private scheduler helpers.

(in-package #:rosette-work-scheduler)

(defun %canonical-set (items)
  (let ((result nil))
    (dolist (item items (nreverse result))
      (pushnew item result :test #'equal))))

(defun %commuting-p (a b)
  (or (member (work-item-name b) (work-item-commutes-with a) :test #'equal)
      (member (work-item-name a) (work-item-commutes-with b) :test #'equal)))

(defun %copy-work-item (item &key
                               (name (work-item-name item))
                               (reads (work-item-reads item))
                               (writes (work-item-writes item))
                               (cost (work-item-cost item))
                               (depends-on (work-item-depends-on item))
                               (commutes-with (work-item-commutes-with item))
                               (metadata (work-item-metadata item)))
  (make-work-item :name name
                  :reads reads
                  :writes writes
                  :cost cost
                  :depends-on depends-on
                  :commutes-with commutes-with
                  :metadata metadata))

(defun %work-cone-metrics-from-items (items &key kind roots boundary exterior-count)
  (let* ((analysis (analyze-work-dag items))
         (total (work-dag-analysis-total-cost analysis))
         (critical (work-dag-analysis-critical-path-cost analysis)))
    (%make-work-cone-metrics
     :kind kind
     :roots (copy-list roots)
     :boundary (copy-list boundary)
     :task-count (length items)
     :exterior-count exterior-count
     :total-cost total
     :critical-path-cost critical
     :available-parallelism (work-dag-analysis-available-parallelism analysis)
     :max-frontier-width (work-dag-analysis-max-frontier-width analysis)
     :frontier-widths (work-dag-analysis-frontier-widths analysis)
     :serial-fraction (if (zerop total)
                          0d0
                          (f64-ratio critical total)))))

(defun %depends-on-p (a b)
  (member (work-item-name b) (work-item-depends-on a) :test #'equal))

(defun %dependencies-done-p (item done)
  (every (lambda (name) (member name done :test #'equal))
         (work-item-depends-on item)))

(defun %target-list (targets)
  (if (and (listp targets)
           (not (keywordp targets)))
      (copy-list targets)
      (list targets)))

(defun %assert-unique-names (items)
  (let ((seen nil))
    (dolist (item items)
      (when (member (work-item-name item) seen :test #'equal)
        (error "parallel-batches: duplicate work-item name ~S"
               (work-item-name item)))
      (push (work-item-name item) seen))))

(defun %items-by-name (items)
  (let ((table (%make-equal-table)))
    (dolist (item items table)
      (let ((name (work-item-name item)))
        (when (gethash name table)
          (error "duplicate work-item name ~S" name))
        (setf (gethash name table) item)))))

(defun %successors-by-name (items)
  (let ((table (%make-equal-table)))
    (dolist (item items table)
      (setf (gethash (work-item-name item) table)
            (or (gethash (work-item-name item) table) nil))
      (dolist (dep (work-item-depends-on item))
        (push (work-item-name item) (gethash dep table))))))

(defun %causal-boundary (cone-items cone-names)
  (let ((successors (%successors-by-name cone-items))
        (boundary nil))
    (dolist (item cone-items (nreverse boundary))
      (let ((succs (gethash (work-item-name item) successors)))
        (when (or (null succs)
                  (some (lambda (succ)
                          (not (member succ cone-names :test #'equal)))
                        succs))
          (push (work-item-name item) boundary))))))

(defun %affected-source-names (items items-by-name changes change-kind)
  (case change-kind
    (:task
     (dolist (name changes)
       (unless (gethash name items-by-name)
         (error "affected-cone: unknown source task ~S" name)))
     changes)
    (:resource
     (let ((sources nil))
       (dolist (item items (nreverse sources))
         (when (intersection changes
                             (append (work-item-reads item)
                                     (work-item-writes item))
                             :test #'equal)
           (push (work-item-name item) sources)))))
    (otherwise
     (error "affected-cone: CHANGE-KIND must be :TASK or :RESOURCE, not ~S."
            change-kind))))

(defun %external-prerequisites (items cone-names)
  (let ((prereqs nil))
    (dolist (item items (nreverse prereqs))
      (dolist (dep (work-item-depends-on item))
        (unless (member dep cone-names :test #'equal)
          (pushnew dep prereqs :test #'equal))))))

(defun %relax-external-dependencies (items cone-names)
  (mapcar (lambda (item)
            (make-work-item
             :name (work-item-name item)
             :reads (work-item-reads item)
             :writes (work-item-writes item)
             :cost (work-item-cost item)
             :depends-on (remove-if-not (lambda (dep)
                                           (member dep cone-names :test #'equal))
                                         (work-item-depends-on item))
             :commutes-with (work-item-commutes-with item)
             :metadata (work-item-metadata item)))
          items))

(defun %affected-boundary (cone-items successors cone-names)
  (let ((boundary nil))
    (dolist (item cone-items (nreverse boundary))
      (let ((succs (gethash (work-item-name item) successors)))
        (when (or (null succs)
                  (every (lambda (succ)
                           (not (member succ cone-names :test #'equal)))
                         succs))
          (push (work-item-name item) boundary))))))

(defun %downstream-cost (item successors items-by-name)
  (let ((seen nil))
    (labels ((visit (name)
               (if (member name seen :test #'equal)
                   0d0
                   (progn
                     (push name seen)
                     (let ((child (gethash name items-by-name)))
                       (+ (if child (work-item-cost child) 0d0)
                          (reduce #'+ (gethash name successors)
                                  :key #'visit
                                  :initial-value 0d0)))))))
      (reduce #'+ (gethash (work-item-name item) successors)
              :key #'visit
              :initial-value 0d0))))

(defun %communication-footprint-cost (items)
  (reduce #'+ items
          :key (lambda (item)
                 (+ (length (work-item-reads item))
                    (length (work-item-writes item))))
          :initial-value 0d0))

(defun %parallelism-pressure (analysis)
  (let ((widths (work-dag-analysis-frontier-widths analysis)))
    (reduce #'+ widths
            :key (lambda (width)
                   (max 0 (1- width)))
            :initial-value 0d0)))

(defun %bottleneck-row< (a b)
  (or (> (getf a :score) (getf b :score))
      (and (= (getf a :score) (getf b :score))
           (string< (prin1-to-string (getf a :task))
                    (prin1-to-string (getf b :task))))))

(defun %work-action-choice< (a b)
  (let ((a-value (work-action-score-value (work-action-choice-score a)))
        (b-value (work-action-score-value (work-action-choice-score b))))
    (or (< a-value b-value)
        (and (= a-value b-value)
             (string< (prin1-to-string (work-action-choice-name a))
                      (prin1-to-string (work-action-choice-name b)))))))

(defun %bottleneck->hint (row)
  (let* ((critical-p (getf row :critical-p))
         (fanout (getf row :fanout))
         (slack (getf row :slack))
         (downstream (getf row :downstream-cost))
         (action (cond
                   ((and critical-p (> fanout 1))
                    :split-shared-predecessor)
                   ((and critical-p (plusp downstream))
                    :cache-or-hoist-critical-output)
                   (critical-p
                    :optimize-critical-task)
                   ((plusp slack)
                    :defer-or-batch-slack-work)
                   (t
                    :inspect-task)))
         (reason (case action
                   (:split-shared-predecessor
                    :critical-fanout)
                   (:cache-or-hoist-critical-output
                    :critical-downstream-chain)
                   (:optimize-critical-task
                    :critical-leaf)
                   (:defer-or-batch-slack-work
                    :positive-slack)
                   (otherwise
                    :unclassified))))
    (list :task (getf row :task)
          :action action
          :reason reason
          :cost (getf row :cost)
          :critical-p critical-p
          :slack slack
          :fanout fanout
          :downstream-cost downstream
          :score (getf row :score))))

(defun %batch-makespan (batch max-workers)
  (reduce #'max (%batch-lanes batch max-workers)
          :key (lambda (lane)
                 (reduce #'+ lane :key #'work-item-cost :initial-value 0d0))
          :initial-value 0d0))

(defun %batch-lanes (batch max-workers)
  (cond
    ((null batch) nil)
    ((null max-workers)
     (mapcar #'list batch))
    (t
     (let* ((lane-count (min max-workers (length batch)))
            (lanes (make-array lane-count :initial-element nil))
            (loads (make-array lane-count :initial-element 0d0))
            (ranked (sort (copy-list batch)
                          (lambda (a b)
                            (if (= (work-item-cost a) (work-item-cost b))
                                (string< (prin1-to-string (work-item-name a))
                                         (prin1-to-string (work-item-name b)))
                                (> (work-item-cost a) (work-item-cost b)))))))
       (dolist (item ranked)
         (let ((best 0))
           (loop for i from 1 below lane-count
                 when (< (aref loads i) (aref loads best))
                   do (setf best i))
           (incf (aref loads best) (work-item-cost item))
           (setf (aref lanes best) (append (aref lanes best) (list item)))))
       (loop for i below lane-count collect (aref lanes i))))))

(defun %plan-reasons (batches serial parallel speedup profitable-p spawn-overhead min-speedup max-workers)
  (let ((multi (count-if (lambda (batch) (> (length batch) 1)) batches)))
    (list :batch-count (length batches)
          :parallel-batch-count multi
          :serial-cost serial
          :parallel-cost parallel
          :spawn-overhead spawn-overhead
          :max-workers max-workers
          :min-speedup min-speedup
          :speedup speedup
          :decision (if profitable-p
                        :parallelize
                        :keep-serial))))

(defun %batches->names (batches)
  (mapcar (lambda (batch)
            (mapcar #'work-item-name batch))
          batches))

(defun %batches-from-names (items batch-names)
  (let ((table (%items-by-name items))
        (reasons nil)
        (batches nil))
    (dolist (batch batch-names)
      (let ((rebuilt nil))
        (dolist (name batch)
          (let ((item (gethash name table)))
            (if item
                (push item rebuilt)
                (push (list :kind :unknown-task :task name) reasons))))
        (push (nreverse rebuilt) batches)))
    (values (nreverse batches) (nreverse reasons))))

(defun %action-choices->score-rows (choices)
  (mapcar (lambda (choice)
            (let ((score (work-action-choice-score choice)))
              (list :name (work-action-choice-name choice)
                    :value (work-action-score-value score)
                    :compute-cost (work-action-score-compute-cost score)
                    :critical-path-cost (work-action-score-critical-path-cost score)
                    :communication-cost (work-action-score-communication-cost score)
                    :parallelism-pressure (work-action-score-parallelism-pressure score))))
          choices))

(defun %action-weight-plist (compute critical-path communication parallelism-pressure)
  (list :compute compute
        :critical-path critical-path
        :communication communication
        :parallelism-pressure parallelism-pressure))

(defun %assert-temperature (temperature caller)
  (unless (and (realp temperature) (plusp temperature))
    (error "~A: TEMPERATURE must be a positive real." caller)))

(defun %choices->ensemble (choices temperature metadata)
  (let* ((values (mapcar (lambda (choice)
                           (as-f64 (work-action-score-value
                                    (work-action-choice-score choice))))
                         choices))
         (minimum (and values (reduce #'min values)))
         (shifted-weights (mapcar (lambda (value)
                                    (exp (/ (- minimum value)
                                            (as-f64 temperature))))
                                  values))
         (shifted-z (reduce #'+ shifted-weights :initial-value 0d0))
         (probabilities
           (mapcar (lambda (choice value weight)
                     (list :name (work-action-choice-name choice)
                           :probability (if (zerop shifted-z)
                                            0d0
                                            (/ weight shifted-z))
                           :score value))
                   choices values shifted-weights))
         (log-partition (if (or (null values) (zerop shifted-z))
                            0d0
                            (+ (/ (- minimum)
                                  (as-f64 temperature))
                               (log shifted-z))))
         (free-energy (if (null values)
                          0d0
                          (* (- (as-f64 temperature))
                             log-partition))))
    (%make-work-action-ensemble
     :temperature temperature
     :choices choices
     :probabilities probabilities
     :free-energy free-energy
     :log-partition log-partition
     :winner (first choices)
     :metadata metadata)))

;;;; ----------------------------------------------------------- graph interop

(defun work-items->csr (items)
  "Project ITEMS into a directed ROSETTE-GRAPH-CORE:CSR-GRAPH.

Vertex i corresponds to the i-th item; out-edges go from each item to
the indices of its declared DEPENDS-ON predecessors.  Unknown
dependency names are skipped silently so the projection works on
partial item sets (use VERIFY-PARALLEL-PLAN for strict checking).

Returns three values: the CSR-GRAPH, the simple-vector of work-item
names indexed by vertex id, and the hash-table mapping each name (under
EQUAL) to its vertex id."
  (let* ((n        (length items))
         (name-vec (make-array n))
         (name->id (%make-equal-table))
         (edges    nil))
    (loop for item in items
          for i from 0
          do (setf (svref name-vec i) (work-item-name item)
                   (gethash (work-item-name item) name->id) i))
    (loop for item in items
          for i from 0
          do (dolist (dep (work-item-depends-on item))
               (let ((j (gethash dep name->id)))
                 (when j
                   (push (cons i j) edges)))))
    (values (rosette-graph-core:csr-from-edges n (nreverse edges) :directed-p t)
            name-vec
            name->id)))

(defun work-items-dependency-cycles (items)
  "Return non-trivial dependency cycles in ITEMS as lists of work-item names.

Uses ROSETTE-GRAPH-CORE's strongly-connected-components primitive over the
dependency graph projection.  A cycle is any SCC with more than one
vertex (true strongly-connected loops); single-vertex SCCs without
self-loops are skipped.  Returns NIL when the dependency graph is
already a DAG."
  (multiple-value-bind (graph name-vec name->id) (work-items->csr items)
    (declare (ignore name->id))
    (multiple-value-bind (comp-id components)
        (rosette-graph-core:csr-strongly-connected-components graph)
      (declare (ignore comp-id))
      (let ((cycles nil))
        (dolist (component components)
          (when (> (length component) 1)
            (push (mapcar (lambda (vid) (svref name-vec vid)) component)
                  cycles)))
        (nreverse cycles)))))
