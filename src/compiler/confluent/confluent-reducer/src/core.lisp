;;;; core.lisp --- the certified-parallel reducer.
;;;;
;;;; PIPELINE (one core-term in, one proved value out):
;;;;
;;;;   core-term let-chain            EXTRACT      per-cell op-stream
;;;;   (accumulator fragment)   ----------------->  [(:set/:add cell v) ...]
;;;;
;;;;   op-stream    CLASSIFY (rosette-confluence-frontier)   confluent BATCHES
;;;;             ------------------------------------->  + non-confluent FRONTIER
;;;;
;;;;   plan     SCHEDULE + FOLD (rosette-causal-frontier)    parallel value
;;;;         ------------------------------------------> == serial normal form
;;;;                                                     == rosette-core-term EVAL
;;;;
;;;;   schedule    SEAL     partition-invariance gauge cert (caf)
;;;;            --------->  + min-cut parallel-width report (rosette-causal-repartition)
;;;;            --------->  + re-runnable rosette-proof-witness master certificate
;;;;
;;;; The point of "parallel" here is PROVABLE ORDER-INDEPENDENCE (a gauge), not
;;;; OS threads: a confluent batch's ops commute, so any concurrent schedule
;;;; reaches the same partial result -- rosette-confluence-frontier decides exactly
;;;; where that holds, and rosette-causal-frontier's PARTITION-INVARIANCE seals that
;;;; the whole parallel schedule equals the serial normal form.

(in-package #:rosette-confluent-reducer)

;;; ---------------------------------------------------------------------------
;;; 0. name comparison (operators/vars are compared BY NAME, as core-term does).
;;; ---------------------------------------------------------------------------

(defun %name= (s name) (and (symbolp s) (string= (symbol-name s) name)))
(defun %cell-name (s) (string-downcase (symbol-name s)))

;;; ---------------------------------------------------------------------------
;;; 1. the extracted op the reducer schedules.
;;; ---------------------------------------------------------------------------

(defstruct (cred-op (:constructor make-cred-op (cell kind value)))
  "One accumulator update lifted to the rosette-causal-frontier / rosette-confluence-
frontier op algebra: KIND :ADD (reversible, commutes -- references the cell) or
:SET (absorbing/forgetting, does not commute -- ignores the cell), VALUE an
integer, on the string-named CELL."
  (cell "" :type string)
  (kind :add :type (member :add :set))
  (value 0 :type integer))

(defun %cf-op (o)
  "Project a CRED-OP onto rosette-confluence-frontier's single-cell op spec (its
classifier is per-cell, so callers only ever pass same-cell ops)."
  (cf:op (cred-op-kind o) (cred-op-value o)))

;;; ---------------------------------------------------------------------------
;;; 2. EXTRACT: core-term accumulator let-chain -> per-cell op-stream.
;;; ---------------------------------------------------------------------------
;;;
;;; The reducer's fragment is a nested let-chain whose innermost body combines
;;; the accumulator cells:
;;;
;;;   (let CELL UPDATE BODY)   with UPDATE one of
;;;       INTEGER              -> op (:set INTEGER)  -- ignores CELL (forgetting)
;;;       (+ CELL INTEGER)     -> op (:add INTEGER)  -- references CELL (reversible)
;;;       (+ INTEGER CELL)     -> op (:add INTEGER)
;;;
;;; and the innermost BODY is a cell symbol or (OP cellA cellB) combine.  Any
;;; other shape is OUTSIDE the reducer's fragment (a located wall: general pure
;;; core-term redexes are already fully confluent, handled by core-term's own
;;; normalizer -- see the README walls section).

(define-condition outside-fragment (error)
  ((form :initarg :form :reader outside-fragment-form))
  (:report (lambda (c s)
             (format s "rosette-confluent-reducer: ~S is outside the accumulator/op fragment"
                     (outside-fragment-form c)))))

(defun %update->op (cell-sym update)
  "Lift one let UPDATE for CELL-SYM into a CRED-OP (or signal OUTSIDE-FRAGMENT)."
  (let ((cell (%cell-name cell-sym)))
    (cond
      ((integerp update) (make-cred-op cell :set update))
      ((and (consp update) (%name= (first update) "+") (= (length update) 3))
       (destructuring-bind (a b) (rest update)
         (cond
           ((and (eq a cell-sym) (integerp b)) (make-cred-op cell :add b))
           ((and (eq b cell-sym) (integerp a)) (make-cred-op cell :add a))
           (t (error 'outside-fragment :form update)))))
      (t (error 'outside-fragment :form update)))))

(defun extract-ops (term)
  "Walk a core-term accumulator let-chain TERM.  Returns (values OPS BODY): OPS
is the list of CRED-OP in program order (interleaved cells preserved), BODY is
the innermost non-let combine term.  Signals OUTSIDE-FRAGMENT on any redex the
reducer's op algebra does not cover."
  (let ((ops '()))
    (loop
      (cond
        ((and (consp term) (%name= (first term) "LET") (= (length term) 4))
         (destructuring-bind (cell update body) (rest term)
           (unless (symbolp cell) (error 'outside-fragment :form term))
           (push (%update->op cell update) ops)
           (setf term body)))
        (t (return (values (nreverse ops) term)))))))

(defun %cells (ops) (remove-duplicates (mapcar #'cred-op-cell ops) :test #'string=))
(defun %cell-ops (ops cell)
  (remove cell ops :test-not #'string= :key #'cred-op-cell))

;;; ---------------------------------------------------------------------------
;;; 3. CLASSIFY: partition each cell's op-stream into confluent batches and the
;;;    non-confluent frontier, using rosette-confluence-frontier's OWN predicates.
;;; ---------------------------------------------------------------------------

(defstruct (reduction-plan (:constructor %make-plan (cell batches frontier)))
  "The confluence classification of one cell's op-stream.  BATCHES is a list of
maximal CONFLUENT runs (each a list of CRED-OP whose rosette-confluence-frontier
CONFLUENT-P holds -- fire concurrently, any order same partial result).
FRONTIER is the list of NON-CONFLUENT ops (each a `:set` that does not commute
with a neighbour -- the irreducibly-serial residue)."
  (cell "" :type string)
  (batches '() :type list)
  (frontier '() :type list))

(defun %confluent-with-p (batch op)
  "T iff appending OP keeps BATCH confluent, per rosette-confluence-frontier."
  (cf:confluent-p (mapcar #'%cf-op (append batch (list op)))))

(defparameter +probe+ (cf:op :add 1)
  "A witness op that always changes the cell (+1): if OP applied after it yields
the same value as OP alone, OP ERASED the +1 -- the forgetting / absorbing
signature (rosette-forgetting-frontier's information-destroyed test, read through
rosette-confluence-frontier's ORDER-RESULT).")

(defun %frontier-op-p (op)
  "T iff OP is ABSORBING (a forgetting `:set`): applying OP after a cell-changing
probe gives the same value as OP alone, so OP destroys prior state.  This is the
non-confluence boundary -- the irreducibly-serial residue -- located via
rosette-confluence-frontier ORDER-RESULT, not by reading the op's tag."
  (= (cf:order-result (list +probe+ (%cf-op op)))
     (cf:order-result (list (%cf-op op)))))

(defun classify (cell-ops)
  "Classify one cell's op-stream (CELL-OPS, program order) into a REDUCTION-PLAN.
Greedy left-to-right: extend the current batch while rosette-confluence-frontier
CONFLUENT-P holds; a `:set` that breaks confluence closes the batch and is a
frontier op.  Multi-op batches are the parallel-safe confluent redexes; the
frontier is located as the ops that do not commute with a neighbour."
  (let* ((cell (if cell-ops (cred-op-cell (first cell-ops)) ""))
         (batches '()) (current '()))
    (dolist (op cell-ops)
      (cond
        ((null current) (setf current (list op)))
        ((%confluent-with-p current op) (setf current (append current (list op))))
        (t (push (nreverse current) batches)
           (setf current (list op)))))
    (when current (push (nreverse current) batches))
    (%make-plan cell (nreverse batches)
                (remove-if-not #'%frontier-op-p cell-ops))))

(defun parallel-width (plan)
  "The parallel width of one cell's plan: the size of its widest confluent batch
= the most redexes that provably fire concurrently in a single wave."
  (reduce #'max (mapcar #'length (reduction-plan-batches plan)) :initial-value 0))

(defun non-confluent-frontier (plan) (reduction-plan-frontier plan))

;;; ---------------------------------------------------------------------------
;;; 4. SCHEDULE + FOLD via rosette-causal-frontier (parallel schedule == serial NF).
;;; ---------------------------------------------------------------------------

(defun %build-events (ops)
  "Lift OPS (global program order) to rosette-causal-frontier events: chained by
DEPS per cell so each cell's linearization respects program order, while
distinct cells stay causally independent (concurrent -- the cross-cell
parallelism).  HLC increases with the global index for a deterministic run."
  (let ((prev (make-hash-table :test 'equal)) (events '()))
    (loop for o in ops for i from 0
          for cell = (cred-op-cell o)
          for id = (format nil "e~D" i)
          for p = (gethash cell prev)
          do (push (caf:make-event :id id :hlc (caf:make-hlc (1+ i) 0)
                                   :deps (if p (list p) '())
                                   :target cell
                                   :kind (cred-op-kind o)
                                   :value (cred-op-value o))
                   events)
             (setf (gethash cell prev) id))
    (nreverse events)))

(defun %binop (name a b)
  (cond ((string= name "+") (+ a b)) ((string= name "-") (- a b))
        ((string= name "*") (* a b))
        (t (error 'outside-fragment :form name))))

(defun %eval-body (body cell-values)
  "Evaluate the innermost combine BODY given CELL-VALUES (alist cell-name->int)."
  (cond
    ((integerp body) body)
    ((symbolp body) (or (cdr (assoc (%cell-name body) cell-values :test #'string=))
                        (error 'outside-fragment :form body)))
    ((and (consp body) (= (length body) 3))
     (%binop (symbol-name (first body))
             (%eval-body (second body) cell-values)
             (%eval-body (third body) cell-values)))
    (t (error 'outside-fragment :form body))))

(defun %state->alist (state cells)
  (mapcar (lambda (c) (cons c (caf:cell-state state c))) cells))

(defun serial-normal-form (ops body)
  "The SERIAL normal form: apply rosette-causal-frontier's canonical linearization
(the deterministic serial fold) to OPS, then combine per BODY."
  (let* ((cells (%cells ops))
         (state (caf:causal-final-state (%build-events ops))))
    (%eval-body body (%state->alist state cells))))

(defun %serial-partition () (constantly "S"))
(defun %parallel-partition ()
  "Cross-cell parallelism: each cell runs on its own arbiter (its own name).
A function of TARGET, hence cell-respecting -- the precondition of the gauge."
  (lambda (target) target))

(defun parallel-schedule-value (ops body)
  "The PARALLEL-schedule value: rosette-causal-frontier ADJUDICATE with each cell on
its own arbiter (cells run concurrently across arbiters), then combine per BODY."
  (let* ((cells (%cells ops))
         (state (caf:adjudicate (%build-events ops) (%parallel-partition))))
    (%eval-body body (%state->alist state cells))))

(defun partition-gauge-certificate (ops)
  "T iff the parallel (per-cell arbiter) schedule adjudicates to the SAME per-cell
state as the serial (single arbiter) schedule -- rosette-causal-frontier PARTITION-
INVARIANT-P.  This is the gauge seal: repartitioning the confluent computation
across arbiters cannot change the logical result IFF no cell's op-stream is split
(both partitions here key on TARGET, hence are cell-respecting)."
  (let ((events (%build-events ops)))
    (and (caf:one-target-one-arbiter-p (%parallel-partition) events)
         (caf:partition-invariant-p events (%serial-partition) (%parallel-partition)))))

;;; ---------------------------------------------------------------------------
;;; 5. rosette-causal-repartition: quantify the CROSS-CELL parallel width (min-cut).
;;; ---------------------------------------------------------------------------

(defun %body-cells (body)
  (cond ((symbolp body) (list (%cell-name body)))
        ((consp body) (append (%body-cells (second body)) (%body-cells (third body))))
        (t '())))

(defun %interaction-graph (ops body)
  "Build the causal-interaction graph over OP NODES: an intra-cell serialization
edge (weight 1) between consecutive same-cell ops (they must run in causal order
on one arbiter), and a cross-cell coupling edge (weight 1) for each pair of cells
the BODY combines (they meet once at the combine).  The min-cut then separates
the independent cells -- the cross-cell parallel split."
  (let* ((ids (loop for i below (length ops) collect (format nil "e~D" i)))
         (edges '()) (prev (make-hash-table :test 'equal))
         (last-per-cell (make-hash-table :test 'equal)))
    (loop for o in ops for id in ids
          for cell = (cred-op-cell o)
          for p = (gethash cell prev)
          do (when p (push (list p id 1) edges))
             (setf (gethash cell prev) id
                   (gethash cell last-per-cell) id))
    ;; couple the cells the body combines, at their last ops
    (let ((body-cells (%body-cells body)))
      (loop for (a . rest) on body-cells do
        (dolist (b rest)
          (let ((ia (gethash a last-per-cell)) (ib (gethash b last-per-cell)))
            (when (and ia ib) (push (list ia ib 1) edges))))))
    (rep:make-interaction-graph ids edges)))

(defun repartition-report (ops body &key (lambda 3/2))
  "Report rosette-causal-repartition's min-cut verdict on the op graph at latency
ratio LAMBDA: (values SHOULD-SPLIT-P BEST-SUBSET BEST-GAIN CROSS-CELL-WIDTH).
CROSS-CELL-WIDTH is the number of independent cells that can run concurrently
across arbiters -- the coarse parallel width the physical layer buys."
  (let ((g (%interaction-graph ops body)))
    (multiple-value-bind (subset gain) (rep:best-cut g lambda)
      (values (rep:should-split-p g lambda) subset gain (length (%cells ops))))))

;;; ---------------------------------------------------------------------------
;;; 6. THE REDUCER: reduce == eval, with the plan and the width.
;;; ---------------------------------------------------------------------------

(defun confluent-reduce (term)
  "Reduce a core-term accumulator let-chain TERM by firing its confluent redex
batches in parallel and serializing only across the non-confluent frontier.
Returns (values VALUE PLANS OPS BODY) where VALUE is the parallel-schedule
normal form and PLANS is the per-cell REDUCTION-PLAN list.  VALUE equals both
the serial normal form and rosette-core-term's own EVAL of TERM (asserted by the
certificate)."
  (multiple-value-bind (ops body) (extract-ops term)
    (let ((plans (mapcar (lambda (c) (classify (%cell-ops ops c))) (%cells ops))))
      (values (parallel-schedule-value ops body) plans ops body))))

(defun reduce-result (term) (nth-value 0 (confluent-reduce term)))

;;; ---------------------------------------------------------------------------
;;; 7. THE MASTER CERTIFICATE: normalizing (in parallel) PRODUCES the proof.
;;; ---------------------------------------------------------------------------

(defun %total-parallel-width (ops body)
  "The peak concurrent width: independent cells each contribute their widest
confluent batch in the same wave, so the peak wave fires the SUM of the per-cell
first-batch widths -- the falsifiable `there is real order-independence' number."
  (declare (ignore body))
  (loop for c in (%cells ops)
        sum (parallel-width (classify (%cell-ops ops c)))))

(defun confluent-reduce-certificate (program)
  "Reduce PROGRAM's MAIN and SEAL the whole run into a re-runnable rosette-proof-
witness certificate.  The certificate PASSES iff, on the sealed term:
  (1) the parallel-schedule value == the serial normal form (gauge value-match);
  (2) that value == rosette-core-term's own PROGRAM-EVAL (reduce == eval);
  (3) it also == core-term's NORMALIZER normal form (reduce == normalize);
  (4) the partition-invariance gauge certificate holds (no op-stream split).
Its WITNESS re-derives all four from the sealed term, so a tampered value, a
tampered width, or a broken gauge all fail RUNTIME-VERIFY."
  (let* ((term (ct:core-program-main program)))
    (multiple-value-bind (pval plans ops body) (confluent-reduce term)
      (let* ((serial (serial-normal-form ops body))
             (evalv (ct:program-eval program))
             (normv (nth-value 0 (ct:program-normalize program)))
             (gauge (partition-gauge-certificate ops))
             (width (%total-parallel-width ops body))
             (frontier (loop for p in plans append (reduction-plan-frontier p)))
             (passed (and (eql pval serial) (eql pval evalv) (eql pval normv) gauge))
             (payload (list :term term
                            :parallel-value pval :serial-value serial
                            :eval-value evalv :normalize-value normv
                            :gauge gauge :parallel-width width
                            :frontier-count (length frontier)
                            :cells (%cells ops) :n-ops (length ops)))
             (cert (pw:make-certificate
                    :name :confluent-reduce
                    :kind :proof
                    :claim :parallel-schedule-is-a-gauge
                    :payload payload
                    :passed passed)))
        (setf (pw:certificate-witness cert)
              (pw:make-lean-witness
               :confluent-reduce-re-runs
               (lambda () (%recheck-certificate cert))))
        cert))))

(defun %recheck-certificate (cert)
  "Independently re-run the full reduce/eval/normalize/gauge pipeline from the
term sealed in CERT and require the recomputed verdicts to equal the sealed ones.
Returns T iff the certificate is honest."
  (handler-case
      (let* ((pl (pw:certificate-payload cert))
             (term (getf pl :term))
             (program (ct:make-core-program :funs nil :main term)))
        (multiple-value-bind (pval plans ops body) (confluent-reduce term)
          (declare (ignore plans))
          (let ((serial (serial-normal-form ops body))
                (evalv (ct:program-eval program))
                (normv (nth-value 0 (ct:program-normalize program)))
                (gauge (partition-gauge-certificate ops))
                (width (%total-parallel-width ops body)))
            (and (eql pval (getf pl :parallel-value))
                 (eql pval serial)
                 (eql pval evalv)
                 (eql pval normv)
                 (eql evalv (getf pl :eval-value))
                 (eql normv (getf pl :normalize-value))
                 (eql gauge (getf pl :gauge)) gauge
                 (eql width (getf pl :parallel-width))))))
    (error () nil)))

(defun verify-confluent-reduce-certificate (cert)
  "Re-run CERT through rosette-proof-witness RUNTIME-VERIFY (fires the installed
witness).  A forged/tampered certificate returns NIL."
  (pw:runtime-verify cert))
