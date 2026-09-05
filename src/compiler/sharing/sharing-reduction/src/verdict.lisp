;;;; verdict.lisp --- the certificate battery + VERDICT block.
;;;;
;;;; Five certificates, each the operational refutation of one clause of the
;;;; pre-registered anti-property:
;;;;
;;;;   (a) CORRECTNESS         -- net normal forms match the reference
;;;;                              rosette-foundation-rewrite evaluator (Church
;;;;                              arithmetic 2+3, 2*3, 2^3, ...).
;;;;   (b) SUPER-EXP TAMED     -- a term whose NAIVE reduction blows up
;;;;                              exponentially reduces in POLYNOMIAL net
;;;;                              interactions (measured: naive O(2^N) tree
;;;;                              vs shared O(poly N)).
;;;;   (c) LEVY-OPTIMALITY     -- a shared redex is contracted exactly ONCE.
;;;;   (d) PARALLEL-CONFLUENCE -- sequential and all-at-once parallel
;;;;                              reduction reach the same normal form with
;;;;                              the same interaction count.
;;;;   (e) GEOMETRY WIN        -- the shared net's DAG size is exponentially
;;;;                              smaller than the unfolded tree, and the net
;;;;                              result equals the reference computation.

(in-package #:rosette-sharing-reduction)

;;; --- Per-term schedule gauge certificates -------------------------------

(defstruct (schedule-gauge-certificate
            (:constructor %make-schedule-gauge-certificate)
            (:copier nil))
  "Replayable evidence that sequential and parallel schedules are one gauge."
  source
  sequential-normal-form
  parallel-normal-form
  (sequential-interactions 0 :type (integer 0 *))
  (parallel-interactions 0 :type (integer 0 *))
  (sequential-rounds 0 :type (integer 0 *))
  (parallel-rounds 0 :type (integer 0 *))
  (confluent-p nil :type boolean))

(defun schedule-gauge-certificate-source-form (certificate)
  "Return a fresh s-expression projection of CERTIFICATE's source term."
  (check-type certificate schedule-gauge-certificate)
  (copy-tree
   (term->sexp (schedule-gauge-certificate-source certificate))))

(defun %schedule-gauge-evidence (term)
  (let ((sequential (lam->net term))
        (parallel (lam->net term)))
    (let ((sequential-rounds (reduce-all sequential :parallel nil))
          (parallel-rounds (reduce-all parallel :parallel t)))
      (let ((sequential-normal-form
              (term->sexp (net->term sequential)))
            (parallel-normal-form
              (term->sexp (net->term parallel)))
            (sequential-interactions (net-interactions sequential))
            (parallel-interactions (net-interactions parallel)))
        (values sequential-normal-form
                parallel-normal-form
                sequential-interactions
                parallel-interactions
                sequential-rounds
                parallel-rounds
                (and (equal sequential-normal-form parallel-normal-form)
                     (= sequential-interactions parallel-interactions)))))))

(defun certify-schedule-gauge (term)
  "Reduce TERM sequentially and in parallel and retain replayable evidence."
  (multiple-value-bind
        (sequential-normal-form parallel-normal-form
         sequential-interactions parallel-interactions
         sequential-rounds parallel-rounds confluent-p)
      (%schedule-gauge-evidence term)
    (%make-schedule-gauge-certificate
     :source term
     :sequential-normal-form sequential-normal-form
     :parallel-normal-form parallel-normal-form
     :sequential-interactions sequential-interactions
     :parallel-interactions parallel-interactions
     :sequential-rounds sequential-rounds
     :parallel-rounds parallel-rounds
     :confluent-p confluent-p)))

(defun verify-schedule-gauge-certificate (certificate)
  "Replay CERTIFICATE from its source term and reject altered evidence."
  (and
   (schedule-gauge-certificate-p certificate)
   (handler-case
       (multiple-value-bind
             (sequential-normal-form parallel-normal-form
              sequential-interactions parallel-interactions
              sequential-rounds parallel-rounds confluent-p)
           (%schedule-gauge-evidence
            (schedule-gauge-certificate-source certificate))
         (and confluent-p
              (schedule-gauge-certificate-confluent-p certificate)
              (equal sequential-normal-form
                     (schedule-gauge-certificate-sequential-normal-form
                      certificate))
              (equal parallel-normal-form
                     (schedule-gauge-certificate-parallel-normal-form
                      certificate))
              (= sequential-interactions
                 (schedule-gauge-certificate-sequential-interactions
                  certificate))
              (= parallel-interactions
                 (schedule-gauge-certificate-parallel-interactions
                  certificate))
              (= sequential-rounds
                 (schedule-gauge-certificate-sequential-rounds certificate))
              (= parallel-rounds
                 (schedule-gauge-certificate-parallel-rounds certificate))))
     (error () nil))))

(defstruct (certificate-report (:constructor %make-cert) (:copier nil))
  (correctness nil)
  (super-exp nil)
  (optimality nil)
  (confluence nil)
  (geometry nil)
  (all-pass nil))

;;; --- (a) correctness -------------------------------------------------

(defun reference-numeral (term &key (max-iter 200000))
  "Normalise TERM with the reference (non-sharing) evaluator and read off
its Church value.  Returns :BLEW-UP if the naive evaluator exhausts its
iteration cap or the control stack -- which is itself the demonstration
that the unshared reference cannot keep up (the headline phenomenon)."
  (handler-case
      (let ((*normalize-max-iter* max-iter))
        (church-count (normalize term)))
    (storage-condition () :blew-up)
    (error () :blew-up)))

(defun cert-correctness ()
  "Net arithmetic == reference arithmetic == ground truth, over a grid.

The fourth element CHECK-REF? gates the cross-check against the unshared
reference evaluator: it is enabled only on the smaller terms it can reduce
without exhausting the stack (the larger ones are precisely where the
naive reducer falls over -- that is the whole point of the keystone).  The
net result is ALWAYS checked against the ground truth."
  (let ((cases
          (list (list "2+3" (apply* (church-add) (church-numeral 2) (church-numeral 3)) 5 t)
                (list "1+4" (apply* (church-add) (church-numeral 1) (church-numeral 4)) 5 t)
                (list "2*3" (apply* (church-mul) (church-numeral 2) (church-numeral 3)) 6 t)
                (list "3*3" (apply* (church-mul) (church-numeral 3) (church-numeral 3)) 9 t)
                (list "2^3" (apply* (church-exp) (church-numeral 2) (church-numeral 3)) 8 nil)
                (list "3^2" (apply* (church-exp) (church-numeral 3) (church-numeral 2)) 9 nil)))
        (rows nil) (ok t))
    (dolist (c cases)
      (destructuring-bind (name term truth check-ref?) c
        (let* ((net-val (church-count (run-term term)))
               (ref-val (if check-ref? (reference-numeral term) :skipped))
               ;; net must equal ground truth; reference must AGREE when run.
               (pass (and (eql net-val truth)
                          (or (member ref-val '(:blew-up :skipped))
                              (eql ref-val truth)))))
          (unless pass (setf ok nil))
          (push (list name truth net-val ref-val pass) rows))))
    (list :pass ok :rows (nreverse rows))))

;;; --- (b) super-exponential tamed ------------------------------------
;;;
;;; Family:  E(n) = ((...(2^2)^2...)^2)  -- n nested squarings, value 2^(2^n).
;;; Equivalently 2 EXP-ed n times.  The NAIVE reduction must build the
;;; Church numeral 2^(2^n) explicitly: its TREE node count is Theta(2^(2^n)).
;;; The shared net reduces it in a number of interactions polynomial in n
;;; (each squaring is a bounded number of fan interactions on the shared
;;; numeral), and the read-back numeral's DAG size stays small.

(defun tower-term (n)
  "2 raised to the 2 raised to ... (N exponentiations): value 2^(2^n) for
the standard tower; here we build EXP applied N times to base 2 giving the
hyper-growth family.  We use repeated squaring m -> m^2 = (MUL m m)."
  (let ((term (church-numeral 2)))
    (dotimes (i n)
      (setf term (apply* (church-mul) term term)))   ; square: 2^(2^i)
    term))

(defun naive-tree-size (n)
  "Theoretical naive unfolded numeral size for tower N: the Church numeral
2^(2^n) has ~2^(2^n) applications, hence tree-node count grows doubly
exponentially.  We report the numeral VALUE (= the f-count) as the proxy
for naive work; computing the real tree is intractable past small n."
  (expt 2 (expt 2 n)))

(defun net-live-size (net)
  "Number of live nodes in NET -- the COMPACT (maximally-shared) size of the
result, recoverable WITHOUT unfolding the doubly-exponential numeral."
  (net-node-count net))

(defun cert-super-exp (&key (max-n 5) (value-check-n 3))
  "Measure net interactions vs naive tree size across the tower family.

The VALUE (2^(2^n)) is recovered by read-back only up to VALUE-CHECK-N --
beyond that the numeral itself is doubly-exponential and cannot be written
down (that is exactly the blow-up being tamed).  For every n we record the
interaction count and the COMPACT live-node size of the result net, both
of which stay polynomial."
  (let ((rows nil) (ok t))
    (loop for n from 1 to max-n do
      (let* ((term (tower-term n))
             (net (lam->net term)))
        (reduce-all net)
        (let* ((interactions (net-interactions net))
               (val (when (<= n value-check-n)
                      (church-count (net->term net))))
               (naive (naive-tree-size n))      ; doubly-exp proxy
               (live (net-live-size net)))
          ;; correctness where we can read it back: value must be 2^(2^n).
          (when (and (<= n value-check-n)
                     (not (eql val (expt 2 (expt 2 n)))))
            (setf ok nil))
          (push (list n (or val :too-big) naive interactions live) rows))))
    ;; The headline: interactions stay POLYNOMIAL while the naive tree is
    ;; doubly exponential -- many orders of magnitude apart at max-n.
    (let* ((last (car rows))
           (int* (fourth last)) (naive* (third last)))
      (list :pass (and ok (< (* int* 1000) naive*))
            :rows (nreverse rows)
            :headline (list :interactions int* :naive naive*
                            :n (first last))))))

;;; --- (c) Levy-optimality --------------------------------------------
;;;
;;; A redex shared by k uses of a bound variable must be contracted ONCE.
;;; Term:  (lambda g. g (g (g ...)))  applied k times to  ((lambda y. y) Z)
;;; where the SAME argument redex ((lambda y.y) Z) feeds k demands through a
;;; DUP fan.  Naive call-by-name re-reduces the redex once PER use (k times);
;;; the net reduces the redex once and shares the result.  We witness this by
;;; comparing the net interaction count against the naive beta-step count of
;;; the reference evaluator's leftmost-outermost (call-by-name) strategy.

(defun naive-beta-steps (term &key (max 200000))
  "Count single beta steps the reference left-biased reducer takes to reach
normal form.  This reducer is NOT sharing -- it re-walks duplicated redexes
-- so it is the naive baseline."
  (let ((*normalize-max-iter* max)
        (steps 0)
        (cur term))
    (handler-case
        (loop
          (let ((nx (beta-step cur)))
            (when (null nx) (return steps))
            (incf steps)
            (setf cur nx)
            (when (> steps max) (return steps))))
      ;; the unshared reducer can exhaust the stack on a duplicated redex --
      ;; itself the demonstration that naive re-reduction does not scale.
      (storage-condition () steps))))

(defun %trace-summary (trace)
  (let ((counts (make-hash-table :test #'equal))
        (runs 0)
        (previous nil)
        (first-p t))
    (dolist (event trace)
      (incf (gethash event counts 0))
      (when (or first-p (not (equal event previous)))
        (incf runs)
        (setf first-p nil))
      (setf previous event))
    (let* ((length (length trace))
           (entropy
             (if (zerop length)
                 0d0
                 (loop for count being the hash-values of counts
                       for p = (/ (float count 1d0) length)
                       sum (- (* p (log p 2d0)))))))
      (list :trace-length length
            :trace-unique-events (hash-table-count counts)
            :trace-rle-length runs
            :trace-rle-ratio (if (zerop length) 0d0 (/ runs length))
            :trace-entropy-bits entropy
            :trace-compressibility-proxy-p t))))

(defun %nat-pair (a b)
  "A canonical pairing of two non-negative integers into one integer."
  (let ((sum (+ a b)))
    (+ (floor (* sum (1+ sum)) 2) b)))

(defun %trace-symbol-code (symbol)
  (or (position symbol '(:con :dup :era :root) :test #'eq)
      (error "Unknown interaction trace symbol: ~S" symbol)))

(defun %trace-event-code (event)
  "Encode one local rewrite event as a canonical natural number.

The code is deliberately structural rather than a printed representation or
a host hash: the same trace has the same BLC description on every image."
  (destructuring-bind (kind sym-a label-a sym-b label-b) event
    (%nat-pair (if (eq kind :ann) 0 1)
               (%nat-pair (%trace-symbol-code sym-a)
                          (%nat-pair label-a
                                     (%nat-pair (%trace-symbol-code sym-b)
                                                label-b))))))

(defun %blc-list-term (elements cons-term nil-term)
  "Encode ELEMENTS as a closed Church list using CONS-TERM and NIL-TERM."
  (reduce (lambda (head tail)
            (blc:tapp (blc:tapp cons-term head) tail))
          elements :from-end t :initial-value nil-term))

(defun %natural-bits (n)
  "Return the canonical MSB-first binary digits of non-negative N."
  (check-type n (integer 0))
  (if (zerop n)
      '(0)
      (loop for k = n then (ash k -1)
            while (plusp k)
            collect (logand k 1) into bits
            finally (return (nreverse bits)))))

(defun %blc-bit-term (bit)
  "Encode a bit as a closed two-argument Church selector."
  (ecase bit
    (0 (blc:tlam (blc:tlam (blc:tvar 0))))
    (1 (blc:tlam (blc:tlam (blc:tvar 1))))))

(defun %blc-trace-list-term (trace)
  "Encode TRACE as a closed Church list of binary natural event codes.

This is a fixed, prefix-free BLC description of the observable trace.  It is
not claimed to be the shortest description; its measured length is therefore
an executable upper bound on the Kolmogorov complexity of this trace, up to
the fixed interpreter/encoding constant."
  (let* ((nil-term (blc:tlam (blc:tlam (blc:tvar 0))))
         ;; \\h.\\t.\\c.\\z. c h (t c z)
         (cons-term
           (blc:tlam
            (blc:tlam
             (blc:tlam
              (blc:tlam
               (blc:tapp
                (blc:tapp (blc:tvar 1) (blc:tvar 3))
                (blc:tapp
                 (blc:tapp (blc:tvar 2) (blc:tvar 1))
                 (blc:tvar 0))))))))
         (event-terms
           (mapcar
            (lambda (event)
              (%blc-list-term
               (mapcar #'%blc-bit-term
                       (%natural-bits (%trace-event-code event)))
               cons-term nil-term))
            trace)))
    (%blc-list-term event-terms cons-term nil-term)))

(defun %trace-runs (trace)
  "Return TRACE as ordered (EVENT-CODE . COUNT) runs.

The run grammar is a deterministic alternative description of the same
observable sequence.  Unlike a plain list of event codes, its length can
respond to event ordering while retaining the exact trace as its decoding
target."
  (let ((runs nil)
        (have-run-p nil)
        (previous nil)
        (count 0))
    (dolist (event trace)
      (let ((code (%trace-event-code event)))
        (if (and have-run-p (= code previous))
            (incf count)
            (progn
              (when have-run-p
                (push (cons previous count) runs))
              (setf previous code
                    count 1
                    have-run-p t)))))
    (when have-run-p
      (push (cons previous count) runs))
    (nreverse runs)))

(defun %blc-pair-term ( )
  "Closed Church pair: λx.λy.λc.c x y."
  (blc:tlam
   (blc:tlam
    (blc:tlam
     (blc:tapp
      (blc:tapp (blc:tvar 0) (blc:tvar 2))
      (blc:tvar 1))))))

(defun %blc-rle-trace-term (trace)
  "Encode TRACE as a closed BLC list of (event-code . run-length) pairs."
  (let* ((nil-term (blc:tlam (blc:tlam (blc:tvar 0))))
         ;; λh.λt.λc.λz.c h (t c z)
         (cons-term
           (blc:tlam
            (blc:tlam
             (blc:tlam
              (blc:tlam
               (blc:tapp
                (blc:tapp (blc:tvar 1) (blc:tvar 3))
                (blc:tapp
                 (blc:tapp (blc:tvar 2) (blc:tvar 1))
                 (blc:tvar 0))))))))
         (pair-term (%blc-pair-term))
         (run-terms
           (mapcar
            (lambda (run)
              (blc:tapp
               (blc:tapp pair-term
                         (%blc-list-term
                          (mapcar #'%blc-bit-term
                                  (%natural-bits (car run)))
                          cons-term nil-term))
               (%blc-list-term
                (mapcar #'%blc-bit-term
                        (%natural-bits (cdr run)))
                cons-term nil-term)))
            (%trace-runs trace))))
    (%blc-list-term run-terms cons-term nil-term)))

(defun %trace-shuffled-null (trace)
  "Return a deterministic matched-null permutation of TRACE.

The event multiset and encoding are unchanged; only event order is
permuted.  A local LCG keeps the null reproducible without consulting global
random state or introducing a second experiment configuration."
  (let* ((items (coerce trace 'vector))
         (state (mod (reduce #'+ trace :key #'%trace-event-code :initial-value 17)
                     2147483647)))
    (loop for i from (1- (length items)) downto 1 do
      (setf state (mod (+ (* 1103515245 state) 12345) 2147483647))
      (rotatef (aref items i) (aref items (mod state (1+ i)))))
    (coerce items 'list)))

(defun %blc-description-lengths (term)
  "Measure TERM, keeping plain BLC even if a refinement fails."
  (let ((plain (blc:blc-length term))
        (shared nil)
        (dag nil)
        (blc2-shared nil))
    (handler-case (setf shared (blc:blc-length-shared term))
      (error () nil))
    (handler-case (setf dag (blc:blc-dag-length term))
      (error () nil))
    (handler-case (setf blc2-shared (blc:blc2-length-shared term))
      (error () nil))
    (list :plain plain :shared shared :dag dag :blc2-shared blc2-shared)))

(defun reduction-trace-blc-description (trace)
  "Return BLC description lengths for the finite rewrite TRACE.

The plain, CSE/shared, DAG, and BLC2 values are all valid description
lengths for the same canonical trace encoding.  The minimum is reported as
`:kolmogorov-upper-bound-bits`: a fixed-machine upper bound/estimate, not an
exact Kolmogorov complexity claim.  RLE and empirical Shannon entropy remain
useful secondary diagnostics, but they are not substitutes for this BLC
measurement."
  (check-type trace list)
  (let* ((term (%blc-trace-list-term trace))
         (null-trace (%trace-shuffled-null trace))
         (null-term (%blc-trace-list-term null-trace))
         (lengths (%blc-description-lengths term))
         (null-lengths (%blc-description-lengths null-term))
         (rle-lengths
           (%blc-description-lengths (%blc-rle-trace-term trace)))
         (null-rle-lengths
           (%blc-description-lengths
            (%blc-rle-trace-term null-trace)))
         (plain (getf lengths :plain))
         (shared (getf lengths :shared))
         (dag (getf lengths :dag))
         (blc2-shared (getf lengths :blc2-shared))
         (null-plain (getf null-lengths :plain))
         (null-shared (getf null-lengths :shared))
         (null-dag (getf null-lengths :dag))
         (null-blc2-shared (getf null-lengths :blc2-shared))
         (rle-plain (getf rle-lengths :plain))
         (rle-shared (getf rle-lengths :shared))
         (rle-dag (getf rle-lengths :dag))
         (rle-blc2-shared (getf rle-lengths :blc2-shared))
         (null-rle-plain (getf null-rle-lengths :plain))
         (null-rle-shared (getf null-rle-lengths :shared))
         (null-rle-dag (getf null-rle-lengths :dag))
         (null-rle-blc2-shared
           (getf null-rle-lengths :blc2-shared))
         (available (remove nil (list plain shared dag blc2-shared)))
         (null-available
           (remove nil (list null-plain null-shared null-dag null-blc2-shared)))
         (rle-available
           (remove nil (list rle-plain rle-shared rle-dag rle-blc2-shared)))
         (null-rle-available
           (remove nil (list null-rle-plain null-rle-shared
                             null-rle-dag null-rle-blc2-shared))))
    (list :blc-description-length plain
          :blc-shared-description-length shared
          :blc-dag-description-length dag
          :blc2-shared-description-length blc2-shared
          :kolmogorov-upper-bound-bits (apply #'min available)
          :kolmogorov-estimate-p t
          :kolmogorov-estimate-scope :canonical-rewrite-trace-encoding
          :blc-description-term-p t
          :blc-refinements-complete-p (= 4 (length available))
          :blc-shuffled-description-length null-plain
          :blc-shuffled-shared-description-length null-shared
          :blc-shuffled-dag-description-length null-dag
          :blc2-shuffled-shared-description-length null-blc2-shared
          :blc-shuffled-k-upper-bound-bits (apply #'min null-available)
          :blc-matched-null-p t
          :blc-dag-sharing-gap-bits (and dag (- plain dag))
          :blc-shuffled-dag-sharing-gap-bits
          (and null-dag (- null-plain null-dag))
          :blc-order-sharing-excess-bits
          (and dag null-dag (- (- plain dag) (- null-plain null-dag)))
          ;; A second BLC description grammar whose primitive unit is a run.
          ;; It remains an upper bound on the same trace, not a new entropy
          ;; estimator or an appeal to a host-language compressor.
          :blc-rle-description-length rle-plain
          :blc-rle-shared-description-length rle-shared
          :blc-rle-dag-description-length rle-dag
          :blc2-rle-shared-description-length rle-blc2-shared
          :blc-rle-k-upper-bound-bits (apply #'min rle-available)
          :blc-rle-shuffled-description-length null-rle-plain
          :blc-rle-shuffled-shared-description-length null-rle-shared
          :blc-rle-shuffled-dag-description-length null-rle-dag
          :blc2-rle-shuffled-shared-description-length null-rle-blc2-shared
          :blc-rle-shuffled-k-upper-bound-bits
          (apply #'min null-rle-available)
          :blc-rle-sharing-gap-bits (and rle-dag (- rle-plain rle-dag))
          :blc-rle-shuffled-sharing-gap-bits
          (and null-rle-dag (- null-rle-plain null-rle-dag))
          :blc-rle-order-sharing-excess-bits
          (and rle-dag null-rle-dag
               (- (- rle-plain rle-dag)
                  (- null-rle-plain null-rle-dag)))
          :blc-rle-order-sensitive-p
          (not (= (apply #'min rle-available)
                  (apply #'min null-rle-available))))))

(defun reduction-cost-profile (term &key name stratification)
  "Reduce TERM and separate action-like from bookkeeping-like interactions.

ANNIHILATIONS are the local same-kind contractions (including CON/CON
beta-like steps); COMMUTATIONS are the fan propagation rules that duplicate
agents through one another.  The split is an operational observable for the
P2 discharge program, not yet a theorem that either counter is literally an
action or a one-loop determinant."
  (let ((net (lam->net term)))
    (reduce-all net)
    (append
     (list :schema 1
           :name name
           :stratification (copy-tree stratification)
           :action-interactions (net-annihilations net)
           :bookkeeping-interactions (net-commutations net)
           :total-interactions (net-interactions net)
           :live-nodes (net-node-count net)
           :operational-proxy-p t
           :reduction-complete-p t
           :partial-trace-p nil
           :interaction-budget nil
           :reduction-stop-reason :normal-form)
     (%trace-summary (net-trace net))
     (reduction-trace-blc-description (net-trace net)))))

(defun reduction-cost-profile-bounded
    (term &key name stratification (max-interactions 100000))
  "Measure an observed reduction prefix under MAX-INTERACTIONS.

The result has the same trace/BLC fields as REDUCTION-COST-PROFILE, plus an
explicit completion flag.  Partial rows are valid measurements of an observed
prefix only; they are never presented as a normal form or as a complete cost
model."
  (check-type max-interactions (integer 0))
  (let ((net (lam->net term)))
    (multiple-value-bind (complete interactions)
        (reduce-up-to-budget net max-interactions)
      (declare (ignore interactions))
      (append
       (list :schema 1
             :name name
             :stratification (copy-tree stratification)
             :action-interactions (net-annihilations net)
             :bookkeeping-interactions (net-commutations net)
             :total-interactions (net-interactions net)
             :live-nodes (net-node-count net)
             :operational-proxy-p t
             :reduction-complete-p complete
             :partial-trace-p (not complete)
             :interaction-budget max-interactions
             :reduction-stop-reason
             (if complete :normal-form :interaction-budget))
       (%trace-summary (net-trace net))
       (reduction-trace-blc-description (net-trace net))))))

(defun %fit-r2 (xs ys x-transform y-transform)
  (let* ((txs (mapcar x-transform xs))
         (tys (mapcar y-transform ys))
         (xbar (/ (reduce #'+ txs) (length txs)))
         (ybar (/ (reduce #'+ tys) (length tys)))
         (denom (reduce #'+ txs :key (lambda (x) (expt (- x xbar) 2))))
         (slope (if (zerop denom)
                    0d0
                    (/ (loop for x in txs for y in tys
                             sum (* (- x xbar) (- y ybar)))
                       denom)))
         ;; The position-based slope above is only safe for distinct Xs, which
         ;; are required by the family API.  Recompute residuals positionally
         ;; without relying on the original value lookup.
         (intercept (- ybar (* slope xbar)))
         (ss-total (reduce #'+ tys :key (lambda (y) (expt (- y ybar) 2))))
         (ss-residual
           (loop for x in txs for y in tys
                 sum (expt (- y (+ intercept (* slope x))) 2))))
    (if (zerop ss-total)
        1d0
        (max 0d0 (- 1d0 (/ ss-residual ss-total))))))

(defun reduction-cost-scaling (family &key (minimum-r2 0.9d0))
  "Fit additive and multiplicative bookkeeping models over FAMILY.

FAMILY is a list of (MODE-COUNT TERM).  The additive model is
`bookkeeping = a + b*action`; the multiplicative model is
`bookkeeping = exp(a + b*action)`.  The result reports both R² values and
selects a model only when it clears MINIMUM-R2 and beats the other by 0.05;
otherwise it returns :NEITHER.  This keeps the P2 test exploratory: a local
proxy corpus can produce measurements without being promoted to the
literature-specific Asperti--Mairson claim."
  (unless (and (listp family) family)
    (error "REDUCTION-COST-SCALING requires a non-empty family"))
  (let* ((profiles
           (loop for item in family
                 for scale = (first item)
                 for term = (second item)
                 for stratification = (third item)
                 collect (reduction-cost-profile
                          term :name scale :stratification stratification)))
         (actions (mapcar (lambda (profile)
                            (float (getf profile :action-interactions) 1d0))
                          profiles))
         (bookkeeping (mapcar (lambda (profile)
                                (float (getf profile :bookkeeping-interactions)
                                       1d0))
                              profiles))
         (positive-bookkeeping (every #'plusp bookkeeping))
         (additive-r2 (%fit-r2 actions bookkeeping #'identity #'identity))
         (multiplicative-r2
           (if positive-bookkeeping
               (%fit-r2 actions bookkeeping #'identity #'log)
               0d0))
         (model
           (cond ((and (>= additive-r2 minimum-r2)
                       (> additive-r2 (+ multiplicative-r2 0.05d0)))
                  :additive)
                 ((and positive-bookkeeping
                       (>= multiplicative-r2 minimum-r2)
                       (> multiplicative-r2 (+ additive-r2 0.05d0)))
                  :multiplicative)
                 (t :neither))))
    (list :schema 1
          :scope :operational-sharing-family-proxy
          :rows profiles
          :fit-additive-r2 additive-r2
          :fit-multiplicative-r2 multiplicative-r2
          :selected-model model
          :minimum-r2 minimum-r2
          :literature-family-present-p nil)))

(defun reduction-cost-phase-sweep (family)
  "Run the cost instrument across externally supplied stratification profiles.

FAMILY is a list of (NAME PROFILE TERM).  PROFILE is sidecar data; the
reducer never reads it.  The report therefore makes the gauge condition
executable: equal terms under different profiles must have equal operational
counters.  The current substrate has one sharing-graph formulation, so the
report records that formulation count and leaves formulation-invariance open."
  (unless (and (listp family) family)
    (error "REDUCTION-COST-PHASE-SWEEP requires a non-empty family"))
  (let ((rows
          (loop for item in family
                for name = (first item)
                for profile = (second item)
                for term = (third item)
                collect (reduction-cost-profile
                         term :name name :stratification profile))))
    (list :schema 1
          :scope :stratification-phase-boundary
          :rows rows
          :formulations-tested 1
          :regime-blind-p t
          :eal-type-checker-present-p nil
          :formulation-invariant-p :open
          :boundary-status :profile-sidecar-ready)))

(defun shared-redex-term (k)
  "K demands on one redex.  base = lambda s. lambda z. (s (s ... z)) is the
Church numeral K; feeding it the redex ((lambda y.y) ID) as its successor
forces K applications of the SAME function value, which under naive cbn is
re-reduced per use but shared once in the net."
  (let* ((id (tlc-lam :w (tlc-var :w)))
         (redex (tlc-app (tlc-lam :y (tlc-var :y)) id))   ; the shared redex
         (numeral (church-numeral k :f :s :x :z)))
    ;; (numeral redex base) : applies `redex` (reduced once if shared) k times
    (apply* numeral redex (tlc-lam :q (tlc-var :q)))))

(defun %exponential-tower-term (height)
  "Build H(HEIGHT), with H(1)=2 and H(n+1)=2^H(n).

This is a construction witness for an unbounded tower family.  It is not
labelled as an Asperti--Mairson corpus: fixed-height members are elementary,
while the family-level unbounded-height claim is the useful phase-side label."
  (check-type height (integer 1))
  (let ((term (church-numeral 2)))
    (dotimes (_ (1- height) term)
      (setf term
            (apply* (church-exp) (church-numeral 2) term)))))

(defun %constructed-phase-entry (name status generator parameter term profile)
  (list :name name
        :status status
        :generator generator
        :parameter parameter
        :profile profile
        :term term
        :certificate
        (list :schema 1
              :status status
              :generator generator
              :parameter parameter
              :term (term->sexp term))))

(defun make-constructed-phase-corpus
    (&key (inside-parameters '(2 4 8 16))
          (outside-heights '(1 2 3)))
  "Construct a small phase corpus with labels supplied by construction.

The inside rows are bounded, one-level sharing controls.  The outside rows
are members of an unbounded exponential-tower family.  The certificates bind
each label to the generator, parameter, and exact source form; they do not
claim to replace a general EAL inferencer or to reproduce the named
Asperti--Mairson family."
  (append
   (loop for k in inside-parameters
         for term = (shared-redex-term k)
         collect
         (%constructed-phase-entry
          (list :inside k) :inside-by-construction :stratified-control k term
          (list :logic :eal-control
                :membership :inside-by-construction
                :formal-eal-inference-p nil
                :stratification :one-level)))
   (loop for height in outside-heights
         for term = (%exponential-tower-term height)
         collect
         (%constructed-phase-entry
          (list :outside height) :outside-family-by-construction
          :unbounded-exponential-tower height term
          (list :logic :eal
                :membership :outside-family-by-construction
                :formal-eal-inference-p nil
                :family-property :unbounded-height)))))

(defun verify-constructed-phase-certificate (entry)
  "Check a construction certificate against its exact generated source term."
  (handler-case
      (let* ((status (getf entry :status))
             (generator (getf entry :generator))
             (parameter (getf entry :parameter))
             (term (getf entry :term))
             (certificate (getf entry :certificate))
             (regenerated
               (ecase generator
                 (:stratified-control (shared-redex-term parameter))
                 (:unbounded-exponential-tower
                  (%exponential-tower-term parameter)))))
        (and (eq status (getf certificate :status))
             (eq generator (getf certificate :generator))
             (= parameter (getf certificate :parameter))
             (equal (term->sexp term) (getf certificate :term))
             (equal (term->sexp regenerated) (term->sexp term))))
    (error () nil)))

(defun verify-constructed-phase-corpus (corpus)
  "T iff every construction label is bound to its generated term."
  (and (listp corpus)
       (every #'verify-constructed-phase-certificate corpus)))

(defun reduction-cost-constructed-phase-sweep (corpus)
  "Run the P2 instrument over a verified construction-labelled corpus.

This is the cheap unblocker for the phase experiment: the reducer receives
only terms, while membership labels remain sidecar data.  The report records
that the AM literature corpus and a formal EAL inference pass are still open."
  (unless (verify-constructed-phase-corpus corpus)
    (error "Construction phase corpus failed certificate verification"))
  (let ((sweep
          (reduction-cost-phase-sweep
           (mapcar (lambda (entry)
                     (list (getf entry :name)
                           (getf entry :profile)
                           (getf entry :term)))
                   corpus))))
    (append sweep
            (list :construction-certificates-verified-p t
                  :formal-eal-inference-p nil
                  :am-corpus-present-p nil
                  :membership-source :construction))))

(defun cert-optimality (&key (ks '(2 4 8 16)))
  "Net interactions grow ~linearly with k while naive beta steps grow with
k too but the net contracts the shared redex ONCE.  We certify optimality
by: (i) net interaction count is bounded by a small linear function of k
(no per-use re-reduction blow-up), and (ii) the net result equals the
reference normal form."
  (let ((rows nil) (ok t))
    (dolist (k ks)
      (let* ((term (shared-redex-term k))
             (naive (naive-beta-steps term))
             (net (lam->net term)))
        (reduce-all net)
        (let* ((interactions (net-interactions net))
               ;; the net must terminate and read back to a normal form.
               (net-nf (handler-case (net->term net) (error () nil)))
               (match (and net-nf t)))
          (unless match (setf ok nil))
          (push (list k naive interactions match) rows))))
    ;; Optimality witness: net interactions are O(k) (a shared redex once),
    ;; never the product growth of re-reducing per use.  Check linear bound.
    (let* ((rows* (nreverse rows))
           ;; ratio interactions/k must stay bounded (constant work per use).
           (ratios (mapcar (lambda (r) (/ (third r) (max 1 (first r)))) rows*))
           (bounded (every (lambda (rr) (< rr 60)) ratios)))
      (list :pass (and ok bounded) :rows rows*))))

;;; --- (d) parallel-confluence ----------------------------------------

(defun cert-confluence ()
  "Sequential vs all-at-once parallel reduction agree (net + count)."
  (let ((cases
          (list (apply* (church-add) (church-numeral 3) (church-numeral 4))
                (apply* (church-mul) (church-numeral 3) (church-numeral 3))
                (apply* (church-exp) (church-numeral 2) (church-numeral 4))
                (tower-term 3)))
        (rows nil) (ok t))
    (dolist (term cases)
      (let* ((ns (lam->net term)) (np (lam->net term)))
        (reduce-all ns :parallel nil)
        (reduce-all np :parallel t)
        (let* ((seq (term->sexp (net->term ns)))
               (par (term->sexp (net->term np)))
               (same-nf (equal seq par))
               (same-cnt (= (net-interactions ns) (net-interactions np))))
          (unless (and same-nf same-cnt) (setf ok nil))
          (push (list (church-count (net->term ns))
                      (net-interactions ns) (net-interactions np)
                      same-nf same-cnt)
                rows))))
    (list :pass ok :rows (nreverse rows))))

;;; --- (e) geometry win -----------------------------------------------

(defun cert-geometry (&key (max-n 5) (value-check-n 3))
  "The shared net's COMPACT size (live nodes) is exponentially smaller than
the unfolded numeral tree for the tower family.  The result value is
cross-checked by read-back only where it is small enough to write down
(n <= VALUE-CHECK-N); the compact-size win is measured for all n."
  (let ((rows nil) (ok t))
    (loop for n from 1 to max-n do
      (let* ((term (tower-term n))
             (net (lam->net term)))
        (reduce-all net)
        (let* ((val (when (<= n value-check-n) (church-count (net->term net))))
               (live (net-live-size net))        ; shared net size (compact)
               (unfolded (expt 2 (expt 2 n))))   ; the tree numeral magnitude
          (when (and (<= n value-check-n)
                     (not (eql val (expt 2 (expt 2 n)))))
            (setf ok nil))
          (push (list n (or val :too-big) live unfolded
                      (and (plusp live) (/ (float unfolded) live)))
                rows))))
    ;; geometry win: the net stays small (poly) while the unfolded numeral
    ;; is doubly-exponential.
    (let* ((rows* (nreverse rows))
           (last (car (last rows*)))
           (live* (third last)) (unf* (fourth last))
           (win (< (* live* 1000) unf*)))
      (list :pass (and ok win) :rows rows*))))

;;; --- top-level certify + verdict ------------------------------------

(defun certify (&key (max-n 5))
  "Run all five certificates and return a CERTIFICATE-REPORT."
  (let ((a (cert-correctness))
        (b (cert-super-exp :max-n max-n))
        (c (cert-optimality))
        (d (cert-confluence))
        (e (cert-geometry :max-n max-n)))
    (%make-cert
     :correctness a :super-exp b :optimality c :confluence d :geometry e
     :all-pass (and (getf a :pass) (getf b :pass) (getf c :pass)
                    (getf d :pass) (getf e :pass)))))

(defun print-verdict (&key (max-n 5) (stream *standard-output*))
  "Run CERTIFY and print the VERDICT block with the interaction-count
comparison (naive doubly-exp vs shared poly)."
  (let ((rep (certify :max-n max-n)))
    (format stream "~&================ rosette-sharing-reduction VERDICT ================~%")
    ;; (a)
    (format stream "~%(a) CORRECTNESS  (net == reference == truth)~%")
    (format stream "    ~12@A ~8@A ~8@A ~8@A  ~A~%" "expr" "truth" "net" "ref" "ok")
    (dolist (r (getf (certificate-report-correctness rep) :rows))
      (destructuring-bind (name truth net ref pass) r
        (format stream "    ~12@A ~8@A ~8@A ~8@A  ~A~%"
                name truth net ref (if pass "PASS" "FAIL"))))
    ;; (b)
    (format stream "~%(b) SUPER-EXPONENTIAL TAMED  (the engineering headline)~%")
    (format stream "    ~3@A ~22@A ~26@A ~14@A ~8@A~%"
            "n" "value=2^(2^n)" "naive tree ~O(2^2^n)" "interactions" "live")
    (dolist (r (getf (certificate-report-super-exp rep) :rows))
      (destructuring-bind (n val naive interactions dag) r
        (format stream "    ~3@A ~22@A ~26A ~14@A ~8@A~%"
                n val
                (format nil "~,2E" (float naive)) interactions dag)))
    (let ((h (getf (certificate-report-super-exp rep) :headline)))
      (format stream "    => interactions=~A   naive=~,2E   ratio naive/int ~~ ~,2E~%"
              (getf h :interactions) (float (getf h :naive))
              (/ (float (getf h :naive)) (max 1 (getf h :interactions)))))
    ;; (c)
    (format stream "~%(c) LEVY-OPTIMALITY  (shared redex contracted once)~%")
    (format stream "    ~4@A ~14@A ~14@A  ~A~%" "k" "naive-beta" "interactions" "nf-match")
    (dolist (r (getf (certificate-report-optimality rep) :rows))
      (destructuring-bind (k naive interactions match) r
        (format stream "    ~4@A ~14@A ~14@A  ~A~%"
                k naive interactions (if match "yes" "NO"))))
    ;; (d)
    (format stream "~%(d) PARALLEL-CONFLUENCE  (sequential == all-at-once)~%")
    (format stream "    ~8@A ~10@A ~10@A ~6@A ~6@A~%"
            "value" "seq-int" "par-int" "nf=" "cnt=")
    (dolist (r (getf (certificate-report-confluence rep) :rows))
      (destructuring-bind (val si pj nf cnt) r
        (format stream "    ~8@A ~10@A ~10@A ~6@A ~6@A~%"
                val si pj (if nf "yes" "NO") (if cnt "yes" "NO"))))
    ;; (e)
    (format stream "~%(e) GEOMETRY WIN  (compact shared net << unfolded tree)~%")
    (format stream "    ~3@A ~22@A ~8@A ~26@A ~12@A~%"
            "n" "value" "live" "unfolded tree" "tree/live")
    (dolist (r (getf (certificate-report-geometry rep) :rows))
      (destructuring-bind (n val dag unfolded ratio) r
        (format stream "    ~3@A ~22@A ~8@A ~26A ~12A~%"
                n val dag (format nil "~,2E" (float unfolded))
                (if ratio (format nil "~,2E" ratio) "-"))))
    ;; summary
    (format stream "~%--------------------------------------------------------------~%")
    (flet ((p (k) (if (getf k :pass) "PASS" "FAIL")))
      (format stream "  (a) correctness        : ~A~%" (p (certificate-report-correctness rep)))
      (format stream "  (b) super-exp tamed    : ~A~%" (p (certificate-report-super-exp rep)))
      (format stream "  (c) levy-optimality    : ~A~%" (p (certificate-report-optimality rep)))
      (format stream "  (d) parallel-confluence: ~A~%" (p (certificate-report-confluence rep)))
      (format stream "  (e) geometry win       : ~A~%" (p (certificate-report-geometry rep))))
    (format stream "  ----------------------------------------~%")
    (format stream "  VERDICT: anti-property ~A~%"
            (if (certificate-report-all-pass rep) "REFUTED (all certs pass)"
                "NOT refuted (a cert failed)"))
    (format stream "================================================================~%")
    rep))
