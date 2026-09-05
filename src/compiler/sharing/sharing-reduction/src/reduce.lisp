;;;; reduce.lisp --- reduction driver (sequential, parallel, normalize).
;;;;
;;;; An active pair is a wire joining two LIVE principal ports.  Reduction
;;;; fires active pairs until none remain.  Because every rule is local and
;;;; consumes its pair, the choice of pair / the firing order is irrelevant
;;;; to the final net (strong confluence) -- which is why a whole DISJOINT
;;;; BATCH can be fired in one parallel round.

(in-package #:rosette-sharing-reduction)

(defun principal-partner (node)
  "If NODE's principal port faces another node's principal port, return that
node; else NIL."
  (let ((q (enter (p0 node))))
    (when (and q (= (port-slot q) +slot-principal+))
      (let ((other (port-node q)))
        (when (node-alive other) other)))))

(defun active-pairs (net)
  "Return a list of (A . B) active pairs, each unordered pair once."
  (let ((seen (make-hash-table :test 'eq))
        (out nil))
    (loop for node across (net-nodes net)
          when (and node (node-alive node) (not (gethash node seen)))
            do (let ((other (principal-partner node)))
                 (when (and other (not (gethash other seen)))
                   (setf (gethash node seen) t
                         (gethash other seen) t)
                   (push (cons node other) out))))
    (nreverse out)))

(defun reduce-step (net)
  "Fire ONE active pair (the first found).  Return T if one fired."
  (let ((pairs (active-pairs net)))
    (when pairs
      (apply-rule net (caar pairs) (cdar pairs))
      t)))

(defun reduce-selected-step (net selector)
  "Fire one active pair selected by SELECTOR and return T if one fired.

SELECTOR is called with NET and the current list returned by ACTIVE-PAIRS.  It
must return a zero-based integer into that list.  The reducer validates the
choice before applying the exact local rule, so a learned scheduler can only
choose among live redexes and cannot mutate the authority surface."
  (check-type net net)
  (check-type selector function)
  (let ((pairs (active-pairs net)))
    (when pairs
      (let ((index (funcall selector net pairs)))
        (unless (and (integerp index) (<= 0 index) (< index (length pairs)))
          (error "reduce-selected-step: selector returned invalid active-pair index ~S"
                 index))
        (let ((pair (nth index pairs)))
          (apply-rule net (car pair) (cdr pair)))
        t))))

(defun reduce-up-to-budget (net max-interactions)
  "Fire at most MAX-INTERACTIONS sequential pairs.

Returns two values: whether NET reached normal form, and the interaction
count.  A false first value is an observed prefix, not a failed reduction;
the caller must not treat its readback as a normal form."
  (check-type net net)
  (check-type max-interactions (integer 0))
  (loop while (and (< (net-interactions net) max-interactions)
                   (reduce-step net)))
  (values (null (active-pairs net))
          (net-interactions net)))

(defun reduce-parallel-round (net)
  "Fire a maximal batch of MUTUALLY DISJOINT active pairs in one round.
Returns the number fired.  Snapshots the active pairs, then fires each
pair whose two nodes are still alive and untouched this round -- a node
freed/created by an earlier pair in the batch is skipped (it will be
caught next round).  Because the snapshot pairs are vertex-disjoint by
construction, every pair in the batch is independent."
  (let ((pairs (active-pairs net))
        (touched (make-hash-table :test 'eq))
        (fired 0))
    (dolist (pr pairs)
      (let ((a (car pr)) (b (cdr pr)))
        (when (and (node-alive a) (node-alive b)
                   (not (gethash a touched)) (not (gethash b touched)))
          (setf (gethash a touched) t (gethash b touched) t)
          (apply-rule net a b)
          (incf fired))))
    fired))

(defun reduce-all (net &key (max-rounds 1000000) (parallel nil))
  "Reduce NET to normal form.  When PARALLEL, fire disjoint batches per
round; otherwise fire one pair at a time.  Returns the round count."
  (let ((rounds 0))
    (loop
      (when (>= rounds max-rounds)
        (error "reduce-all: exceeded ~D rounds (non-terminating net?)"
               max-rounds))
      (let ((progressed
              (if parallel
                  (plusp (reduce-parallel-round net))
                  (reduce-step net))))
        (unless progressed (return))
        (incf rounds)))
    rounds))

(defun normalize-net (net &key (parallel nil))
  "Reduce NET fully; return the interaction count."
  (reduce-all net :parallel parallel)
  (net-interactions net))
