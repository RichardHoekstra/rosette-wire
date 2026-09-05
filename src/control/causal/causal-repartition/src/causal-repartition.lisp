;;;; causal-repartition.lisp --- the physical layer that flows under the logical tree.
;;;;
;;;; The gravity-well tree is the LOGICAL partition: stable, meaningful, and the
;;;; source of the deterministic-replay truth.  On top of it lives a PHYSICAL
;;;; partition that assigns compute to pieces of the logical tree and rebalances
;;;; purely on load -- invisible to the player, and (because the logical outcome
;;;; is partition-invariant) a gauge on the world.  The two must not be
;;;; one-to-one: a single hot logical arbiter -- one contested station, a
;;;; thousand ships -- must be splittable across machines.
;;;;
;;;; This library is the rebalancer's decision core.  Three ideas:
;;;;
;;;;  1. COST MODEL.  Load is measured as CAUSAL-EVENT-RATE, not occupancy: the
;;;;     edge weight between two objects/clusters is how many adjudications per
;;;;     unit time couple them (the size of the Delta_der residue they force).
;;;;     Running a set on one machine pays its whole internal interaction as
;;;;     serialization; splitting it runs the parts in parallel but pays every
;;;;     cut edge as a network round-trip, LATENCY-RATIO times dearer than a
;;;;     local adjudication.  Net gain of a cut S|T is
;;;;         min(internal(S), internal(T))  -  (lambda - 1) * cut(S,T)
;;;;     -- the parallelism won (the smaller side's internal load) minus the
;;;;     excess price of the edges the cut severs.
;;;;
;;;;  2. PHASE TRANSITION.  Split iff net gain > 0.  On a barbell (two dense
;;;;     clusters, a thin bridge) the min-cut is the bridge and the gain is
;;;;     large; on a dense clique there is no thin cut and the gain is negative.
;;;;     The boundary is sharp and LOCATED: bridge weight = internal / (lambda-1).
;;;;     Larger lambda (dearer network) moves the boundary down -- you must cut
;;;;     thinner before it pays -- so lambda IS the sharpness of the transition.
;;;;
;;;;  3. HYSTERESIS.  The transition is sharp; the response must be deliberately
;;;;     blunt, or a cluster balanced on the boundary flaps split/merge many
;;;;     times a second.  A two-threshold controller (split above THETA-HI, merge
;;;;     below THETA-LO, hold between) makes each repartition a discrete,
;;;;     HLC-stamped, replayable SWITCH -- never a membrane.

(in-package #:rosette-causal-repartition)

;;; ---------------------------------------------------------------------------
;;; The causal-interaction graph.
;;; ---------------------------------------------------------------------------

(defstruct (interaction-graph (:constructor %make-graph) (:copier nil))
  "An undirected weighted graph over object/cluster ids.  Edge weight is the
causal-event-rate coupling the two endpoints (adjudications per unit time)."
  (nodes '() :type list)
  (edges (make-hash-table :test 'equal)))  ; canonical (a . b), a string< b -> weight

(defun %edge-key (a b)
  (if (string< a b) (cons a b) (cons b a)))

(defun make-interaction-graph (nodes edges)
  "Build a graph from NODES (list of ids) and EDGES (list of (A B WEIGHT)).
Duplicate edges sum; endpoints must be nodes; weights must be non-negative."
  (let ((g (%make-graph :nodes (remove-duplicates (copy-list nodes)
                                                  :test #'string=))))
    (dolist (e edges g)
      (destructuring-bind (a b w) e
        (unless (and (member a (interaction-graph-nodes g) :test #'string=)
                     (member b (interaction-graph-nodes g) :test #'string=))
          (error "Edge ~S references a non-node" e))
        (when (minusp w) (error "Negative edge weight in ~S" e))
        (unless (string= a b)
          (let ((k (%edge-key a b)))
            (incf (gethash k (interaction-graph-edges g) 0) w)))))))

(defun edge-weight (g a b)
  "Coupling weight between A and B (0 if none)."
  (gethash (%edge-key a b) (interaction-graph-edges g) 0))

(defun %in-set-p (node set) (member node set :test #'string=))

(defun %xor (a b) (if a (not b) b))

(defun internal-weight (g subset)
  "Total edge weight with BOTH endpoints in SUBSET -- the serialization load if
SUBSET runs on one machine."
  (let ((sum 0))
    (maphash (lambda (k w)
               (when (and (%in-set-p (car k) subset) (%in-set-p (cdr k) subset))
                 (incf sum w)))
             (interaction-graph-edges g))
    sum))

(defun cut-weight (g subset)
  "Total edge weight with EXACTLY ONE endpoint in SUBSET -- the cross-machine
traffic a cut at SUBSET's boundary would buy."
  (let ((sum 0))
    (maphash (lambda (k w)
               (when (%xor (%in-set-p (car k) subset)
                           (%in-set-p (cdr k) subset))
                 (incf sum w)))
             (interaction-graph-edges g))
    sum))

;;; ---------------------------------------------------------------------------
;;; Cost model and the phase transition.
;;; ---------------------------------------------------------------------------

(defun net-gain (g subset lambda)
  "Net benefit of the cut SUBSET | (nodes - SUBSET) at latency-ratio LAMBDA (>= 1):
    min(internal(S), internal(T)) - (lambda - 1) * cut(S,T).
A trivial cut (one side empty) has gain 0."
  (let* ((other (set-difference (interaction-graph-nodes g) subset
                                :test #'string=)))
    (if (or (null subset) (null other))
        0
        (- (min (internal-weight g subset) (internal-weight g other))
           (* (- lambda 1) (cut-weight g subset))))))

(defun %proper-subsets-with-first (nodes)
  "All subsets that contain the first node and are proper (exclude the full set),
enumerated once per unordered cut (S and its complement are not both listed)."
  (if (null nodes)
      '()
      (let ((first (first nodes))
            (rest (rest nodes))
            (out '()))
        (dotimes (mask (expt 2 (length rest)) out)
          (let ((s (list first)))
            (loop for i below (length rest)
                  when (logbitp i mask)
                    do (push (nth i rest) s))
            (when (< (length s) (length nodes))     ; proper
              (push s out)))))))

(defun best-cut (g lambda)
  "Return (values BEST-SUBSET BEST-GAIN): the cut maximising NET-GAIN.  Exact by
enumeration -- intended for the small hot sub-domain a rebalancer actually
considers (a real fleet-scale cut needs a streaming min-cut heuristic, the named
frontier).  For >~16 nodes this is deliberately refused."
  (let ((n (length (interaction-graph-nodes g))))
    (when (> n 16)
      (error "best-cut enumerates 2^n; ~D nodes is past the witness cap of 16" n))
    (let ((best nil) (best-gain 0))       ; gain 0 = do not split
      (dolist (s (%proper-subsets-with-first (interaction-graph-nodes g)))
        (let ((gain (net-gain g s lambda)))
          (when (> gain best-gain)
            (setf best-gain gain best s))))
      (values best best-gain))))

(defun should-split-p (g lambda)
  "T iff some cut has strictly positive net gain at LATENCY-RATIO LAMBDA."
  (multiple-value-bind (s gain) (best-cut g lambda)
    (declare (ignore s))
    (> gain 0)))

(defun critical-bridge (internal lambda)
  "The bridge weight at which a barbell (two clusters each of INTERNAL load,
joined by one bridge) sits exactly on the split/no-split boundary:
INTERNAL / (LAMBDA - 1).  Thinner bridge -> split; thicker -> keep merged."
  (when (<= lambda 1) (error "latency-ratio must exceed 1, got ~S" lambda))
  (/ internal (- lambda 1)))

;;; ---------------------------------------------------------------------------
;;; The hysteresis controller: sharp transition -> flap-free switch.
;;; ---------------------------------------------------------------------------

(defstruct (repartition-event (:constructor %make-repartition-event)
                              (:copier nil))
  "A discrete, HLC-stamped physical repartition -- a switch, not a membrane."
  (kind :split :type (member :split :merge))
  (hlc (cf:make-hlc) :type cf:hlc)
  (gain 0))

(defstruct (repartition-controller (:constructor %make-controller) (:copier nil))
  (state :merged :type (member :merged :split))
  (theta-hi 0)
  (theta-lo 0))

(defun make-repartition-controller (&key (theta-hi 1) (theta-lo 0)
                                         (initial-state :merged))
  "A hysteresis controller: split when gain exceeds THETA-HI, merge when it falls
below THETA-LO, hold in the dead band between.  THETA-HI must exceed THETA-LO --
the dead band is what prevents flapping."
  (unless (> theta-hi theta-lo)
    (error "theta-hi (~S) must exceed theta-lo (~S)" theta-hi theta-lo))
  (%make-controller :state initial-state :theta-hi theta-hi :theta-lo theta-lo))

(defun controller-step (ctrl gain hlc)
  "Feed one measured GAIN at causal time HLC.  Returns a REPARTITION-EVENT if the
physical partition changes, else NIL.  Deterministic in (state, gain)."
  (ecase (repartition-controller-state ctrl)
    (:merged
     (when (> gain (repartition-controller-theta-hi ctrl))
       (setf (repartition-controller-state ctrl) :split)
       (%make-repartition-event :kind :split :hlc hlc :gain gain)))
    (:split
     (when (< gain (repartition-controller-theta-lo ctrl))
       (setf (repartition-controller-state ctrl) :merged)
       (%make-repartition-event :kind :merge :hlc hlc :gain gain)))))

(defun run-controller (ctrl gains hlcs)
  "Run CTRL over a parallel trace of GAINS and HLCS; return the ordered list of
repartition events that fired.  Deterministic: identical traces give identical
events -- the physical layer is itself replayable."
  (let ((events '()))
    (loop for gain in gains for hlc in hlcs
          for ev = (controller-step ctrl gain hlc)
          when ev do (push ev events))
    (nreverse events)))

(defun count-naive-flaps (theta gains)
  "How many times a naive SINGLE-threshold controller would switch on GAINS: one
switch per crossing of THETA.  The number the hysteresis dead band collapses."
  (let ((flaps 0) (state nil))
    (dolist (g gains flaps)
      (let ((now (if (> g theta) :split :merged)))
        (when (and state (not (eq state now))) (incf flaps))
        (setf state now)))))
