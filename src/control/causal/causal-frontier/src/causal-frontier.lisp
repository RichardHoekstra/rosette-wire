;;;; causal-frontier.lisp --- the causal partial order and its two laws.
;;;;
;;;; rosette-game-protocol models a game as a totally-ordered input sequence -- one
;;;; global tick everyone agrees on.  That total order is a consensus artifact
;;;; (TiDi: pause the world so everyone agrees who was first).  The honest
;;;; object is a CAUSAL PARTIAL ORDER: events carry hybrid logical clocks and
;;;; explicit causal dependencies; genuinely-concurrent events have no
;;;; world-fact about their order and are settled only by a deterministic
;;;; tiebreak.  Each state cell (the "target") is adjudicated by exactly one
;;;; arbiter, so contention stays local.
;;;;
;;;; Two laws are proved here, and together they are the soundness of the
;;;; whole gravity-well two-tree design:
;;;;
;;;;   QUOTIENT      -- adjudicating the causal order equals running the total
;;;;                    order (rosette-game-protocol:run-game) over any consistent
;;;;                    linearization.  game-protocol IS the totally-ordered
;;;;                    quotient of this causal object.
;;;;
;;;;   PARTITION-INVARIANCE -- the per-cell final state is independent of which
;;;;                    arbiter partition (coarse: one global arbiter; fine:
;;;;                    per-cell) evaluates it, PROVIDED every op on one cell is
;;;;                    adjudicated by one arbiter.  I.e. moving the physical
;;;;                    boundary is a GAUGE -- as long as it never cuts through
;;;;                    a single cell's op-stream.  That proviso is exactly why
;;;;                    the physical repartition is a discrete switch along
;;;;                    cell boundaries, never a membrane that sweeps through an
;;;;                    arbiter.

(in-package #:rosette-causal-frontier)

(defvar +no-arbiter+ (list '#:no-arbiter)
  "Unique sentinel for \"no arbiter yet recorded for this cell\".")

;;; ---------------------------------------------------------------------------
;;; Hybrid logical clock: causality + a bound near physical time, bounded size.
;;; ---------------------------------------------------------------------------

(defstruct (hlc (:constructor make-hlc (&optional (logical 0) (counter 0)))
                (:copier nil))
  (logical 0 :type unsigned-byte)
  (counter 0 :type unsigned-byte))

(defun hlc< (a b)
  "Strict lexicographic order on (LOGICAL, COUNTER).  A deterministic total
order used only to break ties between genuinely-concurrent events."
  (or (< (hlc-logical a) (hlc-logical b))
      (and (= (hlc-logical a) (hlc-logical b))
           (< (hlc-counter a) (hlc-counter b)))))

(defun hlc-tick (clock now)
  "Advance CLOCK for a local event at physical time NOW (an integer)."
  (if (> now (hlc-logical clock))
      (make-hlc now 0)
      (make-hlc (hlc-logical clock) (1+ (hlc-counter clock)))))

(defun hlc-merge (clock remote now)
  "Merge a REMOTE clock observed at physical time NOW into CLOCK (receive)."
  (let ((l (max (hlc-logical clock) (hlc-logical remote) now)))
    (cond ((and (= l (hlc-logical clock)) (= l (hlc-logical remote)))
           (make-hlc l (1+ (max (hlc-counter clock) (hlc-counter remote)))))
          ((= l (hlc-logical clock)) (make-hlc l (1+ (hlc-counter clock))))
          ((= l (hlc-logical remote)) (make-hlc l (1+ (hlc-counter remote))))
          (t (make-hlc l 0)))))

;;; ---------------------------------------------------------------------------
;;; Events: a command in the causal log.
;;; ---------------------------------------------------------------------------

(defstruct (event (:copier nil))
  "One adjudicated operation.  ID is a unique string; ACTOR issued it; HLC
timestamps it; DEPS is the list of event ids this one causally depends on (its
reactive frontier); it mutates the state cell named TARGET by KIND (:add or
:set) VALUE."
  (id "" :type string)
  (actor "" :type string)
  (hlc (make-hlc) :type hlc)
  (deps '() :type list)
  (target "" :type string)
  (kind :add :type (member :add :set))
  (value 0 :type integer))

(defun %index-by-id (events)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e events h)
      (when (gethash (event-id e) h)
        (error "Duplicate event id ~S" (event-id e)))
      (setf (gethash (event-id e) h) e))))

(defun %ancestors (event by-id)
  "Transitive causal ancestor id-set of EVENT (its full past light-cone)."
  (let ((seen (make-hash-table :test 'equal))
        (stack (copy-list (event-deps event))))
    (loop while stack do
      (let ((id (pop stack)))
        (unless (gethash id seen)
          (setf (gethash id seen) t)
          (let ((dep (gethash id by-id)))
            (unless dep (error "Event ~S depends on unknown id ~S"
                               (event-id event) id))
            (dolist (d (event-deps dep)) (push d stack))))))
    seen))

(defun happens-before-p (a b events)
  "T iff event A is in the causal past of event B (A -> B)."
  (let ((by-id (if (hash-table-p events) events (%index-by-id events))))
    (values (gethash (event-id a) (%ancestors b by-id)))))

(defun concurrent-p (a b events)
  "T iff A and B are causally unordered -- genuinely simultaneous, with no
world-fact about which was first."
  (and (not (string= (event-id a) (event-id b)))
       (not (happens-before-p a b events))
       (not (happens-before-p b a events))))

;;; ---------------------------------------------------------------------------
;;; Linearization: a deterministic consistent cut of the partial order.
;;; ---------------------------------------------------------------------------

(defun %event< (a b)
  "The deterministic tiebreak between concurrent events: HLC then id.  Every
replayer computes it identically, so the order it induces is LAW, not a
world-fact."
  (or (hlc< (event-hlc a) (event-hlc b))
      (and (not (hlc< (event-hlc b) (event-hlc a)))
           (string< (event-id a) (event-id b)))))

(defun linearize (events &key (closed t))
  "Return EVENTS in one deterministic topological order: respect happens-before
(never an effect before its cause), and among ready events pick the tiebreak
minimum.  This is the canonical consistent cut.  With CLOSED true a dependency
on an id outside EVENTS is an error (integrity check for a full run); with
CLOSED false such a dangling dep is treated as already-satisfied (used when an
arbiter linearizes only its own cells' ops, whose causes on other arbiters lie
in the past and cannot reorder these ops)."
  (let* ((present (%index-by-id events))
         (indeg (make-hash-table :test 'equal))
         (children (make-hash-table :test 'equal)))
    (dolist (e events)
      (let ((live-deps (if closed
                           (event-deps e)
                           (remove-if-not (lambda (d) (gethash d present))
                                          (event-deps e)))))
        (when closed
          (dolist (d live-deps)
            (unless (gethash d present)
              (error "Event ~S depends on unknown id ~S" (event-id e) d))))
        (setf (gethash (event-id e) indeg)
              (+ (gethash (event-id e) indeg 0) (length live-deps)))
        (dolist (d live-deps)
          (push e (gethash d children)))))
    (let ((ready (sort (remove-if-not
                        (lambda (e) (zerop (gethash (event-id e) indeg 0)))
                        events)
                       #'%event<))
          (out '()))
      (loop while ready do
        (let ((e (pop ready)))
          (push e out)
          (dolist (c (gethash (event-id e) children))
            (when (zerop (decf (gethash (event-id c) indeg)))
              ;; insert c into the sorted ready list
              (setf ready (merge 'list (list c) ready #'%event<))))))
      (let ((result (nreverse out)))
        (unless (= (length result) (length events))
          (error "Causal graph has a cycle; ~D of ~D events ordered"
                 (length result) (length events)))
        result))))

;;; ---------------------------------------------------------------------------
;;; State + hashing (shared with the game-protocol backend below).
;;; ---------------------------------------------------------------------------

(defun cell-state (state target)
  "Value of cell TARGET in STATE (a sorted alist), or 0."
  (or (cdr (assoc target state :test #'string=)) 0))

(defun %apply-op (state event)
  "Apply EVENT's op to STATE (functional; returns a fresh sorted alist)."
  (let* ((target (event-target event))
         (old (cell-state state target))
         (new (ecase (event-kind event)
                (:add (+ old (event-value event)))
                (:set (event-value event))))
         (rest (remove target state :key #'car :test #'string=)))
    (sort (acons target new rest) #'string< :key #'car)))

(defun state-hash (state)
  "Deterministic FNV-1a hash of a sorted (target . value) alist."
  (let ((h 2166136261))
    (flet ((mix (x)
             (let ((x (logand x #xffffffffffffffff)))
               (setf h (logand (* (logxor h (logand x #xffffffff)) 16777619)
                               #xffffffffffffffff))
               (setf h (logand (* (logxor h (ash x -32)) 16777619)
                               #xffffffffffffffff)))))
      (dolist (cell (sort (copy-alist state) #'string< :key #'car) h)
        (loop for ch across (car cell) do (mix (char-code ch)))
        (mix (logand (cdr cell) #xffffffffffffffff))
        (mix (if (minusp (cdr cell)) 1 0))))))

;;; ---------------------------------------------------------------------------
;;; Adjudication: per-cell, ordered by (happens-before, tiebreak).
;;; ---------------------------------------------------------------------------

(defun %apply-order (events &key (closed t))
  "Apply EVENTS in canonical linearized order to a fresh state; return the
sorted (target . value) alist."
  (let ((state '()))
    (dolist (e (linearize events :closed closed) state)
      (setf state (%apply-op state e)))))

(defun causal-final-state (events)
  "The final per-cell state of the causal run: apply the canonical
linearization.  Distinct cells are independent, so a cell's value depends only
on the relative order of that cell's ops -- which the linearization fixes
deterministically."
  (%apply-order events :closed t))

(defun one-target-one-arbiter-p (partition events)
  "T iff PARTITION (a function target -> arbiter id) never assigns two ops on the
same cell to different arbiters.  Because PARTITION keys on TARGET this holds for
any cell-respecting partition; it is the precondition of PARTITION-INVARIANT-P
and the exact thing a physical boundary must never violate (a membrane sweeping
through one arbiter's op-stream would)."
  (unless (functionp partition) (return-from one-target-one-arbiter-p nil))
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (e events t)
      (let* ((target (event-target e))
             (arb (funcall partition target))
             (prev (gethash target seen +no-arbiter+)))
        (if (eq prev +no-arbiter+)
            (setf (gethash target seen) arb)
            (unless (equal prev arb) (return nil)))))))

(defun adjudicate (events partition)
  "Evaluate EVENTS under an arbiter PARTITION (target -> arbiter id): each
arbiter linearizes and applies the ops for the cells it owns; results merge over
the disjoint cells.  Because a cell's ops are never split across arbiters, the
result equals CAUSAL-FINAL-STATE for every cell-respecting partition -- the
content of the invariance law."
  (unless (one-target-one-arbiter-p partition events)
    (error "PARTITION splits a cell across arbiters; not cell-respecting"))
  (let ((by-arbiter (make-hash-table :test 'equal))
        (state '()))
    (dolist (e events)
      (push e (gethash (funcall partition (event-target e)) by-arbiter)))
    (let ((arbiters (sort (loop for k being the hash-key of by-arbiter
                                collect k)
                          #'string< :key #'princ-to-string)))
      (dolist (arb arbiters)
        ;; arbiter owns whole cells; drop deps on other arbiters' past events
        (dolist (cell (%apply-order (nreverse (gethash arb by-arbiter))
                                    :closed nil))
          (push cell state)))
      (sort state #'string< :key #'car))))

(defun partition-invariant-p (events part-a part-b)
  "T iff EVENTS adjudicate to the same per-cell state under PART-A and PART-B.
The gauge-check: physical repartition does not change the logical world."
  (= (state-hash (adjudicate events part-a))
     (state-hash (adjudicate events part-b))))

;;; ---------------------------------------------------------------------------
;;; rosette-game-protocol conformance: the totally-ordered shadow.
;;; ---------------------------------------------------------------------------

(defstruct (cf-world (:copier nil))
  "A world for the game-protocol backend: accumulated cell STATE and a TICK.
game-advance applies one event; run-game over (linearize events) is the total
order whose quotient is the causal object."
  (tick 0 :type unsigned-byte)
  (cells '() :type list))

(defmethod proto:game-advance ((w cf-world) (e event) &key dt)
  (declare (ignore dt))
  (make-cf-world :tick (1+ (cf-world-tick w))
                 :cells (%apply-op (cf-world-cells w) e)))

(defmethod proto:game-hash ((w cf-world))
  (state-hash (cf-world-cells w)))

(defmethod proto:game-tick ((w cf-world))
  (cf-world-tick w))
;; capture / restore inherit identity: cf-world is never mutated in place.

(defun quotient-consistent-p (events)
  "T iff running the TOTAL order (rosette-game-protocol:run-game over the
deterministic linearization) reaches the same state as adjudicating the CAUSAL
partial order.  game-protocol is the totally-ordered quotient of this object."
  (let* ((run (proto:run-game (make-cf-world) (linearize events)))
         (total-hash (proto:game-run-final-hash run)))
    (and (proto:replay-p run)
         (= total-hash (state-hash (causal-final-state events))))))
