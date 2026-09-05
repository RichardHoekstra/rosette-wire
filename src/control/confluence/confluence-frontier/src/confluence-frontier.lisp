;;;; confluence-frontier.lisp --- where a computation stops being order-free.
;;;;
;;;; An interaction net / local rewrite system is SYMBOLIC exactly where it is
;;;; confluent: concurrent redexes commute, so the normal form is independent of
;;;; the order you fire them -- no consensus, and (the sharp point) NO tiebreak.
;;;; A verifier decides the result for free.  It becomes NEURO exactly at a
;;;; non-confluent redex whose outcome genuinely depends on order: there is no
;;;; local fact about which branch is taken, so an external MEASURE must supply
;;;; the choice -- the Born rule / the learned amplitude / the sampler.  This is
;;;; the same split as measurement vs unitary evolution, and the same split as
;;;; rosette-verifier-router's harmonic vs coboundary reward.
;;;;
;;;; The op algebra is rosette-causal-frontier's own (:add commutes, :set does not),
;;;; so the semantics are not re-implemented here -- they are cf's, evaluated by
;;;; forcing a linearization through a dependency chain.  What is new is the
;;;; classification: which redexes are order-free (symbolic) and which require a
;;;; measure (neuro), plus the honest subtlety that order-freedom has TWO
;;;; sources -- commuting (clean) and forgetting (a :set reset that erases the
;;;; branch distinction, a projective measurement onto one state) -- so
;;;; confluence is SUFFICIENT but not NECESSARY for tiebreak-freedom.

(in-package #:rosette-confluence-frontier)

(defparameter +cell+ "x")

(defun op (kind value)
  "An op spec on the redex cell: KIND in (:add :set), integer VALUE."
  (unless (member kind '(:add :set))
    (error "op kind must be :add or :set, got ~S" kind))
  (check-type value integer)
  (list :kind kind :value value))

(defun %chain-events (op-specs)
  "Turn OP-SPECS (in a chosen order) into a cf dependency chain that forces
exactly that linearization, so cf's own evaluator applies them in this order."
  (let ((events '())
        (prev nil))
    (loop for spec in op-specs for i from 0
          for id = (format nil "r~D" i)
          do (push (cf:make-event :id id :hlc (cf:make-hlc (1+ i) 0)
                                  :deps (if prev (list prev) '())
                                  :target +cell+
                                  :kind (getf spec :kind)
                                  :value (getf spec :value))
                   events)
             (setf prev id))
    (nreverse events)))

(defun order-result (op-specs)
  "Apply OP-SPECS in the given order (via cf's evaluator) and return the cell
value.  This IS rosette-causal-frontier's op semantics, not a re-implementation."
  (cf:cell-state (cf:causal-final-state (%chain-events op-specs)) +cell+))

(defun %permutations (list)
  (if (null list)
      (list '())
      (loop for x in list
            append (mapcar (lambda (p) (cons x p))
                           (%permutations (remove x list :count 1 :test #'eq))))))

(defun commuting-p (a b)
  "T iff the two ops A and B commute: applying them in either order gives the
same cell value."
  (= (order-result (list a b)) (order-result (list b a))))

(defun confluent-p (op-specs)
  "T iff every pair of ops commutes -- LOCAL confluence.  For this terminating
one-cell algebra local confluence implies global order-independence (Newman),
so CONFLUENT-P => ORDER-INDEPENDENT-P; over free permutations of this algebra the
two in fact coincide."
  (loop for (a . rest) on op-specs
        always (every (lambda (b) (commuting-p a b)) rest)))

(defun order-independent-p (op-specs)
  "T iff every permutation of OP-SPECS yields the same cell value -- the redex is
order-FREE, the symbolic/verifiable region.  Implied by CONFLUENT-P.  A second
source of order-freedom, FORGETTING (a dominating final :set that erases the
branch distinction -- a projective measurement onto one state), appears only once
CAUSAL dependencies force an op last; over free permutations it cannot, so here
order-independence coincides with confluence.  That causal forgetting is a named
frontier, not witnessed at this layer."
  (let ((results (mapcar #'order-result (%permutations op-specs))))
    (or (null results)
        (every (lambda (r) (= r (first results))) results))))

;;; ---------------------------------------------------------------------------
;;; The frontier.
;;; ---------------------------------------------------------------------------

(defun measurement-needed-p (op-specs)
  "T iff the redex is ORDER-DEPENDENT: its outcome genuinely depends on the order
its concurrent ops resolve in, so no verifier can derive the result -- an
external MEASURE (a choice of order) is required.  This is the neuro region; its
complement is the symbolic region."
  (not (order-independent-p op-specs)))

;;; ---------------------------------------------------------------------------
;;; Symbolic side: a confluent/order-free redex has a unique result, verify-only.
;;; ---------------------------------------------------------------------------

(defun unique-result (op-specs)
  "The single well-defined result of an ORDER-FREE redex.  Signals if a measure
is needed -- you cannot read a unique value off a genuinely non-confluent redex."
  (unless (order-independent-p op-specs)
    (error "redex is order-dependent; a measure is required, not a unique value"))
  (order-result op-specs))

(defun verify-resolution (op-specs order claimed)
  "Symbolic gate: T iff ORDER is a permutation of OP-SPECS and evaluating it
gives CLAIMED.  A verifier can always CHECK a resolved branch cheaply -- even at
a non-confluent redex where it could not have CHOSEN the branch.  Verify is
confluent; choice is not."
  (and (%permutation-of-p order op-specs)
       (= claimed (order-result order))))

(defun %permutation-of-p (a b)
  (and (= (length a) (length b))
       (null (set-exclusive-or a b :test #'equal))))

;;; ---------------------------------------------------------------------------
;;; Neuro side: at a non-confluent redex a MEASURE collapses the superposition.
;;; ---------------------------------------------------------------------------

(defun resolve-with-measure (op-specs measure)
  "Collapse the redex with an external MEASURE -- a function from the op set to a
chosen total order (the sampler / the learned amplitude's argmax).  Returns
(values RESULT ORDER).  At an order-free redex every measure agrees; at a
non-confluent redex different measures give different results, which is precisely
why the choice is irreducibly external."
  (let ((order (funcall measure (copy-list op-specs))))
    (unless (%permutation-of-p order op-specs)
      (error "measure must return a permutation of the ops"))
    (values (order-result order) order)))
