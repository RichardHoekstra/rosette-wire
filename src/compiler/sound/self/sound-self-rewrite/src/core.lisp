;;;; core.lisp --- a sound, certificate-carrying self-rewrite loop.
;;;;
;;;; The loop:
;;;;   improve(program)
;;;;     repeat:
;;;;       candidates <- rewrite-candidates(program, rules)      ; metacircular
;;;;       for each candidate (steepest first):
;;;;         verdict <- verify-rewrite(program, candidate)        ; equiv + improve
;;;;         if verdict = :confirmed: accept, record certificate, restart
;;;;       else: fixpoint, return program + the certificate chain
;;;;
;;;; SOUNDNESS.  A step is accepted only when rosette-claim-referee returns :CONFIRMED
;;;; on a composite of (1) a re-runnable behavioural-equivalence certificate
;;;; (rosette-egraph-saturate:certify-merge -- the e-graph merge gate, which re-runs
;;;; both forms on a battery of integer environments) and (2) a STRICT op-count
;;;; reduction.  A behaviour-changing rewrite fails (1) and is refused; op-count
;;;; strictly decreases on every accepted step, so the loop terminates.
;;;;
;;;; HOMOICONIC.  Rules are data: (NAME LHS RHS) with ?vars.  REWRITE-AT-ROOT
;;;; unifies LHS with a term and instantiates RHS (rosette-unification).  The rewriter
;;;; is an interpreter over s-expr rules, not generated code -- and unification
;;;; enforces non-linear patterns for free (the FACTOR rule's shared ?x must bind
;;;; to the same subterm on both sides, where a structural EQUAL check would not).

(in-package #:rosette-sound-self-rewrite)

;;; ----------------------------------------------------------------------
;;; Rules as s-expression data
;;; ----------------------------------------------------------------------

(defun rule-name (rule) (first rule))
(defun rule-lhs  (rule) (second rule))
(defun rule-rhs  (rule) (third rule))

(defparameter *simplify-rules*
  '((add-zero       (+ ?x 0)                 ?x)
    (add-zero-l     (+ 0 ?x)                 ?x)
    (mul-one        (* 1 ?x)                 ?x)
    (mul-one-r      (* ?x 1)                 ?x)
    (mul-zero       (* ?x 0)                 0)
    (mul-zero-l     (* 0 ?x)                 0)
    (factor-common  (+ (* ?x ?y) (* ?x ?z))  (* ?x (+ ?y ?z)))
    (fold-double    (+ ?x ?x)                (* 2 ?x))
    (mul-to-sq      (* ?x ?x)                (sq ?x)))
  "The default SIMPLIFYING rule set, as s-expr data.  Every rule shrinks or
re-shares its match; each is an arithmetic identity over {+ - * sq}.  Soundness
is NOT assumed from the rule -- every application is still verified on the
battery before it is accepted (the rules are a proposal, the certificate is the
judge).")

;;; ----------------------------------------------------------------------
;;; The metacircular rewriter: unify LHS, instantiate RHS
;;; ----------------------------------------------------------------------

(defun rewrite-at-root (rule expr)
  "If RULE's LHS unifies with EXPR at the root, return (values INSTANTIATED-RHS
RULE-NAME); otherwise NIL.  LHS/RHS are parsed under one shared variable scope so
a ?x in both refers to the same logic variable (non-linear patterns work)."
  (let* ((vmap (make-hash-table :test 'equal))
         (lhs (u:parse-term (rule-lhs rule) vmap))
         (rhs (u:parse-term (rule-rhs rule) vmap)))
    (multiple-value-bind (subst ok) (u:mgu lhs expr)
      (when ok
        (values (u:term->sexp (u:resolve rhs subst)) (rule-name rule))))))

(defun rewrite-candidates (expr &optional (rules *simplify-rules*))
  "Every distinct (CANDIDATE-PROGRAM . RULE-NAME) obtained by applying ONE rule at
ONE position (root or any subterm) of EXPR.  Congruence closure: a rewrite valid
for a subterm is valid in context because every operator is a function of its
arguments."
  (let ((out '()))
    (labels ((rec (e rebuild)
               (dolist (rule rules)
                 (multiple-value-bind (new name) (rewrite-at-root rule e)
                   (when (and new (not (equal new e)))
                     (push (cons (funcall rebuild new) name) out))))
               (when (consp e)
                 (loop for i from 1 below (length e)
                       for sub = (nth i e)
                       do (rec sub
                               (let ((i i))
                                 (lambda (x)
                                   (funcall rebuild
                                            (let ((c (copy-list e)))
                                              (setf (nth i c) x) c)))))))))
      (rec expr #'identity))
    (delete-duplicates (nreverse out) :test #'equal :key #'car :from-end t)))

(defun rules->functions (&optional (rules *simplify-rules*))
  "Adapt the s-expr RULES into the (expr -> expr-or-NIL) root-rewrite functions
rosette-egraph-saturate:saturate expects -- so the very same metacircular rules drive
the independent e-graph equivalence proof."
  (mapcar (lambda (r) (lambda (e) (values (rewrite-at-root r e)))) rules))

;;; ----------------------------------------------------------------------
;;; Metrics
;;; ----------------------------------------------------------------------

(defun program-size (expr)
  "The op-count (interior-node count) of EXPR -- the size metric the loop
strictly reduces."
  (es:op-count expr))

(defun program-free-vars (expr &optional acc)
  "The variable symbols appearing in EXPR."
  (cond ((and (symbolp expr) (not (null expr)))
         (if (member expr acc :test #'eq) acc (cons expr acc)))
        ((consp expr) (dolist (a (rest expr) acc) (setf acc (program-free-vars a acc))))
        (t acc)))

(defun program-battery (expr &key (size 60) (seed 0))
  "A behavioural battery of integer environments covering EXPR's free variables."
  (es:make-battery (or (program-free-vars expr) '(a)) :size size :seed seed))

(defun eval-cost (expr battery)
  "Total primitive operations EXPR executes across BATTERY (the step-count metric,
reported alongside size)."
  (let ((total 0))
    (dolist (env battery total)
      (let ((counter (cons 0 0)))
        (es:expr-eval expr env counter)
        (incf total (car counter))))))

;;; ----------------------------------------------------------------------
;;; The sound per-step verifier
;;; ----------------------------------------------------------------------

(defstruct (rewrite-step (:constructor %make-rewrite-step))
  "One ACCEPTED rewrite, with its evidence."
  rule before after size-before size-after certificate verdict)

(defun verify-rewrite (before after &key battery)
  "Adjudicate the rewrite BEFORE -> AFTER.  Returns (values VERDICT CERTIFICATE):
a composite rosette-claim-referee verdict over (1) behavioural equivalence on BATTERY
(a re-runnable certify-merge certificate) and (2) a STRICT op-count reduction.
VERDICT-STATUS is :CONFIRMED iff BOTH hold -- the accept gate."
  (let* ((battery (or battery (program-battery before)))
         (cert (es:certify-merge before after battery))
         (passed (es:certificate-passed cert))
         (size-before (program-size before))
         (size-after (program-size after))
         (verdict (ref:adjudicate
                   (ref:composite-claim
                    "sound-improving-rewrite"
                    (list (ref:point-claim "behavioural-equivalence"
                                           1 (lambda () (if passed 1 0)) :tolerance 0)
                          (ref:upper-bound-claim "strict-size-reduction"
                                                 (1- size-before)
                                                 (lambda () size-after)))))))
    (values verdict cert)))

(defun certified-improvement-p (verdict)
  "T iff VERDICT is :CONFIRMED -- an equivalent AND strictly smaller rewrite."
  (eq (ref:verdict-status verdict) :confirmed))

;;; ----------------------------------------------------------------------
;;; The closed loop
;;; ----------------------------------------------------------------------

(defun improve (program &key (rules *simplify-rules*) battery (max-steps 200))
  "Drive PROGRAM to a fixpoint by SOUND self-rewriting.  Each round proposes the
candidate rewrites, tries them steepest-first, and ACCEPTS the first whose verdict
is :CONFIRMED (equivalent on BATTERY and strictly smaller).  Returns (values
OPTIMIZED-PROGRAM STEPS): STEPS is the ordered list of REWRITE-STEP records --
the certificate chain.  Every intermediate program is behaviourally equal to
PROGRAM, by construction."
  (let* ((battery (or battery (program-battery program)))
         (current program)
         (steps '()))
    (loop repeat max-steps do
      (let* ((candidates (rewrite-candidates current rules))
             (ordered (stable-sort (copy-list candidates) #'<
                                   :key (lambda (c) (program-size (car c)))))
             (accepted nil))
        (dolist (c ordered)
          (let ((after (car c)) (name (cdr c)))
            (multiple-value-bind (verdict cert) (verify-rewrite current after :battery battery)
              (when (certified-improvement-p verdict)
                (push (%make-rewrite-step
                       :rule name :before current :after after
                       :size-before (program-size current)
                       :size-after (program-size after)
                       :certificate cert :verdict verdict)
                      steps)
                (setf current after accepted t)
                (return)))))
        (unless accepted (return))))
    (values current (nreverse steps))))

(defun improvement-total (steps)
  "Total op-count removed across STEPS (size-before of the first minus size-after
of the last)."
  (if (null steps)
      0
      (- (rewrite-step-size-before (first steps))
         (rewrite-step-size-after (car (last steps))))))

;;; ----------------------------------------------------------------------
;;; Independent e-graph corroboration
;;; ----------------------------------------------------------------------

(defun egraph-proves-equivalent-p (e1 e2 &key (rules *simplify-rules*) battery)
  "Independently prove E1 == E2 by certificate-gated equality saturation: grow an
e-graph from E1 under the same metacircular RULES (adapted to functions) and check
E1 and E2 land in the same e-class.  This is the e-graph (congruence-closure)
proof complementing the per-step battery certificates."
  (let* ((battery (or battery (program-battery e1)))
         (sat (es:saturate (list e1 e2)
                           :rules (rules->functions rules)
                           :battery battery)))
    (es:equivalent-p sat e1 e2)))
