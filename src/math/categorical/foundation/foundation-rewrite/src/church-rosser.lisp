;;;; church-rosser.lisp --- TLC beta-reduction with confluence.
;;;;
;;;; Lean source: ChurchRosser.lean.
;;;;
;;;; Theorem (church_rosser, ChurchRosser.lean): for the typed lambda
;;;; calculus with the deterministic left-biased single-step reducer
;;;; betaStep, the multi-step reduction relation BetaStar is confluent.
;;;; The Lean proof goes via the abstract lemma
;;;; confluent_of_partial_function: the reflexive-transitive closure of
;;;; a partial-function graph is automatically confluent.
;;;;
;;;; Computational content (this file): represent terms as
;;;;   (:VAR  name)
;;;;   (:LAM  param body)
;;;;   (:APP  fn arg)
;;;; via the TLC-TERM struct.  BETA-STEP is the deterministic one-step
;;;; reducer (left-biased: leftmost-outermost).  NORMALIZE iterates it
;;;; up to *NORMALIZE-MAX-ITER* steps and signals DIVERGING-OMEGA on
;;;; non-termination.  CONFLUENT-P checks two terms reduce to the same
;;;; normal form.

(in-package #:rosette-foundation-rewrite)

(defstruct (tlc-term
            (:constructor %make-tlc-term)
            (:copier nil))
  "A typed lambda-calculus term variant.

KIND is one of :VAR, :LAM, :APP.  PARTS is a list whose shape depends
on KIND:
  :VAR — (NAME)                         — NAME is a symbol
  :LAM — (PARAM BODY)                   — PARAM symbol, BODY tlc-term
  :APP — (FN ARG)                       — both tlc-term

Terms are immutable.  Equality is structural (TLC-EQUAL-P)."
  (kind :var :type symbol :read-only t)
  (parts nil :type list :read-only t))

(defun make-tlc-term (kind &rest parts)
  "Convenience constructor: (make-tlc-term :var 'x), etc."
  (check-type kind symbol)
  (%make-tlc-term :kind kind :parts (copy-list parts)))

(defun tlc-var (name)
  "Construct a variable term."
  (make-tlc-term :var name))

(defun tlc-lam (param body)
  "Construct an abstraction term (λ PARAM. BODY)."
  (check-type param symbol)
  (check-type body tlc-term)
  (make-tlc-term :lam param body))

(defun tlc-app (fn arg)
  "Construct an application term (FN ARG)."
  (check-type fn tlc-term)
  (check-type arg tlc-term)
  (make-tlc-term :app fn arg))

(defun tlc-var-name (term)
  "Return the symbol carried by a :VAR term."
  (check-type term tlc-term)
  (assert (eq (tlc-term-kind term) :var) () "expected :VAR TLC term")
  (first (tlc-term-parts term)))

(defun tlc-lam-param (term)
  "Return the binder symbol carried by a :LAM term."
  (check-type term tlc-term)
  (assert (eq (tlc-term-kind term) :lam) () "expected :LAM TLC term")
  (first (tlc-term-parts term)))

(defun tlc-lam-body (term)
  "Return the body carried by a :LAM term."
  (check-type term tlc-term)
  (assert (eq (tlc-term-kind term) :lam) () "expected :LAM TLC term")
  (second (tlc-term-parts term)))

(defun tlc-app-fn (term)
  "Return the function part carried by an :APP term."
  (check-type term tlc-term)
  (assert (eq (tlc-term-kind term) :app) () "expected :APP TLC term")
  (first (tlc-term-parts term)))

(defun tlc-app-arg (term)
  "Return the argument part carried by an :APP term."
  (check-type term tlc-term)
  (assert (eq (tlc-term-kind term) :app) () "expected :APP TLC term")
  (second (tlc-term-parts term)))

(defun tlc-equal-p (s tt)
  "Structural equality of two TLC terms.  Alpha-equivalence is *not*
implemented: bound-variable names are compared literally.  This matches
the Lean source, where TLC.eq is structural and renaming is handled
upstream."
  (and (tlc-term-p s) (tlc-term-p tt)
       (eq (tlc-term-kind s) (tlc-term-kind tt))
       (let ((ps (tlc-term-parts s))
             (pt (tlc-term-parts tt)))
	 (case (tlc-term-kind s)
	   (:var (eq (first ps) (first pt)))
	   (:lam (and (eq (first ps) (first pt))
	              (tlc-equal-p (second ps) (second pt))))
	   (:app (and (tlc-equal-p (first ps) (first pt))
	              (tlc-equal-p (second ps) (second pt))))))))

;;; --- Substitution -----------------------------------------------------
;;;
;;; A capture-avoiding substitution (or lack thereof) is a notorious
;;; source of bugs.  For v0.1.0 we only support the *closed-world* case
;;; the test-suite uses: substituting a closed value V into a body B.
;;; If a binding shadow occurs we recurse without renaming, which is the
;;; same convention as the Lean source's shallow betaStep — beta-η-eta
;;; semantics belong to v0.2 once a fresh-name supply is added.

(defun tlc-free-vars (term)
  "Set of free variables of TERM, as a list of symbols (no duplicates)."
  (case (tlc-term-kind term)
    (:var (list (tlc-var-name term)))
    (:lam (let ((param (tlc-lam-param term))
                (body (tlc-lam-body term)))
            (remove param (tlc-free-vars body))))
    (:app (let ((fn (tlc-app-fn term))
                (arg (tlc-app-arg term)))
            (union (tlc-free-vars fn) (tlc-free-vars arg) :test #'eq)))))

(defun tlc-substitute (term var value)
  "Substitute VALUE for free occurrences of VAR in TERM.  Capture-naive
under abstractions whose binder shadows VAR."
  (case (tlc-term-kind term)
    (:var (if (eq (tlc-var-name term) var)
              value
              term))
    (:lam (let ((param (tlc-lam-param term))
                (body (tlc-lam-body term)))
            (if (eq param var)
                term
                (tlc-lam param (tlc-substitute body var value)))))
    (:app (tlc-app (tlc-substitute (tlc-app-fn term)
                                   var value)
                   (tlc-substitute (tlc-app-arg term)
                                   var value)))))

;;; --- One-step beta-reduction (left-biased) ----------------------------

(defun beta-step (term)
  "Deterministic left-biased single step.

If TERM has the form ((λ x. B) V), reduce it to B[x:=V].  Otherwise
recurse left-first: try to step the function part, then the argument
part, then the body of an abstraction.  Returns the reduced term, or
NIL if TERM is in beta-normal form.

This matches Lean's betaStep: ChurchRosser.lean
betaRel_deterministic."
  (case (tlc-term-kind term)
    (:var nil)
    (:lam (let* ((param (tlc-lam-param term))
                 (body (tlc-lam-body term))
                 (b* (beta-step body)))
            (when b* (tlc-lam param b*))))
    (:app (let ((fn (tlc-app-fn term))
                (arg (tlc-app-arg term)))
            (cond
              ;; Redex: ((λ x. B) V).
              ((eq (tlc-term-kind fn) :lam)
               (let ((param (tlc-lam-param fn))
                     (body (tlc-lam-body fn)))
                 (tlc-substitute body param arg)))
              ;; Step the function part first.
              ((let ((f* (beta-step fn)))
                 (when f* (tlc-app f* arg))))
              ;; Then the argument.
              ((let ((a* (beta-step arg)))
                 (when a* (tlc-app fn a*))))
              (t nil))))))

(defparameter *normalize-max-iter* 10000
  "Default iteration cap for NORMALIZE.  Hitting it signals
DIVERGING-OMEGA — see the canonical Ω = (λx.xx)(λx.xx) test.")

(defun normalize (term &key (max-iter *normalize-max-iter*))
  "Iterate BETA-STEP until a beta-normal form is reached or MAX-ITER
applications have been made.  Signals DIVERGING-OMEGA on overflow.

Reference: ChurchRosser.lean nf_succ_of_eq_some, nf_mono."
  (check-type term tlc-term)
  (let ((current term))
    (dotimes (i max-iter
                (error 'diverging-omega :term term :max-iter max-iter))
      (let ((next (beta-step current)))
        (if (null next)
            (return current)
            (setf current next))))))

(defun confluent-p (term1 term2 &key (max-iter *normalize-max-iter*))
  "Return T iff TERM1 and TERM2 reduce to the same beta-normal form
within MAX-ITER steps.  This is the runtime witness of
ChurchRosser.lean::church_rosser specialised to the pair
\(t →*β u, t →*β v) ⇒ ∃w, u →*β w ∧ v →*β w': if both u and v
converge to the same normal form, we have w explicitly.

Returns NIL if normalisation succeeds but the normal forms differ;
re-signals DIVERGING-OMEGA if either term fails to converge."
  (let ((nf1 (normalize term1 :max-iter max-iter))
        (nf2 (normalize term2 :max-iter max-iter)))
    (tlc-equal-p nf1 nf2)))
