;;;; tests.lisp --- law-verified tests for rosette-sound-self-rewrite.
;;;;
;;;; The laws under test:
;;;;   - the metacircular (unification) rewriter matches and instantiates s-expr
;;;;     rules, with non-linear patterns enforced (FACTOR-COMMON's shared ?x);
;;;;   - the closed loop drives a seeded suboptimal program to a strictly smaller,
;;;;     behaviourally-equivalent fixpoint, every accepted step certificate-carrying
;;;;     (and the certificates re-run);
;;;;   - the equivalence is independently e-graph (congruence-closure) proven;
;;;;   - SOUNDNESS: a behaviour-changing rewrite is never :confirmed, and the loop
;;;;     never accepts one even when it is offered a size-reducing bogus rule.

(defpackage #:rosette-sound-self-rewrite/tests
  (:use #:cl #:rosette-sound-self-rewrite)
  (:local-nicknames (#:es #:rosette-egraph-saturate)
                    (#:ref #:rosette-claim-referee)))

(in-package #:rosette-sound-self-rewrite/tests)

(defmacro is (c msg) `(unless ,c (error "FAIL: ~A" ,msg)))

(defparameter *seed* '(+ (* a (+ b 0)) (* a (+ c 0)))
  "A suboptimal program (size 5) equal to a*(b+c).")
(defparameter *optimum* '(* a (+ b c)))

(defun run-all-tests ()
  (let ((n 0))
    (flet ((ok (c msg) (incf n) (is c msg)))

      ;; --- 1. the metacircular rewriter -----------------------------------
      (ok (equal 'a (rewrite-at-root '(add-zero (+ ?x 0) ?x) '(+ a 0)))
          "rewrite-at-root: (+ a 0) -> a")
      (ok (equal '(* a (+ b c))
                 (rewrite-at-root '(factor-common (+ (* ?x ?y) (* ?x ?z)) (* ?x (+ ?y ?z)))
                                  '(+ (* a b) (* a c))))
          "rewrite-at-root: factor a common left factor")
      (ok (null (rewrite-at-root '(factor-common (+ (* ?x ?y) (* ?x ?z)) (* ?x (+ ?y ?z)))
                                 '(+ (* a b) (* q c))))
          "rewrite-at-root: non-linear ?x refuses a mismatched factor (unification)")
      (ok (null (rewrite-at-root '(add-zero (+ ?x 0) ?x) '(* a 0)))
          "rewrite-at-root: no match -> NIL")
      (ok (plusp (length (rewrite-candidates *seed*)))
          "rewrite-candidates proposes at least one rewrite of the seed")

      ;; --- 2. the per-step verifier is the gate ---------------------------
      (multiple-value-bind (v c) (verify-rewrite '(+ a 0) 'a)
        (ok (certified-improvement-p v) "verify-rewrite: a sound shrinking rewrite is :confirmed")
        (ok (es:certificate-passed c) "verify-rewrite: its equivalence certificate passes"))
      (multiple-value-bind (v c) (verify-rewrite '(* a (+ b c)) '(+ (* a b) (* a c)))
        (declare (ignore c))
        (ok (not (certified-improvement-p v))
            "verify-rewrite: an equivalent but LARGER rewrite is refused (no improvement)"))

      ;; --- 3. the closed loop -> a strictly better, equivalent fixpoint ----
      (multiple-value-bind (opt steps) (improve *seed*)
        (ok (equal *optimum* opt) "improve: seed is driven to a*(b+c)")
        (ok (< (program-size opt) (program-size *seed*)) "improve: strictly smaller")
        (ok (= 3 (improvement-total steps)) "improve: 3 op-count removed (5 -> 2)")
        (ok (es:forms-equal-on-battery-p *seed* opt (program-battery *seed*))
            "improve: optimum is behaviourally equivalent to the seed")
        (ok (every (lambda (s) (es:certificate-passed (rewrite-step-certificate s))) steps)
            "improve: every accepted step carries a passing equivalence certificate")
        (ok (every (lambda (s) (es:runtime-verify (rewrite-step-certificate s))) steps)
            "improve: every certificate RE-RUNS (re-runnable theorem)")
        (ok (every (lambda (s) (< (rewrite-step-size-after s) (rewrite-step-size-before s))) steps)
            "improve: every accepted step strictly reduces size")
        (ok (null (nth-value 1 (improve opt))) "improve: the optimum is a fixpoint (no further step)"))

      ;; --- 4. independent e-graph proof of the result ---------------------
      (ok (egraph-proves-equivalent-p *seed* *optimum*)
          "the seed and the optimum land in the same e-class (congruence-closure proof)")

      ;; --- 5. SOUNDNESS: a behaviour-changing rewrite is refused ----------
      (multiple-value-bind (v c) (verify-rewrite '(+ a a) 'a)   ; FALSE: a+a /= a
        (ok (not (certified-improvement-p v))
            "soundness: (+ a a) -> a is NOT :confirmed though it is smaller")
        (ok (not (es:certificate-passed c))
            "soundness: its equivalence certificate FAILS on the battery"))
      ;; the loop, GIVEN a size-reducing bogus rule, still never breaks correctness
      (let* ((bogus '((bogus-shrink (+ ?x ?x) ?x)))
             (rules (append *simplify-rules* bogus))
             (seed '(+ (* a 1) (* a 1))))
        (multiple-value-bind (opt steps) (improve seed :rules rules)
          (ok (null (member 'bogus-shrink (mapcar #'rewrite-step-rule steps)))
              "soundness: the loop never accepts the behaviour-changing bogus rule")
          (ok (es:forms-equal-on-battery-p seed opt (program-battery seed))
              "soundness: the loop's result is still behaviourally equivalent to the seed")))

      ;; the calibration exploit: a high-degree form that fools a SAMPLED gate
      ;; (agrees on all of [-7,7]) but is NOT equivalent -- refused now that the
      ;; equivalence check is symbolic (polynomial normal form), not a battery.
      (let* ((prod (cons '* (loop for k from -7 to 7 collect (list '- 'a k))))
             (evil (list '+ 'a prod)))
        (multiple-value-bind (v c) (verify-rewrite evil 'a)
          (ok (not (certified-improvement-p v))
              "soundness: Schwartz-Zippel exploit (a+prod(a-k) -> a) is NOT :confirmed")
          (ok (not (es:certificate-passed c))
              "soundness: the exploit's equivalence certificate is refused (symbolic gate)"))))

    (format t "~&rosette-sound-self-rewrite: ~D assertions passed.~%" n)
    n))
