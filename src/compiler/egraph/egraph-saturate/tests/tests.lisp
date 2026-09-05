;;;; tests.lisp --- tests for rosette-egraph-saturate.
;;;;
;;;; The curated demo from scratch-witness-content-addressed-egraph.lisp,
;;;; promoted to the library's gate: value-level collapse, certificate-gated
;;;; merge (a false equivalence is REFUSED), the autonomously-discovered
;;;; Horner == expanded identity, and the DAG op-sharing measurement.

(defpackage #:rosette-egraph-saturate/tests
  (:use #:cl #:rosette-egraph-saturate #:rosette-assert-core)
  (:import-from #:rosette-form-core #:intern-form #:form-address)
  (:export #:run-all-tests))

(in-package #:rosette-egraph-saturate/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-egraph-saturate")

    ;; ---------------------------------------------------- evaluation sanity
    (let ((env '((a . 3) (b . 4) (c . 2))))
      (check run (= 49 (expr-eval '(sq (+ a b)) env))
             "expr-eval: (a+b)^2 = 49 at a=3,b=4")
      (check run (= 49 (expr-eval '(+ (sq a) (* 2 (* a b)) (sq b)) env))
             "expr-eval: binomial-expanded agrees"))

    ;; ------------------------------------------- (1) COLLAPSE by saturation
    ;; Three differently-WRITTEN forms of (a+b)^2, plus a Horner-vs-expanded
    ;; pair. Saturation must collapse the truly equal ones into shared classes.
    (let* ((binom-sq  '(sq (+ a b)))
           (binom-exp '(+ (sq a) (* 2 (* a b)) (sq b)))
           (binom-mul '(+ (* a a) (+ (* a b) (* a b)) (* b b)))
           (horner    '(+ c (* a (+ b (* a c)))))     ; c + a(b + a c)
           (expanded  '(+ c (* a b) (* (* a a) c)))   ; same polynomial, expanded
           (sat (saturate (list binom-sq binom-exp binom-mul horner expanded))))

      (check run (equivalent-p sat binom-sq binom-exp)
             "collapse: sq(a+b) == binomial-expanded")
      (check run (equivalent-p sat binom-exp binom-mul)
             "collapse: binomial-expanded == all-multiplies form")
      (check run (equivalent-p sat binom-sq binom-mul)
             "collapse: sq(a+b) == all-multiplies form (transitive)")
      (check run (>= (length (class-spellings sat binom-sq)) 3)
             "collapse: binomial e-class has >= 3 distinct spellings")

      ;; ------------------------------------------------ (2) DISCOVERY
      ;; HORNER and EXPANDED were seeded as separate forms and never hand-merged.
      ;; Saturation (distribute/factor/comm) reaches across and collapses them:
      ;; c + a(b + a c) == c + ab + a^2 c is non-obvious from the syntax.
      (check run (equivalent-p sat horner expanded)
             "discovery: Horner form collapsed to expanded polynomial (auto)")

      ;; --------------------------------------- (3) THEOREM / certificate
      (let ((passing (remove-if-not #'certificate-passed
                                    (saturation-certificates sat))))
        (check run (>= (length passing) 1)
               "theorem: saturation minted >= 1 passing equivalence certificate")
        (check run (every #'runtime-verify passing)
               "theorem: every passing certificate survives runtime-verify")))

    ;; ------------------------------------ (4) REFUSE near-miss and false cert
    ;; A coefficient-3 near-miss must NOT join the (a+b)^2 class, and a
    ;; hand-built false equivalence must be refused by the gate.
    (let* ((binom-sq '(sq (+ a b)))
           (decoy    '(+ (sq a) (* 3 (* a b)) (sq b)))   ; coefficient 3, not 2
           (sat (saturate (list binom-sq decoy))))
      (check run (not (equivalent-p sat binom-sq decoy))
             "refuse: near-miss (coeff 3) stays in a SEPARATE e-class"))

    (let ((bad (certify-merge '(* a b) '(+ a b))))
      (check run (not (certificate-passed bad))
             "refuse: false 'a*b == a+b' certificate does NOT pass")
      (check run (not (runtime-verify bad))
             "refuse: false equivalence fails runtime-verify"))

    ;; -------------------- (4b) SCHWARTZ-ZIPPEL exploit is REFUSED (soundness)
    ;; a  vs  a + prod_{k=-7..7}(a-k): the product vanishes on every value the
    ;; [-7,7] battery can produce, so a SAMPLED gate is fooled (they "agree"),
    ;; yet they differ at a=8. The symbolic polynomial-NF gate refuses it.
    (let* ((prod (cons '* (loop for k from -7 to 7 collect (list '- 'a k))))
           (evil (list '+ 'a prod)))
      (check run (forms-equal-on-battery-p 'a evil)
             "exploit: the finite battery is FOOLED (a == a+prod agree on it)")
      (check run (not (symbolically-equal-p 'a evil))
             "exploit: polynomial-NF separates a from a+prod(a-k)")
      (let ((cert (certify-merge 'a evil)))
        (check run (not (certificate-passed cert))
               "exploit REFUSED: certify-merge does NOT pass the false equiv")
        (check run (not (runtime-verify cert))
               "exploit REFUSED: the false equiv fails runtime-verify"))
      (let ((sat (saturate (list 'a evil))))
        (check run (not (equivalent-p sat 'a evil))
               "exploit: saturation keeps a and a+prod(a-k) in separate classes")))

    ;; The gate keeps genuinely-distinct forms apart even when a caller tries
    ;; to merge them: only CERTIFICATE-PASSED authorizes the merge.
    (let ((sat (saturate '((* a b) (+ a b)))))
      (check run (not (equivalent-p sat '(* a b) '(+ a b)))
             "gate: certificate gate keeps genuinely-distinct forms apart"))

    ;; A genuine equivalence DOES certify and survives runtime-verify.
    (let ((good (certify-merge '(sq (+ a b))
                               '(+ (sq a) (* 2 (* a b)) (sq b)))))
      (check run (certificate-passed good)
             "certify-merge: genuine identity passes")
      (check run (runtime-verify good)
             "certify-merge: genuine identity survives runtime-verify"))

    ;; ------------------------------------------------- (5) SHARING meter
    ;; A composite that reuses (a+b)^2 four times: content-addressed (DAG) op
    ;; count is strictly smaller than the naive (tree) count -- here 50%.
    (let* ((shared '(sq (+ a b)))
           (composite `(+ (* ,shared ,shared)
                          (* ,shared (+ ,shared c))))
           (naive  (op-count composite))
           (dag    (op-count composite :shared t)))
      (check run (< dag naive)
             "sharing: DAG op-count strictly < naive tree op-count")
      (check run (<= dag (* 0.70 naive))
             "sharing: composite shares the reused block (>= 30% fewer ops)")
      (check run (= 1/2 (sharing-ratio composite))
             "sharing: measured ratio is exactly 50% on this composite"))

    ;; --------------------------------------------- content-addressing claim
    (let ((occ1 (intern-form '(sq (+ a b))))
          (occ2 (intern-form '(sq (+ a b))))
          (occ3 (intern-form '(sq (+ a c)))))
      (check run (eq occ1 occ2)
             "content-address: identical forms intern to the SAME Form (EQ)")
      (check run (= (form-address occ1) (form-address occ2))
             "content-address: equal value => equal address")
      (check run (/= (form-address occ1) (form-address occ3))
             "content-address: distinct value => distinct address"))))
