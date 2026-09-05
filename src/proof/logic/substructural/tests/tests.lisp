;;;; rosette-substructural/tests/tests.lisp --- Test suite.

(defpackage #:rosette-substructural/tests
  (:use #:cl #:rosette-substructural #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-substructural/tests)

;;; Formula shorthands.
(defun x* (a b) (list :tensor a b))   ; (x)  multiplicative conjunction
(defun -o (a b) (list :lolli a b))    ; -o   linear implication
(defun & (a b) (list :with a b))      ; &    additive conjunction
(defun plus* (a b) (list :plus a b))  ; (+)  additive disjunction

(defun run-all-tests ()
  "Run rosette-substructural tests."
  (with-test-run (run "rosette-substructural")

    ;; ===================================================================
    ;; The structural-rule lattice.
    ;; ===================================================================
    (check run (equal (admissible-structural-rules :unrestricted)
                      '(:exchange :weakening :contraction))
           "unrestricted admits all three structural rules")
    (check run (equal (admissible-structural-rules :linear) '(:exchange))
           "linear admits exchange only")
    (let ((linear (logic-structural-profile :linear))
          (unrestricted (logic-structural-profile :unrestricted)))
      (check run (and (structural-profile-exchange-p linear)
                      (not (structural-profile-weakening-p linear))
                      (not (structural-profile-contraction-p linear))
                      (eq :linear (structural-profile-name linear)))
             "linear is a first-class exact-use profile")
      (check run (equal '(:exchange :weakening :contraction)
                        (structural-profile-rules unrestricted))
             "unrestricted profile licenses exchange, weakening, and contraction"))
    (check run (null (admissible-structural-rules :ordered))
           "ordered admits no structural rules")
    ;; lattice inclusions
    (check run (logic<= :linear :affine) "linear <= affine")
    (check run (logic<= :linear :relevant) "linear <= relevant")
    (check run (logic<= :affine :unrestricted) "affine <= unrestricted")
    (check run (logic<= :relevant :unrestricted) "relevant <= unrestricted")
    (check run (logic<= :ordered :linear) "ordered <= linear")
    (check run (not (logic<= :affine :linear)) "affine NOT <= linear")
    (check run (not (logic<= :relevant :affine)) "relevant NOT <= affine")
    (check run (loop for (lo . hi) in (logic-provability-order)
                     always (logic<= lo hi))
           "every declared provability-order pair is a real lattice inclusion")

    ;; ===================================================================
    ;; Headline separations.
    ;; ===================================================================

    ;; (1) A |- A(x)A needs CONTRACTION: provable unrestricted/relevant, REJECTED
    ;;     linearly and affinely (you cannot duplicate the single resource A).
    (check run (provable-p '(a) (x* 'a 'a) :logic :unrestricted)
           "A |- A(x)A provable unrestricted (uses contraction)")
    (check run (provable-p '(a) (x* 'a 'a) :logic :relevant)
           "A |- A(x)A provable in relevant logic")
    (check run (not (provable-p '(a) (x* 'a 'a) :logic :linear))
           "A |- A(x)A REJECTED in linear logic (no contraction)")
    (check run (not (provable-p '(a) (x* 'a 'a) :logic :affine))
           "A |- A(x)A REJECTED in affine logic (no contraction)")

    ;; Additive contrast: A |- A&A needs NO structural rule (shared context),
    ;; so it IS linear -- the contrast that makes contraction visible.
    (check run (provable-p '(a) (& 'a 'a) :logic :linear)
           "A |- A&A provable LINEARLY (additive sharing, not contraction)")
    (check run (provable-p '(a) (& 'a 'a) :logic :ordered)
           "A |- A&A provable even ordered")

    ;; (2) A,B |- A needs WEAKENING: provable unrestricted/affine, REJECTED in
    ;;     linear/relevant (the resource B must be used).
    (check run (provable-p '(a b) 'a :logic :unrestricted)
           "A,B |- A provable unrestricted (uses weakening)")
    (check run (provable-p '(a b) 'a :logic :affine)
           "A,B |- A provable in affine logic")
    (check run (not (provable-p '(a b) 'a :logic :linear))
           "A,B |- A REJECTED in linear logic (no weakening)")
    (check run (not (provable-p '(a b) 'a :logic :relevant))
           "A,B |- A REJECTED in relevant logic (B unused)")

    ;; Dual: A(x)A |- A also needs weakening (drop one A after (x)L).
    (check run (provable-p (list (x* 'a 'a)) 'a :logic :affine)
           "A(x)A |- A provable in affine (weakening)")
    (check run (not (provable-p (list (x* 'a 'a)) 'a :logic :linear))
           "A(x)A |- A REJECTED linearly")

    ;; (3) A(x)B |- B(x)A needs EXCHANGE: provable in every commutative logic,
    ;;     REJECTED ordered.
    (check run (provable-p (list (x* 'a 'b)) (x* 'b 'a) :logic :linear)
           "A(x)B |- B(x)A provable linearly (uses exchange)")
    (check run (not (provable-p (list (x* 'a 'b)) (x* 'b 'a) :logic :ordered))
           "A(x)B |- B(x)A REJECTED ordered (no exchange)")

    ;; Linear modus ponens spends NOTHING -- provable in all five logics.
    (check run (loop for l in '(:ordered :linear :affine :relevant :unrestricted)
                     always (provable-p (list 'a (-o 'a 'b)) 'b :logic l))
           "A, A-oB |- B provable in every logic (no structural rule)")
    ;; Currying / -oR likewise spends nothing.
    (check run (provable-p '() (-o 'a (-o 'b (x* 'a 'b))) :logic :ordered)
           "|- A-o(B-o(A(x)B)) provable ordered")

    ;; ===================================================================
    ;; Lattice monotonicity of provability over a sample.
    ;; ===================================================================
    (let ((samples
            (list (cons (list 'a (-o 'a 'b)) 'b)            ; pure linear
                  (cons '(a) (x* 'a 'a))                    ; needs contraction
                  (cons '(a b) 'a)                          ; needs weakening
                  (cons (list (x* 'a 'b)) (x* 'b 'a))       ; needs exchange
                  (cons (list (& 'a 'b)) 'a)                ; additive proj
                  (cons '(a) (& 'a 'a))                     ; additive dup
                  (cons (list (plus* 'a 'b) (-o 'a 'c) (-o 'b 'c)) 'c)))) ; case
      (check run
             (loop for (g . goal) in samples
                   always (loop for (lo . hi) in (logic-provability-order)
                                always (or (not (provable-p g goal :logic lo))
                                           (provable-p g goal :logic hi))))
             "provability is an up-set in the structural lattice (monotone)"))

    ;; ===================================================================
    ;; struct-tags classify which logics a derivation is legal in.
    ;; ===================================================================

    ;; The unrestricted proof of A |- A(x)A spends contraction (not weakening).
    (let ((d (prove '(a) (x* 'a 'a) :logic :unrestricted)))
      (check run (deriv-p d) "A |- A(x)A has an unrestricted derivation")
      (check run (member :contraction (derivation-struct-tags d))
             "that derivation's tags include :contraction")
      (check run (not (member :weakening (derivation-struct-tags d)))
             "and do NOT include :weakening")
      (check run (and (legal-in-p d :unrestricted) (legal-in-p d :relevant))
             "legal unrestricted and relevant (which admit contraction)")
      (check run (and (not (legal-in-p d :linear)) (not (legal-in-p d :affine))
                      (not (legal-in-p d :ordered)))
             "illegal in linear/affine/ordered")
      ;; legal-in-p agrees with provable-p for this sequent's derivation
      (check run (loop for l in '(:ordered :linear :affine :relevant :unrestricted)
                       always (eq (legal-in-p d l)
                                  (provable-p '(a) (x* 'a 'a) :logic l)))
             "struct-tags legality matches provability across the lattice"))

    ;; The affine proof of A,B |- A spends weakening (not contraction).
    (let ((d (prove '(a b) 'a :logic :affine)))
      (check run (member :weakening (derivation-struct-tags d))
             "A,B |- A derivation spends :weakening")
      (check run (not (member :contraction (derivation-struct-tags d)))
             "and not :contraction"))

    ;; The linear proof of A(x)B |- B(x)A spends exchange.
    (let ((d (prove (list (x* 'a 'b)) (x* 'b 'a) :logic :linear)))
      (check run (member :exchange (derivation-struct-tags d))
             "A(x)B |- B(x)A derivation spends :exchange")
      (check run (not (legal-in-p d :ordered))
             "so it is illegal ordered"))

    ;; Modus-ponens derivation spends nothing -> legal everywhere.
    (let ((d (prove (list 'a (-o 'a 'b)) 'b :logic :linear)))
      (check run (null (derivation-struct-tags d))
             "linear modus ponens spends no structural rule")
      (check run (cut-free-p d) "and it is cut-free")
      (check run (loop for l in '(:ordered :linear :affine :relevant :unrestricted)
                       always (legal-in-p d l))
             "so it is legal in every logic"))

    ;; ===================================================================
    ;; Cut, and its admissibility (cut-free proof proves the same sequent).
    ;; ===================================================================
    (let* ((gamma (list 'a (-o 'a 'b) (-o 'b 'c)))
           (goal 'c)
           (with-cut (prove-with-cut gamma goal 'b :logic :linear))
           (cut-free (eliminate-cut with-cut :logic :linear)))
      (check run (deriv-p with-cut)
             "chain A,A-oB,B-oC |- C is provable WITH cut on lemma B")
      (check run (not (cut-free-p with-cut))
             "the cut derivation genuinely contains a :cut node")
      (check run (deriv-p cut-free)
             "cut-elimination returns a derivation (cut is admissible)")
      (check run (cut-free-p cut-free)
             "the eliminated derivation is cut-free")
      (check run (equal (derivation-end-sequent cut-free) (cons gamma goal))
             "the cut-free proof proves the SAME end-sequent")
      ;; report (and assert) proof sizes before/after -- the eliminable lemma.
      (format t "  cut-elimination: size ~D (with cut) -> ~D (cut-free)~%"
              (derivation-size with-cut) (derivation-size cut-free))
      (check run (and (plusp (derivation-size with-cut))
                      (plusp (derivation-size cut-free)))
             "both derivations have positive size")
      ;; the chain is linear all the way down
      (check run (null (derivation-struct-tags cut-free))
             "the chain spends no structural rule -- pure linear"))

    ;; Cut is admissible but cannot manufacture provability: a sequent
    ;; unprovable linearly stays unprovable even given a cut lemma whose
    ;; halves would themselves need a structural rule.
    (check run (not (prove-with-cut '(a) (x* 'a 'a) 'a :logic :linear))
           "no linear cut proof of A |- A(x)A (cut does not add power)")

    ;; ===================================================================
    ;; Formula / context size primitives.
    ;; ===================================================================
    (check run (= 0 (formula-size 'a)) "atom has 0 connectives")
    (check run (= 1 (formula-size (x* 'a 'b))) "A(x)B has 1 connective")
    (check run (= 3 (formula-size (-o (x* 'a 'b) (& 'c 'd))))
           "(A(x)B)-o(C&D) has 3 connectives")
    (check run (atomic-p 'a) "symbol is atomic")
    (check run (not (atomic-p (x* 'a 'b))) "tensor is not atomic")))
