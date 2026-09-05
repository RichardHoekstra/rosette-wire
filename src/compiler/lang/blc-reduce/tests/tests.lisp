;;;; rosette-blc-reduce/tests/tests.lisp --- Test suite.
;;;;
;;;; Correctness is ORACLE-GATED: build Church arithmetic / Boolean logic
;;;; as rosette-blc lambda terms, reduce them on the combinator machine, decode
;;;; the result, and assert it against ground-truth integers / truth values.

(defpackage #:rosette-blc-reduce/tests
  (:use #:cl #:rosette-blc #:rosette-blc-reduce #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-blc-reduce/tests)

;;; ---- Church / Boolean combinators as de-Bruijn lambda terms ----
(defun lam* (n body) (dotimes (i n body) (setf body (tlam body))))

(defun t-succ () ; \n f x. f (n f x)
  (lam* 3 (tapp (tvar 1) (tapp (tapp (tvar 2) (tvar 1)) (tvar 0)))))
(defun t-add ()  ; \m n f x. m f (n f x)
  (lam* 4 (tapp (tapp (tvar 3) (tvar 1))
                (tapp (tapp (tvar 2) (tvar 1)) (tvar 0)))))
(defun t-mul ()  ; \m n f. m (n f)
  (lam* 3 (tapp (tvar 2) (tapp (tvar 1) (tvar 0)))))
(defun t-pow ()  ; \m n. n m
  (lam* 2 (tapp (tvar 0) (tvar 1))))
(defun t-true ()  (tlam (tlam (tvar 1))))        ; \x y. x
(defun t-false () (tlam (tlam (tvar 0))))        ; \x y. y
(defun t-not ()   (lam* 3 (tapp (tapp (tvar 2) (tvar 0)) (tvar 1))))   ; \p x y. p y x
(defun t-and ()   (lam* 2 (tapp (tapp (tvar 1) (tvar 0)) (tvar 1))))   ; \p q. p q p
(defun t-or ()    (lam* 2 (tapp (tapp (tvar 1) (tvar 1)) (tvar 0))))   ; \p q. p p q

(defun ap (&rest terms) (reduce #'tapp terms))
(defun cn (n) (church-numeral n))

(defun run-all-tests ()
  "Run rosette-blc-reduce tests."
  (with-test-run (run "rosette-blc-reduce")

    ;; ===================================================================
    ;; Church numerals: identity reduction + decode.
    ;; ===================================================================
    (check run (= (reduce-church (cn 0)) 0)  "church 0 decodes to 0")
    (check run (= (reduce-church (cn 5)) 5)  "church 5 decodes to 5")
    (check run (= (reduce-church (cn 12)) 12) "church 12 decodes to 12")

    ;; ===================================================================
    ;; Church arithmetic, oracle = ground-truth integers.
    ;; ===================================================================
    (check run (= (reduce-church (ap (t-succ) (cn 4))) 5)        "SUCC 4 = 5")
    (check run (= (reduce-church (ap (t-add) (cn 2) (cn 3))) 5)  "ADD 2 3 = 5")
    (check run (= (reduce-church (ap (t-add) (cn 3) (cn 4))) 7)  "ADD 3 4 = 7")
    (check run (= (reduce-church (ap (t-mul) (cn 2) (cn 3))) 6)  "MUL 2 3 = 6")
    (check run (= (reduce-church (ap (t-mul) (cn 3) (cn 3))) 9)  "MUL 3 3 = 9")
    (check run (= (reduce-church (ap (t-pow) (cn 2) (cn 3))) 8)  "POW 2 3 = 8")
    (check run (= (reduce-church (ap (t-pow) (cn 3) (cn 2))) 9)  "POW 3 2 = 9")

    ;; ===================================================================
    ;; S / K / I combinator identities (combinators from rosette-blc).
    ;; ===================================================================
    (check run (= (reduce-church (ap (combinator-s) (combinator-k)
                                     (combinator-k) (cn 3))) 3)
           "S K K m (= I m) -> m")
    (check run (= (reduce-church (ap (combinator-k) (cn 7) (cn 2))) 7)
           "K m7 m2 -> m7")
    (check run (= (reduce-church (ap (combinator-i) (cn 6))) 6)
           "I m6 -> m6")

    ;; ===================================================================
    ;; Boolean logic, oracle = ground-truth truth values.
    ;; ===================================================================
    (check run (eq (reduce-bool (ap (t-and) (t-true)  (t-false))) nil) "AND T F = F")
    (check run (eq (reduce-bool (ap (t-and) (t-true)  (t-true)))  t)   "AND T T = T")
    (check run (eq (reduce-bool (ap (t-or)  (t-false) (t-false))) nil) "OR  F F = F")
    (check run (eq (reduce-bool (ap (t-or)  (t-false) (t-true)))  t)   "OR  F T = T")
    (check run (eq (reduce-bool (ap (t-not) (t-true)))  nil)           "NOT T = F")
    (check run (eq (reduce-bool (ap (t-not) (t-false))) t)             "NOT F = T")

    ;; ===================================================================
    ;; Sharing / idempotence: repeated reduction is stable.
    ;; ===================================================================
    (check run (= (reduce-church (ap (t-add) (cn 10) (cn 10)))
                  (reduce-church (ap (t-add) (cn 10) (cn 10))))
           "ADD 10 10 is stable across runs (= 20)")
    (check run (= (reduce-church (ap (t-add) (cn 10) (cn 10))) 20)
           "ADD 10 10 = 20")

    ;; ===================================================================
    ;; Bracket abstraction (lambda->combinators) + readback.
    ;; ===================================================================
    (check run (eq (lambda->combinators (combinator-i)) :i)
           "lambda->combinators I = :i")
    (check run (keywordp (lambda->combinators (combinator-k)))
           "lambda->combinators K is an atomic combinator")
    (check run (equal (lambda->combinators (cn 2))
                      (lambda->combinators (cn 2)))
           "bracket abstraction is deterministic")

    ;; ===================================================================
    ;; reduce-whnf: head combinator + step counter.
    ;; ===================================================================
    (check run (keywordp (reduce-whnf (cn 2)))
           "reduce-whnf returns a keyword head")
    (check run (progn (reduce-church (ap (t-mul) (cn 5) (cn 5)))
                      (> (last-step-count) 0))
           "last-step-count is positive after a non-trivial reduction")

    ;; ===================================================================
    ;; reduce-to-normal-form: terminates, deterministic, idempotent.
    ;; ===================================================================
    (check run (eq (reduce-to-normal-form (combinator-i)) :i)
           "normal form of I is :i")
    (check run (equal (reduce-to-normal-form (cn 3))
                      (reduce-to-normal-form (cn 3)))
           "normal form of church 3 is deterministic")
    (let ((nf (reduce-to-normal-form (ap (t-add) (cn 1) (cn 1)))))
      (check run (or (keywordp nf) (consp nf))
             "normal form of (ADD 1 1) is a well-formed combinator term"))
    ))
