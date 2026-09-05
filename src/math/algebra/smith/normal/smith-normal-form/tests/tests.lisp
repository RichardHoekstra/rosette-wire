;;;; tests.lisp --- tests for rosette-smith-normal-form.
;;;;
;;;; Falsification-first.  PRE-REGISTERED anti-property:
;;;;   "SNF wrong (D /= U A V, or D not diagonal, or factors not divisible)
;;;;    OR integer homology misses torsion (Klein bottle H_1 /= Z (+) Z/2)."
;;;; REFUTED iff: SNF correct + unimodular, invariant factors right, and the
;;;; Klein bottle / RP^2 Z/2 torsion is recovered with the torus as a
;;;; torsion-free control.

(defpackage #:rosette-smith-normal-form/tests
  (:use #:cl #:rosette-smith-normal-form #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-smith-normal-form/tests)

;;; ------------------------------------------------------------------
;;; helpers
;;; ------------------------------------------------------------------

(defun snf-reconstructs-p (a)
  "T iff D = U A V with U, V unimodular and D diagonal, for matrix A."
  (let* ((res (smith-normal-form a))
         (u (snf-u res)) (v (snf-v res)) (d (snf-d res)))
    (and (unimodular-p u)
         (unimodular-p v)
         (diagonal-p d)
         (equal d (mat-mul (mat-mul u a) v)))))

;;; standard CW boundary data for closed surfaces (one 0-cell):
;;;   torus       : edges a,b ; face a b a^-1 b^-1 ; d2 = [0 0]^T
;;;   Klein bottle: edges a,b ; face a b a b^-1    ; d2 = [2 0]^T
;;;   RP^2        : edge  a   ; face a a           ; d2 = [2]
;;; d1 = 0 in all three (single vertex), so C_1 -> C_0 is the zero map.

(defun run-all-tests ()
  (with-test-run (run "rosette-smith-normal-form")

    ;; =====================================================================
    ;; (a) SNF CORRECTNESS:  D = U A V, U/V unimodular, D diagonal, d_i|d_{i+1}
    ;; =====================================================================
    (let ((mats (list '((2 0) (0 3))
                      '((1 2 3) (4 5 6) (7 8 9))
                      '((6 4) (4 6))           ; circulant-ish
                      '((2 4 4) (-6 6 12) (10 -4 -16))
                      '((0 0) (0 0))           ; zero matrix
                      '((1 0 0) (0 1 0))       ; wide identity-ish
                      '((9))
                      '((-3 6) (6 -3)))))
      (dolist (a mats)
        (check run (snf-reconstructs-p a)
               (format nil "SNF reconstructs D = U A V (unimodular) for ~A" a))
        (check run (divisibility-chain-p (invariant-factors a))
               (format nil "invariant factors form a divisibility chain for ~A" a))))

    ;; unimodular det sanity (the transforms really are +/-1)
    (check run (member (mat-det '((1 2) (3 4))) '(-2 2))
           "mat-det of [[1,2],[3,4]] = -2")
    (check run (= (mat-det '((1 0) (0 1))) 1)  "det I = 1")
    (check run (= (mat-det '((2 1) (1 1))) 1)  "det [[2,1],[1,1]] = 1 (unimodular)")

    ;; =====================================================================
    ;; (b) INVARIANT FACTORS match known examples
    ;; =====================================================================
    ;; diag(2,3) -> (1, 6):  gcd=1, lcm=6
    (check run (equal (invariant-factors '((2 0) (0 3))) '(1 6))
           "SNF of diag(2,3) has invariant factors (1, 6)")
    ;; diag(4,6) -> (2, 12)
    (check run (equal (invariant-factors '((4 0) (0 6))) '(2 12))
           "SNF of diag(4,6) has invariant factors (2, 12)")
    ;; diag(6,10,15) -> (1,30,?) : gcd-all=1, then ... actually (1,1,30)? check
    ;; product = 900 = 1*1*900? no: invariant factors of diag(6,10,15):
    ;;   d1 = gcd of entries = 1; product of all = 6*10*15 = 900;
    ;;   d1 d2 = gcd of 2x2 minors = gcd(60,90,150)=30; so d2 = 30; d3=900/30=30.
    (check run (equal (invariant-factors '((6 0 0) (0 10 0) (0 0 15))) '(1 30 30))
           "SNF of diag(6,10,15) has invariant factors (1, 30, 30)")
    ;; a 3x3 circulant circ(1,2,3): det = 1^3+2^3+3^3 - 3*1*2*3 = 18, rank 3;
    ;; SNF = diag(1,1,18) -> invariant factors (1, 1, 18).
    (check run (equal (invariant-factors '((1 2 3) (3 1 2) (2 3 1))) '(1 1 18))
           "SNF of circulant circ(1,2,3) has invariant factors (1, 1, 18)")
    ;; [[2,4],[6,8]] det = -8, gcd entries = 2 -> (2,4)
    (check run (equal (invariant-factors '((2 4) (6 8))) '(2 4))
           "SNF of [[2,4],[6,8]] has invariant factors (2, 4)")

    ;; =====================================================================
    ;; (c) INTEGER HOMOLOGY WITH TORSION -- the headline
    ;; =====================================================================

    ;; ---- Klein bottle:  H_0 = Z, H_1 = Z (+) Z/2, H_2 = 0 ----
    (let* ((d1 '((0 0)))            ; d1 : C_1=Z^2 -> C_0=Z^1, zero map (1 vert)
           (d2 '((2) (0)))          ; d2 : C_2=Z^1 -> C_1=Z^2, face = 2a + 0b
           (chain-dims '(1 2 1))    ; dim C_0=1, C_1=2, C_2=1
           (h (integer-homology (list d1 d2) chain-dims))
           (h0 (nth 0 h)) (h1 (nth 1 h)) (h2 (nth 2 h)))
      (check run (and (= (hom-free-rank h0) 1) (null (hom-torsion h0)))
             "Klein bottle H_0 = Z")
      (check run (and (= (hom-free-rank h1) 1) (equal (hom-torsion h1) '(2)))
             "Klein bottle H_1 = Z (+) Z/2  -- THE TORSION the rational H misses")
      (check run (and (= (hom-free-rank h2) 0) (null (hom-torsion h2)))
             "Klein bottle H_2 = 0")
      ;; the contrast: Betti-1 = 1 only; rational/float homology stops here.
      (check run (= (hom-betti h1) 1)
             "Klein bottle beta_1 = 1 (rational homology sees ONLY this)")
      (check run (string= (format-homology-group h1) "Z (+) Z/2")
             "Klein bottle H_1 prints as Z (+) Z/2"))

    ;; ---- RP^2:  H_0 = Z, H_1 = Z/2, H_2 = 0  (PURE torsion in H_1) ----
    (let* ((d1 '((0)))              ; d1 : C_1=Z -> C_0=Z, zero map
           (d2 '((2)))              ; d2 : C_2=Z -> C_1=Z, face = 2a
           (chain-dims '(1 1 1))
           (h (integer-homology (list d1 d2) chain-dims))
           (h0 (nth 0 h)) (h1 (nth 1 h)) (h2 (nth 2 h)))
      (check run (and (= (hom-free-rank h0) 1) (null (hom-torsion h0)))
             "RP^2 H_0 = Z")
      (check run (and (= (hom-free-rank h1) 0) (equal (hom-torsion h1) '(2)))
             "RP^2 H_1 = Z/2  (PURE torsion -- beta_1 = 0 over Q)")
      (check run (and (= (hom-free-rank h2) 0) (null (hom-torsion h2)))
             "RP^2 H_2 = 0")
      (check run (string= (format-homology-group h1) "Z/2")
             "RP^2 H_1 prints as Z/2"))

    ;; ---- Torus T^2 CONTROL:  H_0 = Z, H_1 = Z^2, H_2 = Z, NO torsion ----
    (let* ((d1 '((0 0)))            ; zero map
           (d2 '((0) (0)))          ; face a b a^-1 b^-1 -> 0a + 0b
           (chain-dims '(1 2 1))
           (h (integer-homology (list d1 d2) chain-dims))
           (h0 (nth 0 h)) (h1 (nth 1 h)) (h2 (nth 2 h)))
      (check run (and (= (hom-free-rank h0) 1) (null (hom-torsion h0)))
             "torus H_0 = Z")
      (check run (and (= (hom-free-rank h1) 2) (null (hom-torsion h1)))
             "torus H_1 = Z^2  (CONTROL: torsion-free)")
      (check run (and (= (hom-free-rank h2) 1) (null (hom-torsion h2)))
             "torus H_2 = Z")
      (check run (null (hom-torsion h1))
             "torus control has NO torsion -- distinguishes it from Klein bottle"))

    ;; =====================================================================
    ;; (d) BETTI numbers (free rank) agree with rational homology
    ;;     S^1 as a triangle: 3 verts, 3 edges, 0 faces. H_1 = Z (beta_1 = 1).
    ;; =====================================================================
    (let* ((d1 '((-1 0 1) (1 -1 0) (0 1 -1)))   ; rows=verts, cols=edges
           (chain-dims '(3 3))
           (h (integer-homology (list d1) chain-dims))
           (h0 (nth 0 h)) (h1 (nth 1 h)))
      (check run (= (hom-betti h0) 1) "S^1 beta_0 = 1 (connected)")
      (check run (and (= (hom-betti h1) 1) (null (hom-torsion h1)))
             "S^1 H_1 = Z (beta_1 = 1, torsion-free) -- agrees with rational H"))))
