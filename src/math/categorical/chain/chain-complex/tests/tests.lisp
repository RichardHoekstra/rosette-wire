;;;; tests.lisp --- tests for rosette-chain-complex.
;;;;
;;;; Falsification-first.  PRE-REGISTERED anti-property:
;;;;   "d^2 != 0  /  the Betti numbers are wrong  /  no torsion  /
;;;;    the Euler characteristic fails  /  the instances don't unify."
;;;; REFUTED iff: d^2 = 0 on a triangle / tetrahedron / graph; the correct
;;;; Betti numbers for the circle (1,1), the 2-sphere (1,0,1) and the torus
;;;; (1,2,1); the Z-torsion of the Klein bottle (H_1 = Z (+) Z/2); chi =
;;;; sum (-1)^n dim C_n = sum (-1)^n b_n (Euler-Poincare, chi(S^2)=2,
;;;; chi(T^2)=0); and the Cech frustrated-triangle H^1 recovered as an
;;;; instance of this one abstract complex.

(defpackage #:rosette-chain-complex/tests
  (:use #:cl #:rosette-chain-complex #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-chain-complex/tests)

;;; ------------------------------------------------------------------
;;; standard spaces as simplicial complexes
;;; ------------------------------------------------------------------

(defun circle ()
  "S^1 as the boundary of a triangle: 3 edges, NO filling face."
  (simplicial-complex '((1 2) (2 3) (1 3))))

(defun filled-triangle ()
  "A 2-disk: the filled triangle (1 2 3)."
  (simplicial-complex '((1 2 3))))

(defun two-sphere ()
  "S^2 as the boundary of a tetrahedron: the four triangular faces, no solid."
  (simplicial-complex '((1 2 3) (1 2 4) (1 3 4) (2 3 4))))

(defun torus (&optional (m 3) (n 3))
  "T^2 as a triangulated m x n grid with periodic wraparound (m,n >= 3 so the
identifications are non-degenerate).  9 vertices, 27 edges, 18 triangles for
3x3: b_0=1, b_1=2, b_2=1, chi=0."
  (flet ((vid (i j) (+ (* (mod i m) n) (mod j n))))
    (let (faces)
      (dotimes (i m)
        (dotimes (j n)
          (let ((a (vid i j))       (b (vid (1+ i) j))
                (c (vid i (1+ j)))  (d (vid (1+ i) (1+ j))))
            (push (list a b d) faces)
            (push (list a d c) faces))))
      (simplicial-complex faces))))

;;; ------------------------------------------------------------------
;;; CW closed surfaces with ONE vertex (for the torsion contrast)
;;;   torus       : edges a,b ; face a b a^-1 b^-1 ; d2 = [0 0]^T
;;;   Klein bottle: edges a,b ; face a b a b^-1     ; d2 = [2 0]^T
;;;   RP^2        : edge  a   ; face a a            ; d2 = [2]
;;; ------------------------------------------------------------------

(defun klein-bottle ()
  (make-chain-complex (list '((0 0)) '((2) (0))) '(1 2 1)))

(defun rp2 ()
  (make-chain-complex (list '((0)) '((2))) '(1 1 1)))

(defun torus-cw ()
  (make-chain-complex (list '((0 0)) '((0) (0))) '(1 2 1)))

(defun test-matrix-product (left right)
  (when (or (null left) (null right))
    (return-from test-matrix-product nil))
  (let ((right-columns (apply #'mapcar #'list right)))
    (loop for left-row in left
          collect (loop for right-column in right-columns
                        collect (reduce #'+ (mapcar #'* left-row right-column)
                                        :initial-value 0)))))

(defun unit-coordinates (dimension index)
  (loop for column below dimension collect (if (= column index) 1 0)))

;;; ------------------------------------------------------------------

(defun run-all-tests ()
  (with-test-run (run "rosette-chain-complex")

    ;; Coordinate relative chains: an interval modulo both endpoints is S^1.
    (let* ((interval (graph-complex '(a b) '((a b))))
           (relative (make-relative-chain-complex interval '((0 1) ())))
           (quotient (relative-chain-complex-quotient relative)))
      (check run (equal (chain-complex-dims quotient) '(0 1))
             "relative interval/endpoints has one degree-one generator")
      (check run (equal (betti-numbers quotient) '(0 1))
             "H_1(I, boundary I) is one-dimensional")
      (check run (relative-chain-complex-valid-p relative)
             "relative quotient receipt replays")
      (let ((copy (relative-chain-complex-subspace-indices relative)))
        (setf (first (first copy)) 99)
        (check run (relative-chain-complex-valid-p relative)
               "relative coordinate readers are defensive")))
    (let ((bad (make-chain-complex (list '((-1) (1))) '(2 1))))
      (check run
             (handler-case
                 (progn (make-relative-chain-complex bad '((0) (0))) nil)
               (relative-chain-error (condition)
                 (and (= 1 (relative-chain-error-dimension condition))
                      (= 1 (relative-chain-error-row condition)))))
             "a coordinate subset that is not a subcomplex is refused"))

    ;; Exact rational cohomology carries representatives, not only dimensions.
    (let* ((space (make-rational-cohomology-space (torus-cw) 1))
           (representatives (rational-cohomology-space-representatives space)))
      (check run (= (rational-cohomology-space-dimension space) 2)
             "exact H^1(T^2;Q) has dimension two")
      (check run (= (length representatives) 2)
             "H^1(T^2;Q) publishes two ambient cocycle representatives")
      (loop for representative in representatives
            for index from 0 do
        (check run
               (equal (rational-cohomology-class-coordinates space representative)
                      (unit-coordinates 2 index))
               (format nil "torus H^1 representative ~D has certified unit class coordinates"
                       index)))
      (check run (rational-cohomology-space-valid-p space)
             "the torus rational-cohomology receipt replays")
      (setf (first (first representatives)) 99)
      (let ((receipt (rational-cohomology-space-receipt space)))
        (setf (getf receipt :degree) 99))
      (check run (rational-cohomology-space-valid-p space)
             "rational-cohomology representatives and receipts are defensive"))
    (let ((point-h0
            (make-rational-cohomology-space
             (simplicial-complex '((point))) 0)))
      (check run (and (= (rational-cohomology-space-dimension point-h0) 1)
                      (equal (rational-cohomology-space-representatives point-h0)
                             '((1)))
                      (rational-cohomology-space-valid-p point-h0))
             "H^0(point;Q) preserves the shaped zero incoming coboundary"))
    (let* ((circle-complex (circle))
           (circle-h1 (make-rational-cohomology-space circle-complex 1))
           (representative
             (first (rational-cohomology-space-representatives circle-h1)))
           (coboundary (first (boundary-matrix circle-complex 1)))
           (shifted (mapcar #'+ representative coboundary)))
      (check run (= (rational-cohomology-space-dimension circle-h1) 1)
             "the triangulated circle has one exact rational H^1 class")
      (check run
             (equal (rational-cohomology-class-coordinates circle-h1 shifted)
                    (rational-cohomology-class-coordinates circle-h1 representative))
             "adding a nonzero coboundary preserves exact H^1 class coordinates"))
    (let ((klein-h1 (make-rational-cohomology-space (klein-bottle) 1))
          (rp2-h1 (make-rational-cohomology-space (rp2) 1)))
      (check run (= (rational-cohomology-space-dimension klein-h1)
                    (cohomology (klein-bottle) 1)
                    1)
             "Klein-bottle rational H^1 agrees with rank-nullity despite Z/2 torsion")
      (check run (= (rational-cohomology-space-dimension rp2-h1)
                    (cohomology (rp2) 1)
                    0)
             "RP^2 rational H^1 is zero while integer H_1 retains Z/2"))

    ;; Chain maps prove relation preservation before inducing H^n pullbacks.
    (let* ((complex (torus-cw))
           (identity (identity-chain-map complex))
           (f (make-chain-map complex complex
                              '(((1)) ((2 0) (0 3)) ((1)))))
           (g (make-chain-map complex complex
                              '(((1)) ((1 1) (0 1)) ((1)))))
           (zero (make-chain-map complex complex '(nil nil nil)))
           (gf (compose-chain-maps g f))
           (identity-h1 (induced-cohomology-map identity 1))
           (f-h1 (induced-cohomology-map f 1))
           (g-h1 (induced-cohomology-map g 1))
           (zero-h1 (induced-cohomology-map zero 1))
           (gf-h1 (induced-cohomology-map gf 1)))
      (check run (and (chain-map-valid-p identity)
                      (chain-map-valid-p f)
                      (chain-map-valid-p g)
                      (chain-map-valid-p gf))
             "identity, factors, and composite replay their chain-law receipts")
      (check run (equal (induced-cohomology-map-matrix identity-h1)
                        '((1 0) (0 1)))
             "the identity chain map induces the identity on H^1")
      (check run (equal (induced-cohomology-map-matrix zero-h1)
                        '((0 0) (0 0)))
             "a NIL-represented zero chain map descends with its exact shape")
      (check run
             (equal (chain-map-matrices (compose-chain-maps identity f))
                    (chain-map-matrices f))
             "chain-map composition has the identity law")
      (check run
             (equal (induced-cohomology-map-matrix gf-h1)
                    (test-matrix-product
                     (induced-cohomology-map-matrix f-h1)
                     (induced-cohomology-map-matrix g-h1)))
             "cohomology is contravariant: (g o f)^* = f^* o g^*")
      (check run (equal (induced-cohomology-map-matrix gf-h1)
                        '((2 0) (3 3)))
             "the exact composite pullback has the expected rational matrix")
      (check run (and (induced-cohomology-map-valid-p identity-h1)
                      (induced-cohomology-map-valid-p f-h1)
                      (induced-cohomology-map-valid-p g-h1)
                      (induced-cohomology-map-valid-p zero-h1)
                      (induced-cohomology-map-valid-p gf-h1))
             "all induced cohomology descent receipts replay")
      (let ((matrices (chain-map-matrices f))
            (receipt (induced-cohomology-map-receipt f-h1)))
        (setf (first (first (second matrices))) 99
              (getf receipt :degree) 99)
        (check run (and (chain-map-valid-p f)
                        (induced-cohomology-map-valid-p f-h1))
               "chain-map and induced-map readers are defensive")))
    (let ((interval (graph-complex '(a b) '((a b)))))
      (check run
             (handler-case
                 (progn
                   (make-chain-map interval interval
                                   '(((1 0) (0 1)) ((2))))
                   nil)
               (error () t))
             "a near-miss that violates d f = f d is refused"))
    (let* ((circle-complex (circle))
           (twice
             (make-chain-map
              circle-complex circle-complex
              '(((2 0 0) (0 2 0) (0 0 2))
                ((2 0 0) (0 2 0) (0 0 2)))))
           (twice-h1 (induced-cohomology-map twice 1)))
      (check run (and (chain-map-valid-p twice)
                      (induced-cohomology-map-valid-p twice-h1)
                      (equal (induced-cohomology-map-matrix twice-h1) '((2))))
             "nonzero circle boundaries descend the doubling map to H^1 multiplication by two"))
    (let ((inexact (make-chain-complex (list '((0.0d0))) '(1 1))))
      (check run
             (handler-case
                 (progn (make-chain-map inexact inexact '(((1)) ((1)))) nil)
               (error () t))
             "the exact chain-map carrier refuses floating boundary data"))

    ;; =================================================================
    ;; (a) d^2 = 0 -- the defining property (boundary of a boundary)
    ;; =================================================================
    (check run (d-squared-zero-p (filled-triangle))
           "d^2 = 0 on a filled triangle (d_1 d_2 = 0)")
    (check run (d-squared-zero-p (simplicial-complex '((1 2 3 4))))
           "d^2 = 0 on a tetrahedron (solid 3-simplex)")
    (check run (d-squared-zero-p (two-sphere))
           "d^2 = 0 on the 2-sphere (boundary of a tetrahedron)")
    (check run (d-squared-zero-p (torus))
           "d^2 = 0 on the triangulated torus")
    (check run (d-squared-zero-p (graph-complex '(a b c) '((a b) (b c) (a c))))
           "d^2 = 0 trivially on a graph (only d_1)")
    ;; the witness is actually the zero matrix
    (check run (every (lambda (p) (every (lambda (r) (every #'zerop r)) (or (cdr p) '(()))))
                      (d-squared-residual (two-sphere)))
           "the d_1 d_2 composite is the zero matrix on S^2 (witness)")

    ;; =================================================================
    ;; (b) BETTI NUMBERS of known spaces (over a field, rank-nullity)
    ;; =================================================================
    ;; a point
    (check run (equal (betti-numbers (simplicial-complex '((1)))) '(1))
           "point: b_0 = 1")
    ;; the circle S^1: b_0 = 1, b_1 = 1
    (check run (equal (betti-numbers (circle)) '(1 1))
           "circle S^1: (b_0 b_1) = (1 1)")
    ;; the filled triangle is contractible: b_0 = 1, all higher b_n = 0
    (check run (equal (betti-numbers (filled-triangle)) '(1 0 0))
           "disk (filled triangle): (b_0 b_1 b_2) = (1 0 0) -- contractible")
    ;; the 2-sphere S^2: b_0 = 1, b_1 = 0, b_2 = 1
    (check run (equal (betti-numbers (two-sphere)) '(1 0 1))
           "2-sphere S^2: (b_0 b_1 b_2) = (1 0 1)")
    ;; the torus T^2: b_0 = 1, b_1 = 2, b_2 = 1
    (check run (equal (betti-numbers (torus)) '(1 2 1))
           "torus T^2: (b_0 b_1 b_2) = (1 2 1)")

    ;; =================================================================
    ;; (c) TORSION over Z -- the field-vs-Z difference (SNF)
    ;; =================================================================
    ;; Klein bottle: H_1 = Z (+) Z/2 -- b_1 = 1 over a field, torsion Z/2 over Z
    (check run (equal (betti-numbers (klein-bottle)) '(1 1 0))
           "Klein bottle Betti (over a field): (1 1 0) -- the field MISSES torsion")
    (check run (equal (torsion-coefficients (klein-bottle) 1) '(2))
           "Klein bottle H_1 torsion = Z/2 -- the SNF torsion the field can't see")
    (check run (string= (rosette-smith-normal-form:format-homology-group
                         (nth 1 (homology (klein-bottle)))) "Z (+) Z/2")
           "Klein bottle H_1 = Z (+) Z/2")
    ;; RP^2: H_1 = Z/2 (pure torsion, b_1 = 0 over a field)
    (check run (= (betti (rp2) 1) 0)
           "RP^2: b_1 = 0 over a field (pure torsion is invisible there)")
    (check run (equal (torsion-coefficients (rp2) 1) '(2))
           "RP^2 H_1 = Z/2 -- PURE torsion")
    ;; the torus is the torsion-free control
    (check run (null (torsion-coefficients (torus-cw) 1))
           "torus H_1 is torsion-free (the control)")

    ;; =================================================================
    ;; (d) EULER-POINCARE -- chi = sum(-1)^n dim C_n = sum(-1)^n b_n
    ;; =================================================================
    (check run (= (euler-characteristic (two-sphere)) 2)
           "chi(S^2) = V - E + F = 2")
    (check run (= (euler-characteristic (torus)) 0)
           "chi(T^2) = 0")
    (check run (= (euler-characteristic (circle)) 0)
           "chi(S^1) = 0")
    (check run (= (euler-characteristic (filled-triangle)) 1)
           "chi(disk) = 1")
    ;; Euler-Poincare: the two ways of computing chi agree
    (dolist (sp (list (circle) (filled-triangle) (two-sphere) (torus)
                      (klein-bottle) (rp2)))
      (check run (= (euler-characteristic sp) (euler-from-betti sp))
             (format nil "Euler-Poincare: sum(-1)^n dim C_n = sum(-1)^n b_n = ~D"
                     (euler-characteristic sp))))

    ;; =================================================================
    ;; (e) THE INSTANCES UNIFY -- Cech H^1 is THIS complex's first cohomology
    ;; =================================================================
    ;; A "frustrated triangle": three vertices, three edges, NO filling
    ;; face.  This is the open 1-cycle whose Cech 1-cocycle cannot be a
    ;; coboundary -- rosette-cech-cochain reports dim H^1 = 1 (the obstruction).
    ;; As an instance of this abstract complex, H^1 = b_1 = 1.
    (let ((frustrated (cech-1-cochain-complex '(1 2 3)
                                              '((1 2) (2 3) (1 3))
                                              '())))
      (check run (= (cohomology frustrated 1) 1)
             "Cech frustrated-triangle H^1 = 1 (the obstruction) -- as an instance")
      (check run (= (betti frustrated 1) 1)
             "...and H^1 = b_1 = H_1 over a field (the instances UNIFY)")
      ;; fill the triangle: the obstruction vanishes (H^1 = 0)
      (let ((filled (cech-1-cochain-complex '(1 2 3)
                                            '((1 2) (2 3) (1 3))
                                            '((1 2 3)))))
        (check run (= (cohomology filled 1) 0)
               "filling the triangle kills H^1 (consistent Cech datum)")))

    ;; the graph homology is the same structure: a triangle graph (cycle)
    ;; has b_0 = 1, b_1 = 1 -- federation-homology's b_0/b_1.
    (let ((g (graph-complex '(a b c) '((a b) (b c) (a c)))))
      (check run (equal (betti-numbers g) '(1 1))
             "graph triangle (cycle): (b_0 b_1) = (1 1) -- federation-homology's instance"))
    ;; two disjoint edges: b_0 = 2 components, b_1 = 0
    (let ((g (graph-complex '(a b c d) '((a b) (c d)))))
      (check run (equal (betti-numbers g) '(2 0))
             "two disjoint edges: b_0 = 2 components, b_1 = 0"))

    ;; the dual cochain complex satisfies delta^2 = 0
    (check run (d-squared-zero-p (cochain-complex (two-sphere)))
           "the dual cochain complex satisfies delta^2 = 0 (S^2)")

    ;; =================================================================
    ;; (f) THE CUP PRODUCT -- cohomology as a graded RING (GF(2))
    ;;
    ;; Anti-property: "cohomology is only an additive invariant (Betti);
    ;; the cup product either breaks the ring axioms or fails to see more
    ;; than Betti numbers do."  REFUTED iff: the cup is bilinear,
    ;; associative and graded-commutative on the H^1 generators; AND on the
    ;; torus the two H^1 generators cup to the H^2 top class (nonzero) while
    ;; on the wedge S^1 v S^1 v S^2 -- with the SAME Betti numbers (1,2,1) --
    ;; every product of 1-classes VANISHES.  The ring beats the Betti numbers.
    ;; =================================================================
    (run-cup-tests run)))

;;; ------------------------------------------------------------------
;;; the cup-product witnesses (Part (f), factored out for clarity)
;;; ------------------------------------------------------------------

(defun torus-sc ()
  "T^2 as the same 3x3 toroidal grid, as a SIMPLICIAL-COCHAINS object."
  (flet ((vid (i j) (+ (* (mod i 3) 3) (mod j 3))))
    (let (faces)
      (dotimes (i 3)
        (dotimes (j 3)
          (let ((a (vid i j))       (b (vid (1+ i) j))
                (c (vid i (1+ j)))  (d (vid (1+ i) (1+ j))))
            (push (list a b d) faces)
            (push (list a d c) faces))))
      (simplicial-cochains faces))))

(defun wedge-sc ()
  "S^1 v S^1 v S^2 wedged at vertex 0: two hollow triangles (the loops) and
the hollow boundary of a tetrahedron (the sphere) sharing ONLY vertex 0.
Betti = (1 2 1) -- identical to the torus."
  (simplicial-cochains
   (list '(0 1) '(1 2) '(0 2)            ; S^1 #1 (hollow)
         '(0 3) '(3 4) '(0 4)            ; S^1 #2 (hollow)
         '(0 5 6) '(0 5 7) '(0 6 7) '(5 6 7)))) ; S^2 = bdry tetra{0,5,6,7}

(defun run-cup-tests (run)
  (let* ((torus (torus-sc))
         (wedge (wedge-sc)))

    ;; --- the two spaces have the SAME Betti numbers (GF(2) and over Q) ----
    (check run (equal (list (gf2-betti torus 0) (gf2-betti torus 1) (gf2-betti torus 2))
                      '(1 2 1))
           "T^2 GF(2) cohomology dims (dim H^0 H^1 H^2) = (1 2 1)")
    (check run (equal (list (gf2-betti wedge 0) (gf2-betti wedge 1) (gf2-betti wedge 2))
                      '(1 2 1))
           "wedge GF(2) cohomology dims = (1 2 1) -- SAME as the torus")
    (check run (equal (cohomology-ring-dimensions (cohomology-ring torus))
                      (cohomology-ring-dimensions (cohomology-ring wedge)))
           "T^2 and wedge have IDENTICAL graded dimensions (Betti cannot separate)")

    ;; --- RING AXIOMS on the torus H^1 generators a, b (GF(2)) -------------
    (let* ((gens (cohomology-generators torus 1))
           (a (first gens)) (b (second gens))
           (one (make-list (sc-n-simplices torus 0) :initial-element 1)))
      (check run (= (length gens) 2)
             "T^2 has exactly two H^1 generators a, b")
      ;; bilinearity:  (a + b) cup b = a cup b + b cup b   (XOR over GF(2))
      (check run (equal (cochain-cup torus (mapcar #'logxor a b) 1 b 1)
                        (mapcar #'logxor
                                (cochain-cup torus a 1 b 1)
                                (cochain-cup torus b 1 b 1)))
             "cup is BILINEAR: (a+b) cup b = a cup b + b cup b")
      ;; bilinearity in the other slot
      (check run (equal (cochain-cup torus a 1 (mapcar #'logxor a b) 1)
                        (mapcar #'logxor
                                (cochain-cup torus a 1 a 1)
                                (cochain-cup torus a 1 b 1)))
             "cup is BILINEAR: a cup (a+b) = a cup a + a cup b")
      ;; associativity at the cochain level: (1 cup a) cup b = 1 cup (a cup b)
      (check run (equal (cochain-cup torus (cochain-cup torus one 0 a 1) 1 b 1)
                        (cochain-cup torus one 0 (cochain-cup torus a 1 b 1) 2))
             "cup is ASSOCIATIVE: (1 cup a) cup b = 1 cup (a cup b)")
      ;; graded-commutativity: over GF(2) the sign (-1)^{1.1} is +1, so
      ;; [a cup b] = [b cup a] as classes (their XOR is a coboundary).
      (check run (cohomology-cohomologous-p
                  torus (cochain-cup torus a 1 b 1) (cochain-cup torus b 1 a 1) 2)
             "cup is GRADED-COMMUTATIVE: [a cup b] = (-1)^{1.1}[b cup a] (GF(2): same class)")
      ;; the unit: 1 cup a = a  (1 = the all-ones 0-cochain, a cocycle for H^0)
      (check run (equal (cochain-cup torus one 0 a 1) a)
             "the H^0 unit acts as identity: 1 cup a = a"))

    ;; --- THE HEADLINE: cup distinguishes T^2 from the wedge ---------------
    (let* ((tr (cohomology-ring torus))
           (wr (cohomology-ring wedge))
           (t-form (cohomology-ring-h1-cup-form tr))
           (w-form (cohomology-ring-h1-cup-form wr)))
      ;; on the torus SOME product of 1-classes is the nonzero H^2 top class
      (check run (cup-form-nonzero-p t-form)
             "TORUS: some H^1 x H^1 cup is the NONZERO H^2 top class")
      ;; concretely a cup b is nonzero (the symplectic pairing reads 1)
      (let* ((g (cohomology-generators torus 1))
             (a (first g)) (b (second g)))
        (check run (or (nth-value 1 (cohomology-cup torus a 1 b 1))
                       (nth-value 1 (cohomology-cup torus b 1 a 1)))
               "TORUS: a cup b (or b cup a) is a NONZERO class in H^2"))
      ;; on the wedge EVERY product of 1-classes vanishes
      (check run (not (cup-form-nonzero-p w-form))
             "WEDGE: EVERY H^1 x H^1 cup VANISHES in H^2")
      ;; the discrimination, stated as one falsifiable claim
      (check run (and (cup-form-nonzero-p t-form)
                      (not (cup-form-nonzero-p w-form))
                      (equal (cohomology-ring-dimensions tr)
                             (cohomology-ring-dimensions wr)))
             "the CUP PRODUCT distinguishes T^2 from the wedge -- which Betti numbers CANNOT"))))
