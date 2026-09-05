;;;; tests.lisp --- tests for rosette-cubical-core.

(defpackage #:rosette-cubical-core/tests
  (:use #:cl #:rosette-cubical-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-cubical-core/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-cubical-core")
    ;; Interval: De Morgan algebra, endpoint reduction.
    (check run (eq (eval-interval (ineg :0) nil) :1) "neg(0)=1")
    (check run (eq (eval-interval (ineg :1) nil) :0) "neg(1)=0")
    (check run (eq (eval-interval (imeet (idim 'x) :0) nil) :0)
           "meet: 0 absorbs")
    (check run (eq (eval-interval (ijoin (idim 'x) :1) nil) :1)
           "join: 1 absorbs")
    (check run (eq (eval-interval (idim 'x) '((x . :1))) :1)
           "dimension variable reduces under env")
    (check run (interval-term-p (imeet (idim 'x) (ineg (idim 'y))))
           "compound interval term well-formed")

    ;; Paths as functions out of I: endpoints definitional.
    (let* ((b (bool-type))
           (p (make-cpath b (lambda (i) (ecase i (:0 t) (:1 nil))))))
      (check run (eq (path-app p :0) t) "path @ i0 reduces to 0-endpoint")
      (check run (eq (path-app p :1) nil) "path @ i1 reduces to 1-endpoint")
      (check run (eq (cpath-i0 (crefl b t)) (cpath-i1 (crefl b t)))
             "crefl is a constant line"))

    ;; Kan transp regularity.
    (check run (transp-const-id-p (bool-type) t)
           "transp over constant line is identity (regularity beta)")
    (check run (transp-const-id-p (int-type) 7 :test #'=)
           "transp over constant Int line is identity")

    ;; Computational univalence -- the headline beta-rules.
    (let ((noteq (bool-not-equivalence))
          (succ (int-succ-equivalence)))
      (check run (eq (ua-beta noteq t) nil)
             "transp (ua not) true = false  [ua-beta on Bool]")
      (check run (eq (ua-beta noteq nil) t)
             "transp (ua not) false = true  [ua-beta on Bool]")
      (check run (= (ua-beta succ 3) 4)
             "transp (ua succ) 3 = 4        [ua-beta on Z]")
      (check run (= (ua-beta succ -1) 0)
             "transp (ua succ) -1 = 0       [ua-beta on Z]")
      (check run (= (ua-beta (rosette-hott-core:inverse-equivalence succ) 3) 2)
             "transp (ua pred) 3 = 2        [ua-beta inverse]")
      (let ((line (ua-line succ)))
        (check run (eq (type-line-at line :0)
                       (rosette-hott-core:hott-equivalence-source succ))
               "(ua succ) i0 = source")
        (check run (eq (type-line-at line :1)
                       (rosette-hott-core:hott-equivalence-target succ))
               "(ua succ) i1 = target")))

    ;; Located wall: book-HoTT transport is stuck on the same expression.
    (let ((succ (int-succ-equivalence))
          (noteq (bool-not-equivalence)))
      (check run (book-transport-stuck-p succ 3)
             "book-HoTT family transport STUCK across (ua succ)")
      (check run (book-transport-stuck-p noteq t)
             "book-HoTT family transport STUCK across (ua not)"))

    ;; Toward pi_1(S^1) = Z.
    (check run (= (winding-of-loop-power 0) 0) "winding(loop^0) = 0")
    (check run (= (winding-of-loop-power 5) 5) "winding(loop^5) = 5")
    (check run (= (winding-of-loop-power -3) -3) "winding(loop^-3) = -3")
    (check run (circle-encode-decode-roundtrip-p 4)
           "encode/decode roundtrip n=4")

    ;; hcomp.
    (let ((z (int-type)))
      (check run (= (hcomp z 10 '()) 10) "hcomp degenerate = cap")
      (check run (= (hcomp z 0 (list #'1+ #'1+)) 2)
             "hcomp composes two side faces"))

    ;; ---- THE 2-CELL LAYER: squares (the cube, first increment) --------
    (let ((z (int-type)) (b (bool-type)))

      ;; make-csquare: a 2-cell is REPRESENTABLE (book-HoTT could not).
      (let ((sq (make-csquare z (lambda (r s) (declare (ignore r s)) 7))))
        (check run (csquare-p sq) "make-csquare builds a representable 2-cell")
        (check run (= (square-app sq :0 :1) 7) "square endpoints reduce definitionally")
        (check run (square-boundary-coherent-p sq) "square boundary faces are coherent"))

      ;; refl-square + face extraction: all four faces of a constant square refl.
      (let ((sq (refl-square z 3)))
        (check run (= (cpath-i0 (csquare-face-r0 sq)) 3) "refl-square r0 face = refl")
        (check run (= (cpath-i1 (csquare-face-s1 sq)) 3) "refl-square s1 face = refl")
        (check run (square-boundary-coherent-p sq) "refl-square coherent"))

      ;; hrefl / vrefl: a path degenerated into a square in each direction.
      (let* ((p (make-cpath b (lambda (i) (ecase i (:0 t) (:1 nil)))))
             (h (hrefl-square p)) (v (vrefl-square p)))
        (check run (eq (cpath-i0 (csquare-face-s0 h)) t)  "hrefl s0 face follows the path (i0)")
        (check run (eq (cpath-i1 (csquare-face-s0 h)) nil) "hrefl s0 face follows the path (i1)")
        (check run (eq (cpath-i1 (csquare-face-r1 v)) nil) "vrefl carries the path vertically"))

      ;; THE TORUS FILLING SQUARE: the commutator 2-cell, now representable.
      (let* ((base 0)
             (a (crefl z base)) (bb (crefl z base))   ; loops at the basepoint
             (sq (commuting-square z a bb)))
        (check run (csquare-p sq)
               "torus commuting square a b a^-1 b^-1 is REPRESENTABLE")
        (check run (square-boundary-coherent-p sq)
               "torus 2-cell boundary closes (the relator bounds a filled square)")
        (check run (= (cpath-i0 (csquare-face-s0 sq)) base)
               "torus square bottom face = loop a"))

      ;; ASSOCIATOR as a genuine 2-path (not merely endpoint equality).
      (let* ((p (make-cpath z (lambda (i) (ecase i (:0 0) (:1 1)))))
             (q (make-cpath z (lambda (i) (ecase i (:0 1) (:1 2)))))
             (s (make-cpath z (lambda (i) (ecase i (:0 2) (:1 3)))))
             (assoc (associator-square p q s)))
        (check run (csquare-p assoc) "associator (P.Q).S = P.(Q.S) is a representable 2-cell")
        (check run (= (cpath-i0 (csquare-face-r0 assoc)) 0) "associator source endpoint = 0")
        (check run (= (cpath-i1 (csquare-face-r0 assoc)) 3) "associator target endpoint = 3"))

      ;; hfill: the comp's intermediate stages as a representable square,
      ;; whose lid is definitionally the hcomp result.
      (let ((cap 0) (tube (list #'1+ #'1+ #'1+)))
        (check run (= (hfill-lid z cap tube) (hcomp z cap tube))
               "hfill lid = hcomp result (the filler fills the box)")
        (let ((fsq (hfill z cap tube)))
          (check run (= (square-app fsq :1 :0) cap) "hfill s0 face = cap")
          (check run (= (square-app fsq :1 :1) 3) "hfill s1 face = lid (0 ->1->2->3)"))))

    ;; ---- PathP + comp (the cube, increment 2) -------------------------
    (let ((z (int-type)) (succ (int-succ-equivalence)) (boolbit (bool-bit-equivalence)))

      ;; cpath -> PathP over a constant line (the homogeneous special case).
      (let* ((p (make-cpath z (lambda (i) (ecase i (:0 5) (:1 9)))))
             (pp (cpath->pathp p)))
        (check run (cpathp-p pp) "a cpath is a PathP over a constant line")
        (check run (= (cpathp-i0 pp) 5) "PathP-over-const 0-endpoint reduces")
        (check run (= (cpathp-i1 pp) 9) "PathP-over-const 1-endpoint reduces")
        (check run (not (pathp-heterogeneous-p pp))
               "constant-line PathP is NOT heterogeneous"))

      ;; transp-fill: transport realised as a PathP over the line.
      (let ((pf (transp-fill (ua-line succ) 0)))
        (check run (cpathp-p pf) "transp-fill is a PathP over the ua line")
        (check run (= (cpathp-i0 pf) 0) "transp-fill starts at the cap (0)")
        (check run (= (cpathp-i1 pf) 1) "transp-fill ends at (transp = ua-beta) = 1")
        (check run (/= (cpathp-i0 pf) (cpathp-i1 pf))
               "transp-fill over ua succ moves the cap (0 -> 1): a nontrivial path over the line")
        (check run (not (pathp-heterogeneous-p pf))
               "ua succ : Z = Z fibres share a descriptor -- NOT heterogeneous (ordinary Z-path)"))

      ;; GENUINELY distinct fibres: PathP over ua (Bool ~= Bit).
      (let ((pf (transp-fill (ua-line boolbit) t)))
        (check run (eq (cpathp-i0 pf) t) "Bool~=Bit PathP 0-endpoint : Bool = T")
        (check run (eq (cpathp-i1 pf) :one) "Bool~=Bit PathP 1-endpoint : Bit = :one")
        (check run (pathp-heterogeneous-p pf)
               "Bool~=Bit PathP is heterogeneous across DISTINCT fibres"))

      ;; comp degeneracy laws: comp generalises BOTH transp and hcomp.
      (check run (= (comp (ua-line succ) 3 '()) (transp (ua-line succ) 3))
             "comp with EMPTY tube = transp (degeneracy 1)")
      (check run (= (comp (ua-line succ) 3 '()) 4) "  ... and it computes (3 -> 4)")
      (let ((tube (list #'1+ #'1+)))
        (check run (= (comp (make-const-line z) 0 tube) (hcomp z 0 tube))
               "comp over a CONSTANT line = hcomp (degeneracy 2)")
        ;; fusion: comp along ua succ WITH a tube applies succ to the composed cap.
        (check run (= (comp (ua-line succ) 0 tube) 3)
               "comp fuses hcomp+transp: ua succ over hcomp(0,[+1,+1]=2) = 3")))

    ;; ---- dependent circle-induction + pi_1(S^1) = Z (increment 3) -----
    (let* ((fam (helix-family))
           (section (circle-ind fam 0)))
      ;; point beta: the section at base is the base value.
      (check run (= (circle-section-at-base section) 0)
             "circle-ind point-beta: section @ base = base value (0)")
      ;; loop beta: the loop case is a PathP over the family (book-HoTT lacked this).
      (check run (cpathp-p (circle-section-loop-pathp section))
             "circle-ind loop case is a genuine PathP over the helix family")
      (check run (= (cpathp-i0 (circle-section-loop-pathp section)) 0)
             "loop-PathP starts at the base value (0)")
      (check run (= (circle-section-loop-transport section) 1)
             "circle-ind loop-beta: transport along loop = ua succ applied (0 -> 1)")
      ;; encode / decode and the equivalence pi_1(S^1) = Z.
      (check run (= (circle-encode 7) 7) "encode(loop^7) = 7 (by iterated ua-beta)")
      (check run (= (circle-encode -4) -4) "encode(loop^-4) = -4 (ua pred)")
      (check run (circle-pi1-encode-decode-id-p 5) "encode . decode = id on Z (n=5)")
      (check run (circle-pi1-encode-decode-id-p -9) "encode . decode = id on Z (n=-9)")
      (check run (circle-pi1-decode-encode-id-p 6)
             "decode . encode = id on loop^n (n=6, the computational shadow)")
      (check run (every #'circle-pi1-encode-decode-id-p '(-3 -1 0 1 2 8))
             "pi_1(S^1) = Z: encode/decode roundtrip across a range"))

    ;; ---- pi_n, n-truncation, Eckmann-Hilton (increment 4) -------------
    ;; The circle is aspherical (K(Z,1)): pi_1 = Z, pi_{>=2} = 0.
    (check run (eq (pi-n :circle 1) :Z) "pi_1(S^1) = Z")
    (check run (eq (pi-n :circle 2) :trivial) "pi_2(S^1) = 0")
    (check run (eq (pi-n :circle 7) :trivial) "pi_7(S^1) = 0")
    (check run (circle-aspherical-p) "the circle is aspherical (K(Z,1))")
    ;; Sets have no higher homotopy.
    (check run (set-higher-homotopy-trivial-p 1) "pi_1(set) = 0")
    (check run (set-higher-homotopy-trivial-p 4) "pi_4(set) = 0")
    ;; Every pi_n is set-level (0-truncated by construction).
    (check run (group-class-is-set-p (pi-n :circle 1)) "pi_1 is a set (0-truncated)")
    ;; THE WALL (post inc10): only STRICTLY ABOVE the diagonal stalls.
    (check run (eq (pi-n (list :sphere 3) 4) :stalled) "pi_4(S^3) STALLS (Brunerie number, above diagonal)")
    (check run (not (sphere-pi-n-stalls-p 2 2)) "pi_2(S^2) NO LONGER stalls (diagonal: now Z)")
    (check run (sphere-pi-n-stalls-p 3 4) "pi_4(S^3) still stalls (4 > 3, above diagonal)")
    (check run (not (sphere-pi-n-stalls-p 2 3)) "pi_3(S^2) NO LONGER stalls (inc11 Hopf: now Z)")
    ;; Eckmann-Hilton: pi_n abelian for n >= 2 (interchange forces it).
    (check run (interchange-holds-p #'+ #'+ '(0 1 2 -1) :test #'=)
           "interchange holds for an abelian composition (+)")
    (check run (pi-n-abelian-for-n>=2-p)
           "Eckmann-Hilton: pi_n abelian for n>=2 (and S_3 FAILS interchange)")

    ;; ---- the face lattice + comp over a cofibration (increment 5) -----
    (let ((z (int-type)) (succ (int-succ-equivalence)))
      ;; the De Morgan cofibration algebra evaluates correctly.
      (check run (eval-face (f-eq0 'i) '((i . :0))) "(i=0) holds when i=0")
      (check run (not (eval-face (f-eq0 'i) '((i . :1)))) "(i=0) fails when i=1")
      (check run (eval-face (f-or (f-eq0 'i) (f-eq1 'i)) '((i . :1)))
             "(i=0)\\/(i=1) holds at i=1")
      (check run (not (eval-face (f-and (f-eq0 'i) (f-eq1 'i)) '((i . :0))))
             "(i=0)/\\(i=1) is contradictory (= 0F)")
      (check run (face-contradictory-p 'i) "opposite-endpoint meet is bottom")
      (check run (eval-face (f-top) '()) "1F always holds")
      (check run (not (eval-face (f-bot) '())) "0F never holds")

      ;; systems of partial elements + compatibility.
      (let* ((envs '(((i . :0)) ((i . :1))))
             (good (make-system (list (cons (f-eq0 'i) 5) (cons (f-eq1 'i) 9))))
             (overlap-ok (make-system (list (cons (f-top) 7) (cons (f-eq1 'i) 7))))
             (bad (make-system (list (cons (f-top) 7) (cons (f-eq1 'i) 8)))))
        (check run (system-compatible-p good envs) "disjoint-face system is compatible")
        (check run (= (system-value good '((i . :1))) 9) "system reads the holding clause")
        (check run (system-compatible-p overlap-ok envs)
               "overlapping faces with equal values are compatible")
        (check run (not (system-compatible-p bad envs))
               "overlapping faces with DIFFERENT values are rejected"))

      ;; comp over a cofibration: degeneracy, adjacency, and reading the lid.
      (check run (= (comp-sys (ua-line succ) 3 (make-system '()) :dim 'i :test #'=)
                    (transp (ua-line succ) 3))
             "comp-sys with EMPTY system = transp (degeneracy)")
      (check run (= (comp-sys (ua-line succ) 3 (make-system '()) :dim 'i :test #'=) 4)
             "  ... and it computes (3 -> 4)")
      ;; a system that fixes the i1 lid explicitly (adjacency at i0 satisfied).
      (let ((sys (make-system (list (cons (f-eq0 'i) 3) (cons (f-eq1 'i) 42)))))
        (check run (= (comp-sys (ua-line succ) 3 sys :dim 'i :test #'=) 42)
               "comp-sys reads the system's lid where (i=1) holds")))

    ;; ---- connection squares (increment 6) -----------------------------
    (let* ((z (int-type))
           (p (make-cpath z (lambda (i) (ecase i (:0 4) (:1 9))))))  ; p : 4 = 9
      (check run (csquare-p (connection-and-square p))
             "AND-connection lambda i j. p(i/\\j) is a representable square")
      (check run (csquare-p (connection-or-square p))
             "OR-connection lambda i j. p(i\\/j) is a representable square")
      (check run (connection-and-left-unit-p p :test #'=)
             "AND-connection witnesses left-unit refl . p ~ p as a 2-cell")
      (check run (connection-or-right-unit-p p :test #'=)
             "OR-connection witnesses right-unit p . refl ~ p as a 2-cell")
      ;; spot-check the defining reductions p(i/\j), p(i\/j) at the corners.
      (check run (= (square-app (connection-and-square p) :1 :1) 9)
             "p(1/\\1) = p(1) = 9")
      (check run (= (square-app (connection-and-square p) :0 :1) 4)
             "p(0/\\1) = p(0) = 4 (the refl side)")
      (check run (= (square-app (connection-or-square p) :0 :0) 4)
             "p(0\\/0) = p(0) = 4")
      (check run (= (square-app (connection-or-square p) :0 :1) 9)
             "p(0\\/1) = p(1) = 9 (the refl side)"))

    ;; ---- Kan comp for the type-formers: Sigma / Pi / Path (increment 7)
    (let ((z (int-type)) (succ (int-succ-equivalence)))

      ;; SIGMA: comp is componentwise.  First component along ua succ, second
      ;; (independent) also +1.  (3 . 5) transports to (4 . 6).
      (let ((sl (make-sigma-line (ua-line succ)
                                 (lambda (e a) (declare (ignore e a)) z)
                                 (lambda (a0 b0) (declare (ignore a0)) (1+ b0)))))
        (let ((r (transp sl (cons 3 5))))
          (check run (and (= (car r) 4) (= (cdr r) 6))
                 "comp Sigma is componentwise: (3 . 5) -> (4 . 6)")))
      ;; SIGMA with a DEPENDENT second component: b-transp uses a0.
      (let ((sl (make-sigma-line (ua-line succ)
                                 (lambda (e a) (declare (ignore e a)) z)
                                 (lambda (a0 b0) (+ b0 a0)))))
        (let ((r (transp sl (cons 3 5))))
          (check run (and (= (car r) 4) (= (cdr r) 8))
                 "comp Sigma, dependent 2nd component: (3 . 5) -> (4 . 5+3=8)")))

      ;; PI: comp is contravariant in the domain.  Back-transport = pred,
      ;; codomain transport = +1.  transp(identity) = identity.
      (let* ((pl (make-pi-line #'1- (lambda (a0 v) (declare (ignore a0)) (1+ v))))
             (tid (transp pl #'identity)))
        (check run (= (funcall tid 7) 7)
               "comp Pi: transp(identity) = identity (7 -> 7)")
        ;; transp(double): a1 |-> 1 + 2*(a1 - 1) = 2 a1 - 1.
        (let ((tdbl (transp pl (lambda (x) (* 2 x)))))
          (check run (= (funcall tdbl 5) 9) "comp Pi: transp(2x) at 5 = 2*5-1 = 9")
          (check run (= (funcall tdbl 0) -1) "comp Pi: transp(2x) at 0 = -1")))

      ;; PATH: comp moves a path pointwise along the inner line.
      (let* ((p (make-cpath z (lambda (i) (ecase i (:0 0) (:1 1)))))   ; p : 0 = 1
             (plift (make-path-line (ua-line succ)))
             (tp (transp plift p)))
        (check run (= (cpath-i0 tp) 1) "comp Path: endpoint 0 transports to 1")
        (check run (= (cpath-i1 tp) 2) "comp Path: endpoint 1 transports to 2"))
      ;; PATH over a CONSTANT inner line: transp is (pointwise) the identity path.
      (let* ((p (make-cpath z (lambda (i) (ecase i (:0 3) (:1 8)))))
             (plift (make-path-line (make-const-line z)))
             (tp (transp plift p)))
        (check run (and (= (cpath-i0 tp) 3) (= (cpath-i1 tp) 8))
               "comp Path over a constant line preserves the path (3 = 8)")))

    ;; ---- S^2 / higher hcomp (increment 9) ----
    (let ((z (int-type)))
      ;; Higher (2-dimensional) hcomp: a tube of square faces folded onto a cap.
      (let ((cap (refl-square z 0)))
        ;; degenerate tube = cap (the 2-dim analogue of hcomp's `tube = cap`).
        (check run (hcomp2-degenerate-is-cap-p z cap :test #'=)
               "hcomp2 with EMPTY tube = cap square (higher degeneracy)")
        (check run (eq (hcomp2 z cap '()) cap)
               "hcomp2 empty-tube returns the cap object unchanged")
        ;; a non-trivial tube: each face replaces the square with a +1-shifted one.
        (let* ((bump (lambda (sq)
                       (let ((v (1+ (square-app sq :0 :0))))
                         (refl-square z v))))
               (lid (hcomp2 z cap (list bump bump))))
          (check run (csquare-p lid) "hcomp2 lid is a representable square")
          (check run (= (square-app lid :1 :1) 2)
                 "hcomp2 folds the tube onto the cap (0 ->1 ->2)")
          (check run (hcomp2-lid-boundary-coherent-p z cap (list bump bump))
                 "hcomp2 lid boundary closes (square-boundary-coherent-p)"))))

    ;; S^2 as a genuine HIT: base + surf (a 2-cell whose boundary is refl base).
    (let ((s2 (make-sphere2)))
      (check run (sphere2-p s2) "S^2 is built as a HIT (base + surf)")
      (check run (eq (sphere2-base s2) :base) "S^2 has the single point base")
      (check run (csquare-p (sphere2-surf s2)) "S^2's surf is a representable 2-cell (a square)")
      (check run (sphere2-surf-boundary-refl-p s2)
             "surf's ENTIRE boundary is refl base: it is a 2-cell refl = refl, not Sigma S^1 of points")

      ;; The S^2 recursor: point-beta and surf -> a target 2-cell.
      (let* ((z (int-type))
             (surf-img (refl-square z 5))          ; a 2-cell refl 5 = refl 5 in Z
             (rec (sphere2-rec s2 z 5 surf-img)))
        (check run (sphere2-rec-p rec) "sphere2-rec builds an eliminator into the target")
        (check run (= (sphere2-rec-app-base rec) 5)
               "S^2 recursor point-beta: rec @ base = base-image (5)")
        (check run (sphere2-rec-point-beta-p rec 5 :test #'=) "point-beta holds (rec @ base = 5)")
        (check run (csquare-p (sphere2-rec-app-surf rec)) "rec sends surf to a 2-cell (a square)")
        (check run (sphere2-rec-surf-is-2cell-p rec)
               "rec's surf image is a genuine 2-cell (boundary closes)"))

      ;; Omega^2(S^2): the surf generator, and the honest partial facts.
      (check run (csquare-p (sphere2-omega2-generator s2))
             "the surf generator is a representable element of Omega^2(S^2)")
      (check run (sphere2-surf-nontrivial-p s2)
             "surf is carried as a non-degenerate 2-cell generator (structural, by construction -- NOT a computed proof surf =/= refl)")
      (check run (sphere2-not-a-set-p s2)
             "S^2 carries a generating 2-cell BY CONSTRUCTION (not a set by construction -- NOT a computed truncation proof)")
      (check run (sphere2-pi2-has-nontrivial-element-p s2)
             "HONEST PARTIAL: pi_2(S^2) has a non-trivial element (the surf generator)")

      ;; THE WALL, RECEDED by inc10: pi_2(S^2)=Z is now derived; only the
      ;; off-diagonal stems stay stalled.
      (check run (sphere2-pi2-is-Z-p)
             "pi_2(S^2) = Z is now DERIVED (Freudenthal diagonal, no longer stalled)")
      (check run (sphere2-pi3-hopf-is-Z-p)
             "pi_3(S^2) = Z (Hopf) is now DERIVED (inc11, via the Hopf LES)")
      (check run (brunerie-pi4-s3-stalled-p)
             "pi_4(S^3) = Z/2 (Brunerie number) stays STALLED -- honestly out of reach"))

    ;; ---- suspension HIT + connectivity + Freudenthal (increment 10) ----
    (let* ((s0 (s0-type))
           (susp (make-suspension s0)))
      ;; The suspension HIT: poles + a meridian per base point, endpoints reduce.
      (check run (eql (suspension-north susp) :north) "Sigma S^0 has the north pole")
      (check run (eql (suspension-south susp) :south) "Sigma S^0 has the south pole")
      (check run (suspension-meridian-endpoints-p susp :pt0)
             "merid pt0 : north -> south (endpoints reduce definitionally)")
      (check run (suspension-meridian-endpoints-p susp :pt1)
             "merid pt1 : north -> south (a second, distinct meridian)")
      ;; Sigma S^0 ~= S^1: two meridians compose to the circle loop.
      (check run (suspension-of-s0-is-circle-p)
             "Sigma S^0 ~= S^1: two meridians compose to a north=north loop (the circle)")
      ;; The recursor: north/south images + a meridian-image family; point-betas.
      (let* ((b (bool-type))
             (rec (susp-rec susp b t nil
                            (lambda (x) (declare (ignore x))
                              (make-cpath b (lambda (i) (ecase i (:0 t) (:1 nil))))))))
        (check run (susp-rec-point-beta-p rec t nil)
               "susp-rec point-betas: rec @ north = t, rec @ south = nil")
        (check run (eq (cpath-i0 (susp-rec-app-merid rec :pt0)) t)
               "susp-rec sends merid to a path north-image=south-image (i0 = t)"))

      ;; Connectivity: S^0 is (-1)-connected; suspension raises it by one.
      (check run (= (suspension-connectivity -1) 0) "Sigma raises connectivity: (-1)->0 (S^0 -> S^1)")
      (check run (= (sphere-connectivity 1) 0) "S^1 is 0-connected")
      (check run (= (sphere-connectivity 3) 2) "S^3 is 2-connected")
      (check run (sphere-connectivity-by-iterated-suspension-p 4)
             "S^4 connectivity (=3) computed by iterating suspension from S^0, not hard-coded")

      ;; Freudenthal RANGE: sigma : pi_k(S^n) -> pi_{k+1}(S^{n+1}).
      (check run (sphere-suspension-iso-p 2 2) "Freudenthal: pi_2(S^2)->pi_3(S^3) is an ISO (k=2 <= 2n-2=2)")
      (check run (not (sphere-suspension-iso-p 1 1)) "Freudenthal: pi_1(S^1)->pi_2(S^2) is NOT an iso (it is the edge)")
      (check run (freudenthal-surj-p (sphere-connectivity 1) 1)
             "Freudenthal: pi_1(S^1)->pi_2(S^2) is the edge SURJECTION (k=1=2c+1)")

      ;; The diagonal pi_n(S^n)=Z: cyclic + degree surjects onto Z.
      (check run (degree-suspension-invariant-p) "degree is suspension-invariant (winding base computes)")
      (check run (sphere-diagonal-cyclic-p 5) "pi_5(S^5) is cyclic (Freudenthal up the diagonal from pi_1=Z)")
      (check run (degree-surjective-p 4) "deg : pi_4(S^4) ->> Z is surjective (degree-k maps exist)")
      (check run (pi-sphere-diagonal-is-Z-p 1) "pi_1(S^1)=Z (base case: a NORMALISATION, winding computes)")
      (check run (pi-sphere-diagonal-is-Z-p 2) "pi_2(S^2)=Z (DERIVED via Freudenthal diagonal)")
      (check run (pi-sphere-diagonal-is-Z-p 6) "pi_6(S^6)=Z (DERIVED, deep on the diagonal)")
      ;; the loop-space table now agrees on every cell of the filled triangle.
      (check run (eq (pi-n (list :sphere 4) 4) :Z) "pi_4(S^4)=Z (diagonal cell)")
      (check run (sphere-below-diagonal-trivial-p 4 2) "pi_2(S^4)=0 (below diagonal: connectivity)")
      (check run (eq (pi-n (list :sphere 4) 1) :trivial) "pi_1(S^4)=0 (below diagonal)")
      (check run (sphere-above-diagonal-stalled-p 3 4) "pi_4(S^3) stays stalled (above diagonal)")
      (check run (sphere-above-diagonal-stalled-p 4 7) "pi_7(S^4) stays stalled (above diagonal)")

      ;; The honest partial fact about the Brunerie group.
      (check run (pi4-s3-cyclic-by-freudenthal-p)
             "HONEST PARTIAL: Freudenthal's edge surjection pi_3(S^2)->>pi_4(S^3) forces pi_4(S^3) CYCLIC (consistent with Z/2; value still stalled)"))

    ;; ---- the Hopf fibration LES -> pi_3(S^2)=Z (increment 11) ----
    (let ((fib (make-hopf-fibration)))
      ;; The fibration S^1 -> S^3 -> S^2.
      (check run (eq (fibration-fibre fib) :circle) "Hopf fibre is S^1")
      (check run (equal (fibration-total fib) '(:sphere 3)) "Hopf total space is S^3")
      (check run (equal (fibration-base fib) '(:sphere 2)) "Hopf base is S^2")
      ;; The LES middle map pi_3(E)->pi_3(B) is an iso (both fibre neighbours vanish).
      (check run (eq (les-pi :circle 3) :trivial) "pi_3(S^1)=0 (circle aspherical)")
      (check run (eq (les-pi :circle 2) :trivial) "pi_2(S^1)=0 (circle aspherical)")
      (check run (les-mid-map-iso-p fib 3)
             "Hopf LES: pi_3(S^3)->pi_3(S^2) is an ISO (pi_3(S^1)=pi_2(S^1)=0)")
      (check run (les-segment-exact-p fib 3) "the n=3 LES segment is well-formed (no stalled inputs)")
      ;; The derivation: pi_3(S^2) = pi_3(S^3) = Z, through the iso.
      (check run (eq (hopf-derived-pi3-base) :Z) "Hopf LES derives pi_3(S^2) = pi_3(S^3) = Z")
      (check run (hopf-pi3-s2-is-Z-p) "pi_3(S^2) = Z (the Hopf map), derived + table agrees")
      (check run (eq (pi-n (list :sphere 2) 3) :Z) "table: pi_3(S^2)=Z (the Hopf cell)")
      ;; the diagonal source is intact and the rest of the column still stalls.
      (check run (eq (pi-n (list :sphere 3) 3) :Z) "pi_3(S^3)=Z (the diagonal source of the iso)")
      (check run (eq (pi-n (list :sphere 2) 4) :stalled) "pi_4(S^2) still stalls (not reached by this LES)")
      ;; The local cubical content of the Hopf family (mechanism witness).
      (check run (hopf-family-fibre-at-base-p) "the Hopf family H:S^2->U has H(base)=S^1 (the fibre)")
      (check run (hopf-surf-action-ua-computes-p)
             "the surf action's ua COMPUTES (ua-beta fires on the fibre automorphism) -- local Hopf content")
      ;; HONEST PARTIAL on the Brunerie group.
      (check run (hopf-les-relates-pi4-s3-s2-p)
             "HONEST PARTIAL: Hopf LES gives pi_4(S^3) ~= pi_4(S^2) -- relates two unknowns, pins NEITHER (Brunerie still stalled)"))

    ;; ---- the flattening lemma: int helix contractible (increment 12) ----
    ;; The loop-lift on the universal cover is the COMPUTING +1 (ua-beta).
    (check run (eq (car (cover-point 3)) :base) "a cover point is (base, n)")
    (check run (= (cover-point-fibre (cover-loop-lift (cover-point 5))) 6)
           "the loop-lift sends (base,5) -> (base,6) (the lifted loop edge, computing)")
    (check run (cover-loop-lift-computes-p) "the loop-lift is the computing +1 across a range (ua-beta)")
    ;; The retraction onto the centre reduces, in both directions.
    (check run (equal (cover-retract-to-centre (cover-point 4)) (cover-centre))
           "(base,4) retracts to the centre (base,0) -- computed by iterating the lift")
    (check run (equal (cover-retract-to-centre (cover-point -3)) (cover-centre))
           "(base,-3) retracts to the centre too (inverse lift)")
    (check run (cover-point-retracts-p 7) "the contraction homotopy at (base,7) lands on the centre")
    (check run (cover-reachable-from-centre-p -5)
           "(base,-5) is reached from the centre by 5 inverse lifts (length = winding = -5)")
    ;; The headline: the total space of the universal cover is contractible.
    (check run (cover-total-space-contractible-p)
           "FLATTENING LEMMA (computing instance): Sigma(x:S^1) helix(x) is CONTRACTIBLE")
    (check run (flattening-lemma-cover-instance-p)
           "the flattening lemma runs end-to-end on the universal cover")
    ;; HONEST: the same lemma sharpens the Hopf wall to the S^1 h-space coherence.
    (check run (flattening-reduces-hopf-wall-to-hspace-p)
           "HONEST: flattening reduces the int Hopf ~= S^3 wall to the S^1 h-space coherence; pi_4(S^3) still stalled")

    ;; ---- the S^1 h-space: multiplication + genuine rotation (increment 13) ----
    ;; mu on pi_1 = addition, computed through the loop-lift; unit + assoc + comm.
    (check run (= (circle-mult-winding 3 4) 7) "mu(loop^3, loop^4) = loop^7 on pi_1 (computed via the lift)")
    (check run (= (circle-mult-winding 5 -2) 3) "mu(loop^5, loop^-2) = loop^3 (inverse lift)")
    (check run (circle-mult-unit-left-p 6) "left unit: mu(base, loop^6) = loop^6")
    (check run (circle-mult-unit-right-p 6) "right unit: mu(loop^6, base) = loop^6")
    (check run (circle-mult-associative-on-pi1-p 2 3 5) "mu associative on pi_1")
    (check run (circle-mult-commutative-on-pi1-p 4 -7) "mu commutative on pi_1 (abelian)")
    ;; The genuine rotation equivalence -- the real Hopf fibre automorphism.
    (check run (= (ua-beta (circle-rotation-equiv 3) 10) 13) "rotation by loop^3 acts as +3 (ua-beta computes)")
    (check run (circle-rotation-ua-computes-p 2) "rotation-2 ua computes (+2 across a range)")
    (check run (circle-rotation-is-equivalence-p 4) "rotation by loop^4 is a genuine equivalence (forward/backward = id)")
    (check run (circle-rotation-unit-is-identity-p) "rotation by loop^0 = identity (the h-space unit)")
    (check run (circle-rotation-composes-additively-p 2 3) "rotation 2 then 3 = rotation 5 (h-space mult on automorphisms)")
    ;; The genuine rotation now feeds the Hopf family (inc11's stand-in replaced).
    (check run (hopf-fibre-automorphism-is-genuine-rotation-p)
           "the Hopf fibre is glued by the GENUINE circle rotation (not inc11's Z-succ stand-in)")
    (check run (circle-hspace-pi1-structure-computes-p)
           "the whole S^1 h-space structure computes on the pi_1 shadow")
    ;; HONEST RESIDUAL: the higher coherence cells remain the wall.
    (check run (circle-hspace-higher-coherence-residual-p)
           "HONEST: pi_1 h-space computes; the higher associator/unitor coherence (for int Hopf ~= S^3) stays the wall; pi_4(S^3) stalled")

    ;; ---- the h-space coherence: associator/unitor 2-cells, pentagon (inc14) ----
    (check run (= (circle-bracket-left 2 3 5) (circle-bracket-right 2 3 5))
           "mu associative: ((2.3).5) = (2.(3.5)) on windings")
    (check run (circle-associator-coherent-p 2 3 5)
           "the associator is a boundary-coherent 2-cell (bracketings agree)")
    (check run (circle-associator-coherent-p -1 4 -6) "associator coherent on mixed signs")
    (check run (circle-unitor-coherent-p 7) "the unitor 2-cell closes (mu(0,7)=7=mu(7,0))")
    ;; the pentagon: all five bracketings of four elements agree -> it closes.
    (check run (apply #'= (circle-five-bracketings 1 2 3 4)) "all five bracketings of (1,2,3,4) agree")
    (check run (circle-pentagon-closes-p 1 2 3 4) "the associator PENTAGON closes (= 1+2+3+4)")
    (check run (circle-pentagon-closes-p -2 5 -3 8) "the pentagon closes on mixed signs (= 8)")
    (check run (circle-triangle-coherent-p 3 -4) "the unit TRIANGLE commutes (associator through unit = unitors)")
    (check run (circle-hspace-coherence-computes-p)
           "the whole h-space coherence (associator/unitor/pentagon/triangle) computes on the pi_1 shadow")
    ;; HONEST RESIDUAL: the 3-cell pentagonator + computing truncation remain.
    (check run (hspace-coherence-residual-p)
           "HONEST: coherence shadow computes; the 3-cell pentagonator + Z/2-computing truncation stay the wall; pi_4(S^3) stalled")

    ;; ---- where the 2 comes from: the Brunerie number as a cup square (inc15) ----
    ;; The cohomology ring H*(S^2 x S^2): a^2 = b^2 = 0, ab the top class.
    (let* ((a (make-cohomology-class 0 1 0 0))
           (b (make-cohomology-class 0 0 1 0)))
      (check run (= (cohomology-class-cab (cohomology-cup a a)) 0) "a^2 = 0 in H*(S^2 x S^2)")
      (check run (= (cohomology-class-cab (cohomology-cup b b)) 0) "b^2 = 0")
      (check run (= (cohomology-class-cab (cohomology-cup a b)) 1) "a . b = ab (the top class)"))
    ;; THE 2: (a+b)^2 = 2ab -- the cup-square coefficient is the Brunerie number.
    (check run (= (whitehead-square-hopf-invariant) 2)
           "(a+b)^2 = 2ab: the Whitehead square's Hopf invariant is 2")
    (check run (classical-brunerie-number-is-2-p)
           "THE BRUNERIE NUMBER IS 2 -- computed the classical way (cup square in H*(S^2 x S^2))")
    ;; consistency with pi_3(S^2)=Z: [iota_2,iota_2] = 2 eta.
    (check run (whitehead-square-is-2-eta-p)
           "[iota_2,iota_2] = 2 eta in pi_3(S^2)=Z (H(eta)=1, H([iota,iota])=2)")
    ;; the same 2 for every even sphere (the binomial coefficient C(2,1)).
    (check run (even-sphere-hopf-invariant-always-2-p)
           "the Whitehead-square Hopf invariant is 2 for every even sphere (binomial C(2,1), n-independent)")
    ;; THE HONEST LINE: classical 2 computes; the cubical Z/2 truncation stalls.
    (check run (brunerie-2-located-and-cubical-stalls-p)
           "HONEST FRONTIER: the classical Brunerie 2 COMPUTES (cup square); the type-theoretic pi_4(S^3)=Z/2 as a CUBICAL truncation stays stalled -- two routes, one runs here")

    ;; ---- assembling the classical group pi_4(S^3)=Z/2 (increment 16) ----
    ;; Nontriviality: H*(CP^2)=Z[u]/u^3, Sq^2(u)=u^2 =/= 0 (a cup square).
    (check run (= (cp2-sq2-of-u) 1) "Sq^2(u) = u^2 = generator in H*(CP^2) (cup square)")
    (check run (eta-stably-nontrivial-p) "eta is stably non-trivial: Sq^2 =/= 0 on the cofibre CP^2 (order =/= 1)")
    ;; Order divides 2: 2 eta_3 = Sigma^2[iota_2,iota_2] = 0.
    (check run (pi4-s3-generator-order-divides-2-p)
           "order | 2: 2 eta_3 = Sigma^2(2 eta) = Sigma^2[iota_2,iota_2] = 0 (Whitehead suspends to zero)")
    ;; Cyclic: Freudenthal edge surjection (inc10).
    (check run (pi4-s3-cyclic-p) "pi_4(S^3) is cyclic (Freudenthal edge surjection pi_3(S^2)->>pi_4(S^3))")
    ;; The assembly: cyclic + order|2 + nontrivial => Z/2.
    (check run (eql (pi4-s3-classical-order) 2) "the classical order of pi_4(S^3) comes out 2")
    (check run (pi4-s3-is-Z-mod-2-classically-p)
           "pi_4(S^3) = Z/2, CLASSICALLY assembled (cyclic + order-2 + nontrivial)")
    ;; THE HONEST FRONTIER, complete: classical Z/2 assembled, cubical still stalls.
    (check run (pi4-s3-classical-computed-cubical-stalls-p)
           "HONEST: classical pi_4(S^3)=Z/2 fully assembled; the CUBICAL normalisation (Brunerie's thesis) stays stalled -- table pi-n(:sphere 3) 4 untouched")
    ;; the cubical table is genuinely untouched by the classical assembly.
    (check run (eq (pi-n (list :sphere 3) 4) :stalled)
           "the cubical kernel still does NOT compute pi_4(S^3) (table stays :stalled)")

    ;; ---- the join HIT A*B: first piece of the cubical normalisation (inc17) ----
    (let ((j (make-cjoin (s0-type) (s0-type))))
      (check run (equal (cjoin-inl j :pt0) '(:inl . :pt0)) "inl : A -> A*B")
      (check run (equal (cjoin-inr j :pt1) '(:inr . :pt1)) "inr : B -> A*B")
      (check run (cjoin-push-endpoints-p j :pt0 :pt1)
             "push pt0 pt1 : inl pt0 -> inr pt1 (the join path, endpoints reduce)")
      ;; the recursor: inl/inr/push images, point-betas.
      (let ((rec (cjoin-rec j (bool-type)
                            (lambda (a) (declare (ignore a)) t)
                            (lambda (b) (declare (ignore b)) nil)
                            (lambda (a b) (declare (ignore a b))
                              (make-cpath (bool-type) (lambda (i) (ecase i (:0 t) (:1 nil))))))))
        (check run (cjoin-rec-point-beta-p rec :pt0 :pt1 t nil)
               "join recursor point-betas: rec(inl)=t, rec(inr)=nil")))
    ;; A * 1 = cone A is contractible (every point pushes to the apex).
    (check run (cone-contractible-p (s0-type) '(:pt0 :pt1))
           "A * 1 = cone A is CONTRACTIBLE (every inl a pushes to the apex)")
    (check run (cone-contractible-p (bool-type) '(t nil))
           "the cone is contractible on a different base too")
    ;; Connectivity: join adds conn(A)+conn(B)+2; S^n * S^m = S^{n+m+1}.
    (check run (= (cjoin-connectivity 0 0) 2) "conn(A*B) = conn A + conn B + 2; (0,0)->2")
    (check run (= (sphere-join-dimension 1 1) 3) "S^1 * S^1 = S^3 (dimension)")
    (check run (sphere-join-connectivity-matches-p 1 1) "conn(S^1 * S^1) matches conn(S^3) both ways")
    (check run (sphere-join-connectivity-matches-p 2 3) "conn(S^2 * S^3) = conn(S^6) (general join law)")
    (check run (s1-join-s1-is-s3-connectivity-p)
           "S^1 * S^1 = S^3 at the level of connectivity (= 2): the Hopf construction's domain")
    ;; HONEST: the typed Glue equivalence S^1 * S^1 ~= S^3 stays the wall.
    (check run (join-s1-s1-typed-equiv-to-s3-stalls-p)
           "HONEST: join connectivity computes; the typed S^1*S^1 ~= S^3 equivalence stays the coherence wall; pi_4(S^3) stalled")

    ;; ---- the Hopf construction: eta : S^3 -> S^2 as a cubical term (inc18) ----
    ;; The construction on a concrete base: well-formed + point-betas.
    (check run (hopf-construction-well-formed-p (s0-type) (lambda (a b) (declare (ignore b)) a)
                                                '((:pt0 :pt1) (:pt1 :pt0)))
           "Hopf construction well-formed: push (a,b) |-> merid(mu(a,b)), north->south")
    (check run (hopf-construction-point-beta-p (s0-type) (lambda (a b) (declare (ignore b)) a) :pt0 :pt1)
           "Hopf construction point-betas: inl |-> north, inr |-> south")
    ;; The bidegree of the S^1 multiplication = (1,1), from the unit laws.
    (check run (= (circle-mult-first-degree) 1) "degree of a|->mu(a,base) = 1 (right unit)")
    (check run (= (circle-mult-second-degree) 1) "degree of b|->mu(base,b) = 1 (left unit)")
    (check run (equal (circle-hopf-construction-bidegree) '(1 1)) "the S^1 multiplication has bidegree (1,1)")
    ;; The cubical Hopf map eta and its invariant.
    (check run (= (circle-hopf-invariant-via-bidegree) 1)
           "Hopf invariant of eta = product of the bidegree = 1.1 = 1")
    (check run (cubical-hopf-map-invariant-is-1-p)
           "eta = H(rotation) has Hopf invariant 1 (from the multiplication), matching H(eta)=1")
    (check run (cubical-hopf-map-well-formed-p)
           "eta on S^1 is a well-formed cubical term (S^1 * S^1 -> Sigma S^1 = the cubical S^3 -> S^2)")
    ;; HONEST: eta built + invariant computes; the fibration total = S^3 stays the wall.
    (check run (cubical-hopf-construction-residual-p)
           "HONEST: eta is built and its Hopf invariant computes to 1; H(eta) = the Hopf fibration (total S^3) stays the wall; pi_4(S^3) stalled")

    ;; ---- pi_1(M(Z/2,1)) = Z/2 computed cubically: the Brunerie shape (inc19) ----
    ;; The degree-2 attaching map, via winding.
    (check run (= (moore-attaching-degree) 2) "M(Z/2,1)'s 2-cell attaches by the degree-2 map (winding loop^2 = 2)")
    ;; The cubical encode: a^N |-> N mod 2, via ua-beta on the order-2 swap.
    (check run (= (moore-encode 0) 0) "encode(a^0) = 0")
    (check run (= (moore-encode 1) 1) "encode(a^1) = 1 (a =/= 1)")
    (check run (= (moore-encode 2) 0) "encode(a^2) = 0 (a^2 = 1) -- the order-2 swap closes")
    (check run (= (moore-encode 3) 1) "encode(a^3) = 1")
    (check run (moore-encode-is-mod-2-p) "the cubical encode computes a^N |-> N mod 2 (ua-beta on bool-not)")
    (check run (every #'moore-encode-decode-id-p '(0 1)) "encode . decode = id on Z/2")
    (check run (eql (moore-loop-order) 2) "the loop a has order exactly 2")
    ;; The headline: pi_1 = Z/2, the first nontrivial FINITE homotopy group computed.
    (check run (moore-pi1-is-z2-p)
           "pi_1(M(Z/2,1)) = Z/2 -- the FIRST nontrivial finite homotopy group computed cubically (encode via ua-beta)")
    (check run (moore-z2-order-is-attaching-degree-p)
           "the 2 in Z/2 = the order of the swap = the degree of the attaching map (both computed)")
    ;; HONEST: the exact Brunerie shape, one dimension where it computes.
    (check run (moore-is-brunerie-shape-one-dim-p)
           "HONEST: pi_1=Z/2 (degree-2 attaching -> order-2 group) RUNS cubically; pi_4(S^3)=Z/2 is the same shape 3 dims up where Omega^4 truncation stalls")

    ;; ---- the set-truncation || - ||_0 as an operation (increment 20) ----
    ;; pi_0: path components.
    (check run (pi0-circle-is-point-p) "|| S^1 ||_0 = 1 (the circle is connected: one component)")
    (check run (pi0-s0-is-two-points-p) "|| S^0 ||_0 = S^0 (two components)")
    ;; the recursion principle: || A ||_0 -> B for B a set.
    (let ((tr (omega-s1-truncation)))
      (check run (= (funcall (trunc0-rec tr (lambda (n) (* 10 n))) (trunc0-incl tr 3)) 30)
             "|| - ||_0 recursion: a component-constant map factors through the truncation"))
    ;; pi_1(S^1) = || Omega S^1 ||_0 = Z, grounded in the operation.
    (check run (pi1-s1-via-truncation-is-Z-p)
           "pi_1(S^1) = || Omega S^1 ||_0 = Z (components = winding classes, loop^2 =/= loop^3)")
    ;; pi_1(M(Z/2,1)) = || Omega M ||_0 = Z/2, grounded.
    (check run (pi1-moore-via-truncation-is-Z2-p)
           "pi_1(M(Z/2,1)) = || Omega M ||_0 = Z/2 (a^2 ~ a^0, a^3 ~ a^1, a =/= 1)")
    ;; the summit truncation stalls.
    (check run (pi4-s3-via-truncation-stalls-p)
           "|| Omega^4 S^3 ||_0 does NOT compute (no representable loops -- the coherence wall)")
    ;; HONEST: || - ||_0 grounds the computing pi_n; the summit stalls.
    (check run (truncation-grounds-computing-pi-n-p)
           "HONEST: || - ||_0 is now a real operation grounding pi_1(S^1)=Z and pi_1(M)=Z/2; || Omega^4 S^3 ||_0 stalls; pi_4(S^3) stalled")

    ;; ---- the uniform engine: pi_1(M(Z/k,1))=Z/k for all k (increment 21) ----
    ;; the order-k cyclic monodromy and its encode = N mod k.
    (check run (cyclic-equiv-has-order-k-p 5) "the cyclic monodromy on Z/5 has order exactly 5 (ua-beta)")
    (check run (= (cyclic-encode 3 7) 1) "encode_3(a^7) = 7 mod 3 = 1 (ua-beta on the 3-cycle)")
    (check run (= (cyclic-encode 4 4) 0) "encode_4(a^4) = 0 (the 4-cycle closes)")
    (check run (= (cyclic-encode 6 -1) 5) "encode_6(a^-1) = 5 (backward monodromy)")
    (check run (cyclic-encode-is-mod-k-p 3) "encode_3 computes a^N |-> N mod 3 across a range")
    (check run (eql (cyclic-loop-order 5) 5) "the loop a has order exactly 5 in pi_1(M(Z/5,1))")
    ;; pi_1(M(Z/k,1)) = Z/k for individual k, then uniformly.
    (check run (cyclic-pi1-is-Zk-p 3) "pi_1(M(Z/3,1)) = Z/3 (computed)")
    (check run (cyclic-pi1-is-Zk-p 7) "pi_1(M(Z/7,1)) = Z/7 (computed)")
    (check run (moore-Zk-for-all-k-p)
           "UNIFORM: pi_1(M(Z/k,1)) = Z/k for every k (2..7) -- one engine, every finite cyclic group")
    ;; HONEST: the Brunerie Z/2 is the k=2 row; the summit stays stalled.
    (check run (brunerie-2-is-k2-instance-p)
           "HONEST: the Brunerie Z/2 is the k=2 instance of the uniform Z/k engine at pi_1; pi_4(S^3) (3 dims up) stays stalled")

    ;; ---- a COMPUTING encode at pi_4: pi_4(S^3) -> Z/2 (increment 22) ----
    ;; the Hopf invariant on pi_3(S^2)=Z computes (H(eta)=1).
    (check run (= (pi3-s2-hopf-invariant 5) 5) "Hopf invariant of 5.eta in pi_3(S^2)=Z is 5 (H(eta)=1)")
    (check run (= (pi4-s3-suspension-kernel-generator) 2) "Sigma's kernel generator has Hopf invariant 2 (= inc15 cup square)")
    ;; the encode values compute: eta_3 |-> 1, 2.eta_3 |-> 0, n.eta_3 |-> n mod 2.
    (check run (pi4-s3-encode-eta3-is-1-p) "encode(eta_3) = 1 (generator non-trivial)")
    (check run (pi4-s3-encode-2eta3-is-0-p) "encode(2.eta_3) = 0 (the order-2 relation, computed)")
    (check run (= (pi4-s3-encode 7) 1) "encode(7.eta_3) = 7 mod 2 = 1")
    (check run (= (pi4-s3-encode -4) 0) "encode(-4.eta_3) = 0")
    (check run (pi4-s3-encode-computes-p)
           "the encode pi_4(S^3) -> Z/2 = (Hopf invariant) mod 2 computes its values across a range (ua-beta)")
    ;; the iso-property given EHP; the kernel's 2 is the cup square.
    (check run (pi4-s3-encode-iso-given-ehp-p)
           "the encode is an iso pi_4(S^3) ~= Z/2 GIVEN ker Sigma = <2 eta> (EHP encoded; the 2 is inc15's cup square)")
    ;; HONEST: encode values compute; the full normalisation + EHP iso stay the wall.
    (check run (pi4-s3-cubical-encode-residual-p)
           "HONEST: a computing encode at pi_4 (values reduce); the FULL || Omega^4 S^3 ||_0 normalisation stays stalled; pi-n(:sphere 3) 4 :stalled")

    ;; ---- the encode/decode equivalence at pi_4, and the ceiling (increment 23) ----
    (check run (= (pi4-s3-decode 3) 1) "decode : Z/2 -> pi_4(S^3), 3 |-> 1.eta_3 (class 1)")
    (check run (pi4-s3-encode-decode-id-p) "encode . decode = id on Z/2 (computes)")
    (check run (pi4-s3-order-2-relation-holds-p) "the relation 2.eta_3 = 0 holds (eta_3 has order 2)")
    (check run (pi4-s3-decode-encode-id-p) "decode . encode = id on pi_4(S^3) given the order-2 relation (round-trip on the generator index)")
    (check run (pi4-s3-is-Z2-encode-decode-p)
           "pi_4(S^3) = Z/2 in encode/decode form -- the same shape as pi_1(S^1)=Z, climbed to pi_4")
    (check run (pi4-s3-computed-at-presentation-level-p)
           "pi_4(S^3)=Z/2 computed at the PRESENTATION level: every input computed (Hopf inv, mod-2, the 2) or named-encoded (Freudenthal, EHP)")
    ;; THE CEILING, named: presentation-level computes; loop-transport normalisation does not.
    (check run (pi4-s3-loop-transport-normalisation-is-the-ceiling-p)
           "HONEST CEILING: pi_4(S^3)=Z/2 computes at the presentation level; the loop-transport normalisation of || Omega^4 S^3 ||_0 (representable 4-loops) is beyond this kernel; pi-n(:sphere 3) 4 :stalled")

    ;; ---- Glue comp (increment 8) ----
    ;; The hardest definition in cubical type theory: a base A glued, only on a
    ;; cofibration phi, to a partial type T via an equivalence e : T ~= A; and the
    ;; comp along a LINE of such Glue types, built on the increment-5 face lattice.
    (let ((noteq (bool-not-equivalence))
          (succ (int-succ-equivalence))
          (b (bool-type))
          (z (int-type)))

      ;; A TOTAL Glue type (phi = 1F): Glue [1F -> (T,e)] A IS T, unglue = e.fun.
      (let ((gty (make-gtype (f-top) noteq b)))
        (check run (gtype-p gty) "make-gtype builds a Glue type Glue [1F -> (Bool,not)] Bool")
        (check run (eq (glue-type-at gty '()) (rosette-hott-core:hott-equivalence-source noteq))
               "Glue [1F -> (T,e)] A = T on phi (definitional reduction)")
        ;; unglue (glue t a) = a : the first beta-law (a = e.fun t on phi).
        (check run (eq (unglue gty (glue gty t nil)) nil)
               "unglue (glue T nil) = nil = not(T)  [unglue = e.fun on phi]")
        (check run (glue-unglue-beta-p gty t nil)
               "beta-law 1: unglue (glue t a) = a on phi")
        (check run (glue-unglue-beta-p gty nil t)
               "beta-law 1 (other point): unglue (glue nil t) = t")
        ;; glue (unglue g) recovers the BASE part on phi (NOT the full eta law:
        ;; the T-part recovers only up to the equivalence's left-homotopy residual).
        (check run (unglue-glue-base-beta-p gty (glue gty t nil))
               "beta-law 2 (BASE part): glue (unglue g) recovers the base on phi (T-part only up to the left-homotopy residual)"))

      ;; A genuinely PARTIAL Glue type (phi = (i=0)): off phi the Glue type is the
      ;; base A; on phi it is the glued partial type T.  The cofibration is respected.
      (let ((gty (make-gtype (f-eq0 'i) succ z)))
        (check run (eq (glue-type-at gty '((i . :0))) (rosette-hott-core:hott-equivalence-source succ))
               "partial Glue: on phi (i=0) the type is T")
        (check run (eq (glue-type-at gty '((i . :1))) z)
               "partial Glue: OFF phi (i=1) the type is the base A (cofibration respected)")
        ;; off phi a glued value simply IS its base part.
        (check run (= (unglue gty (glue gty nil 99 '((i . :1))) '((i . :1))) 99)
               "partial Glue: off phi, unglue is the identity on the base part"))

      ;; TOTAL-case comp: comp-glue along the ua glue line reduces to e.fun = ua-beta.
      (check run (comp-glue-total-reduces-to-ua-p succ 3 :test #'=)
             "comp-glue (total case) reduces to transp (ua e) = e.fun: succ 3 -> ua-beta = 4")
      (check run (comp-glue-total-reduces-to-ua-p succ -1 :test #'=)
             "comp-glue (total case): succ -1 -> 0  (matches ua-beta)")
      (check run (comp-glue-total-reduces-to-ua-p noteq t)
             "comp-glue (total case): not T -> NIL  (matches ua-beta on Bool)")
      ;; spell out that the lid base IS ua-beta on the nose.
      (let* ((gl (ua-glue-line succ))
             (g0 (glue-line-gtype-at gl :0))
             (cap (glue g0 3 (1+ 3) '((i . :0))))
             (lid (comp-glue gl cap (make-system '()) :dim 'i :test #'=)))
        (check run (= (unglue (glue-line-gtype-at gl :1) lid '((i . :1))) (ua-beta succ 3))
               "comp-glue total lid base = (ua-beta succ 3) = 4, on the nose"))

      ;; GENUINELY PARTIAL comp over phi = (i=1): where phi holds the result is
      ;; governed by the partial type T / equivalence; the system supplies the lid.
      (let* ((gl (make-glue-type-line (f-eq1 'i)
                                      (make-const-line z) (make-const-line z)
                                      (constantly succ)))
             (g0 (glue-line-gtype-at gl :0))      ; at i0, (i=1) is FALSE -> Glue = A
             (g1 (glue-line-gtype-at gl :1))      ; at i1, (i=1) holds   -> Glue = T
             (cap (glue g0 nil 5 '((i . :0))))    ; off-phi cap: base 5
             (sysval (glue g1 7 8 '((i . :1))))   ; on-phi system value: T-part 7, base succ 7 = 8
             (sys (make-system (list (cons (f-eq1 'i) sysval))))
             (lid (comp-glue gl cap sys :dim 'i :test #'=)))
        (check run (= (glue-value-t-part lid) 7)
               "partial comp-glue: on phi (i=1) the lid's T-part comes from the system (7)")
        (check run (= (unglue g1 lid '((i . :1))) 8)
               "partial comp-glue: lid base = e1.fun (T-part) = succ 7 = 8 (governed by the equivalence)"))

      ;; The cap-system ADJACENCY side-condition: comp-glue rejects a system whose
      ;; (DIM=0) value disagrees with the cap (the coherence the value-fold could not state).
      (let* ((gl (make-glue-type-line (f-top)
                                      (make-const-line z) (make-const-line z)
                                      (constantly succ)))
             (g0 (glue-line-gtype-at gl :0))
             (cap (glue g0 10 11 '()))                 ; cap: T-part 10, base succ 10 = 11
             (bad (make-system (list (cons (f-eq0 'i) (glue g0 98 99 '())))))) ; disagrees at i0
        (check run (handler-case (progn (comp-glue gl cap bad :dim 'i :test #'=) nil)
                     (error () t))
               "comp-glue enforces cap-system adjacency at (DIM=0) (rejects a disagreeing system)")
        ;; a COMPATIBLE system (agrees with the cap at i0) composes and reads its i1 lid.
        (let* ((g1 (glue-line-gtype-at gl :1))
               (ok (make-system (list (cons (f-eq0 'i) (glue g0 10 11 '()))
                                      (cons (f-eq1 'i) (glue g1 20 21 '())))))
               (lid (comp-glue gl cap ok :dim 'i :test #'=)))
          (check run (= (unglue g1 lid '()) 21)
                 "comp-glue with a compatible system reads the system's lid at (DIM=1) (21)"))))))
