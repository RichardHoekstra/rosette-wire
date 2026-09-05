;;;; homology.lisp --- (co)homology, Betti numbers, torsion, Euler char.
;;;;
;;;; Over a FIELD, H_n = ker d_n / im d_{n+1} is determined by rank-nullity:
;;;;
;;;;     b_n = dim ker d_n - rank d_{n+1}
;;;;         = (dim C_n - rank d_n) - rank d_{n+1}.
;;;;
;;;; This is pure linear algebra (rank.lisp, exact rational).  Over Z the
;;;; same free rank appears, PLUS torsion: the invariant factors > 1 of
;;;; d_{n+1} via Smith normal form.  We do NOT recompute the Z side -- we
;;;; delegate to rosette-smith-normal-form's INTEGER-HOMOLOGY, the partial Z
;;;; back-end this library is the abstraction on top of.

(in-package #:rosette-chain-complex)

;;; ------------------------------------------------------------------
;;; field-side: Betti numbers by rank-nullity (the honest statement)
;;; ------------------------------------------------------------------

(defun betti (cc n)
  "The n-th Betti number b_n = dim H_n over a field, by rank-nullity.
b_n = (dim C_n - rank d_n) - rank d_{n+1}.  Exact (rational rank)."
  (let* ((dims (chain-complex-dims cc))
         (top  (chain-complex-top cc))
         (dim-cn (if (<= 0 n top) (nth n dims) 0))
         (d-n   (boundary-matrix cc n))           ; d_n : C_n -> C_{n-1}
         (d-n+1 (boundary-matrix cc (1+ n)))      ; d_{n+1} : C_{n+1} -> C_n
         (rank-dn   (field-rank d-n))
         (rank-dn+1 (field-rank d-n+1)))
    (- (- dim-cn rank-dn) rank-dn+1)))

(defun dim-h (cc n) "dim H_n over a field (= the Betti number)." (betti cc n))

(defun betti-numbers (cc)
  "(b_0 b_1 ... b_N), the Betti numbers of CC over a field."
  (loop for n from 0 to (chain-complex-top cc) collect (betti cc n)))

;;; ------------------------------------------------------------------
;;; Z-side: free rank + torsion (delegated to rosette-smith-normal-form)
;;; ------------------------------------------------------------------

(defun homology (cc)
  "The integer homology (H_0 ... H_N): each a rosette-smith-normal-form
HOMOLOGY-GROUP carrying the free rank (= Betti b_n) AND the torsion
coefficients (> 1) -- the torsion the field/Betti homology cannot see
(Klein bottle H_1 = Z (+) Z/2).  Delegates to the Z back-end."
  (integer-homology (chain-complex-boundaries cc) (chain-complex-dims cc)))

(defun torsion-coefficients (cc n)
  "The torsion coefficients t_i > 1 of H_n over Z (NIL if torsion-free)."
  (hom-torsion (nth n (homology cc))))

;;; ------------------------------------------------------------------
;;; the dual cochain complex + cohomology
;;; ------------------------------------------------------------------
;;;
;;; The cochain complex is the dual: the coboundary delta^n : C^n -> C^{n+1}
;;; is the transpose of d_{n+1}.  Over a field, H^n = H_n (universal
;;; coefficients with no torsion), so the cohomology Betti numbers equal the
;;; homology Betti numbers -- we expose this as a complex on the transposed
;;; maps so callers can treat delta^2 = 0 exactly as d^2 = 0.

(defun %transpose (m)
  "Transpose of a list-of-rows matrix M (NIL -> NIL)."
  (when m (apply #'mapcar #'list m)))

(defun cochain-complex (cc)
  "The dual cochain complex: coboundary delta^n = (d_{n+1})^T.  Returned as
a chain-complex on the transposed boundary maps, so delta^2 = 0 is checked
by the same D-SQUARED-ZERO-P.  The cochain dimensions equal the chain
dimensions (free modules are self-dual)."
  (let* ((bs   (chain-complex-boundaries cc))
         (dims (chain-complex-dims cc))
         (top  (1- (length dims)))
         ;; delta^n : C^n -> C^{n+1} is (d_{n+1})^T : a map "up" in degree.
         ;; Re-index as a chain complex on the reversed grading C^N..C^0 so
         ;; the same struct law applies: the boundary at step k is the
         ;; transpose of the matching d, reversed.
         (rev-boundaries
           (loop for k from top downto 1
                 collect (%transpose (nth (1- k) bs)))))
    (%make-chain-complex :boundaries rev-boundaries
                         :dims (reverse dims))))

(defun cohomology (cc n)
  "The n-th cohomology Betti number dim H^n.  Over a field H^n = H_n, so
this equals (betti cc n); exposed for symmetry and for the delta^2 = 0
witness via (cochain-complex cc)."
  (betti cc n))

;;; ------------------------------------------------------------------
;;; the Euler characteristic
;;; ------------------------------------------------------------------

(defun euler-characteristic (cc)
  "chi = sum_n (-1)^n dim C_n  (V - E + F for a surface).  This is a
homotopy invariant: it equals the alternating sum of the Betti numbers
(Euler-Poincare).  Computed here from the CHAIN dimensions."
  (loop for d in (chain-complex-dims cc)
        for n from 0
        sum (* (expt -1 n) d)))

(defun euler-from-betti (cc)
  "chi = sum_n (-1)^n b_n, the alternating sum of the Betti numbers.
Euler-Poincare: this MUST equal (euler-characteristic cc)."
  (loop for b in (betti-numbers cc)
        for n from 0
        sum (* (expt -1 n) b)))

;;; ------------------------------------------------------------------
;;; a homology report (Betti + torsion + Euler), printable
;;; ------------------------------------------------------------------

(defstruct (homology-report (:constructor %make-report))
  "A homology summary of a chain complex."
  (betti '() :type list)        ; (b_0 ... b_N)
  (torsion '() :type list)      ; ((t-coeffs for H_0) ... (for H_N))
  (euler 0 :type integer))

(defun report (cc)
  "A HOMOLOGY-REPORT of CC: the Betti numbers, per-degree torsion, and the
Euler characteristic (from the chain dimensions)."
  (let ((hs (homology cc)))
    (%make-report
     :betti (mapcar #'hom-betti hs)
     :torsion (mapcar #'hom-torsion hs)
     :euler (euler-characteristic cc))))
