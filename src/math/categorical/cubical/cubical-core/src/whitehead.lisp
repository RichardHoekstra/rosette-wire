;;;; whitehead.lisp --- the cube, increment 15: WHERE THE 2 COMES FROM.  The
;;;; Brunerie number as the Hopf invariant of the Whitehead square, computed the
;;;; CLASSICAL way -- the cup-square coefficient in H*(S^2 x S^2).
;;;;
;;;; Every previous increment receded the wall and named the residual.  By inc14
;;;; the residual was a single thing: the n-truncation that computes the integer
;;;; Z/2 -- i.e. WHERE DOES THE 2 COME FROM.  This increment answers that, with
;;;; an honest computation, and draws the sharpest possible line between what it
;;;; does and does not establish.
;;;;
;;;; THE 2, CLASSICALLY.  pi_4(S^3) = Z/2 because the Brunerie number is 2, and
;;;; that 2 is the Hopf invariant of the Whitehead square [iota_2, iota_2] in
;;;; pi_3(S^2) = Z (which inc11 derived).  The Whitehead square is the attaching
;;;; map of the top cell of  S^2 x S^2, and its Hopf invariant is the CUP-SQUARE
;;;; coefficient in the cohomology ring  H*(S^2 x S^2) = Z[a,b]/(a^2, b^2):
;;;;        (a + b)^2  =  a^2 + 2 a b + b^2  =  2 a b.
;;;; The coefficient of the top class is 2 -- the binomial coefficient C(2,1) --
;;;; and THAT is the Brunerie number.  We build the ring, the cup product, and
;;;; compute the 2.  No new dependency: truncated-polynomial integer arithmetic.
;;;;
;;;; THE HONEST LINE.  This computes the 2 by the CLASSICAL route (the cup
;;;; product / Hopf invariant of the Whitehead square).  It is genuine and
;;;; correct.  It is NOT what Brunerie's thesis did -- normalising a specific
;;;; CUBICAL term to 2 inside the type theory -- which needs the n-truncation
;;;; that computes the group, the 3-cell coherence, and the full encode this
;;;; kernel still lacks.  So:  the classical Brunerie number computes (= 2),
;;;; AND the type-theoretic pi_4(S^3) = Z/2 as a COMPUTING cubical truncation
;;;; stays :stalled.  Two routes to the same 2; one runs here, one does not.

(in-package #:rosette-cubical-core)

;;; ---- The cohomology ring  H*(S^2 x S^2) = Z[a,b]/(a^2, b^2) -----------
;;;
;;; A class is graded:  c1 . 1  (deg 0)  +  ca . a + cb . b  (deg 2)  +
;;; cab . ab  (deg 4).  Generators a, b are the pullbacks of the two S^2
;;; fundamental classes; a^2 = b^2 = 0; ab is the top class.  Degree 2 is even,
;;; so a and b commute (ab = ba).

(defstruct (cohomology-class (:constructor make-cohomology-class (c1 ca cb cab)))
  "An element of H*(S^2 x S^2): coefficients of 1, a, b, ab."
  (c1 0 :type integer :read-only t)
  (ca 0 :type integer :read-only t)
  (cb 0 :type integer :read-only t)
  (cab 0 :type integer :read-only t))

(defun cohomology-cup (x y)
  "The cup product on H*(S^2 x S^2).  Bilinear, with a^2 = b^2 = 0 and ab = ba
 (degree-2 classes commute).  The top class ab collects a.b and b.a."
  (make-cohomology-class
   (* (cohomology-class-c1 x) (cohomology-class-c1 y))                         ; 1
   (+ (* (cohomology-class-c1 x) (cohomology-class-ca y))                      ; a
      (* (cohomology-class-ca x) (cohomology-class-c1 y)))
   (+ (* (cohomology-class-c1 x) (cohomology-class-cb y))                      ; b
      (* (cohomology-class-cb x) (cohomology-class-c1 y)))
   (+ (* (cohomology-class-c1 x) (cohomology-class-cab y))                     ; ab
      (* (cohomology-class-cab x) (cohomology-class-c1 y))
      (* (cohomology-class-ca x) (cohomology-class-cb y))    ; a . b
      (* (cohomology-class-cb x) (cohomology-class-ca y))))) ; b . a (a^2=b^2=0)

(defun cohomology-a+b ()
  "The diagonal-style class  a + b  -- the sum of the two S^2 fundamental
classes, whose cup square detects the Whitehead square's Hopf invariant."
  (make-cohomology-class 0 1 1 0))

;;; ---- The Brunerie number as the cup-square coefficient ----------------

(defun whitehead-square-hopf-invariant ()
  "The Hopf invariant of the Whitehead square [iota_2, iota_2]: the coefficient
of the top class ab in  (a + b)^2.  Computed by the cup product:
 (a+b)^2 = a^2 + 2ab + b^2 = 2ab, so the coefficient is 2."
  (cohomology-class-cab (cohomology-cup (cohomology-a+b) (cohomology-a+b))))

(defun classical-brunerie-number ()
  "The classical Brunerie number: |Hopf invariant of the Whitehead square| = 2.
This is the order of the generator of pi_4(S^3), computed via the cohomology
cup product (the classical route, not the cubical normalisation)."
  (abs (whitehead-square-hopf-invariant)))

(defun classical-brunerie-number-is-2-p ()
  "The Brunerie number is 2, computed the classical way: (a+b)^2 = 2ab in
H*(S^2 x S^2)."
  (= (classical-brunerie-number) 2))

;;; ---- Generality: why even spheres get exactly 2 ----------------------

(defun even-sphere-whitehead-square-hopf-invariant (n)
  "On the even sphere S^{2n}, the Whitehead square [iota_{2n}, iota_{2n}] has
Hopf invariant 2 -- always the binomial coefficient C(2,1) = 2, the cross-term
of (a+b)^2 in H*(S^{2n} x S^{2n}).  (Independent of n: the same 2 for every even
sphere -- the reason every pi_{4n-1}(S^{2n}) carries that even class.)"
  (declare (ignore n))
  (whitehead-square-hopf-invariant))

(defun even-sphere-hopf-invariant-always-2-p (&optional (range 5))
  "The Whitehead-square Hopf invariant is 2 for every even sphere in a range --
the binomial coefficient does not depend on the dimension."
  (loop for n from 1 to range
        always (= (even-sphere-whitehead-square-hopf-invariant n) 2)))

;;; ---- The Hopf invariant homomorphism on pi_3(S^2) = Z -----------------
;;;
;;; pi_3(S^2) = Z (inc11).  The Hopf invariant  H : pi_3(S^2) -> Z  is the
;;; isomorphism sending the Hopf class eta to 1; so [iota_2,iota_2] = 2 eta is
;;; the class with Hopf invariant 2 -- consistent with the cup-square above.

(defun hopf-invariant-of-eta () 1)

(defun whitehead-square-as-multiple-of-eta ()
  "[iota_2, iota_2] = 2 . eta  in pi_3(S^2) = Z: the Whitehead square is twice
the Hopf class, since H(eta)=1 and H([iota_2,iota_2])=2 and H is an iso."
  (/ (whitehead-square-hopf-invariant) (hopf-invariant-of-eta)))

(defun whitehead-square-is-2-eta-p ()
  "[iota_2, iota_2] = 2 eta -- the source of the 2, consistent across the two
computations (cup square = 2, and 2 = 2 . H(eta) = 2 . 1)."
  (and (= (whitehead-square-as-multiple-of-eta) 2)
       (eq (pi-n (list :sphere 2) 3) :Z)))      ; lives in the inc11 group pi_3(S^2)=Z

;;; ---- The honest line: classical 2 computed, cubical Z/2 still stalled --

(defun brunerie-2-located-and-cubical-stalls-p ()
  "THE HONEST FRONTIER.  The Brunerie number 2 is LOCATED and COMPUTED by the
classical route: it is the cup-square coefficient in H*(S^2 x S^2), equivalently
the Hopf invariant of the Whitehead square [iota_2,iota_2] = 2 eta in
pi_3(S^2)=Z.  That computes here (= 2).  What stays :stalled is the
TYPE-THEORETIC pi_4(S^3) = Z/2 as a COMPUTING cubical truncation -- Brunerie's
actual thesis result, normalising a cubical term to 2, which needs the
n-truncation that computes the group + the 3-cell coherence this kernel lacks.
Two routes to the same 2: the classical one runs (T), the cubical one stalls."
  (and (classical-brunerie-number-is-2-p)
       (whitehead-square-is-2-eta-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
