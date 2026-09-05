;;;; pi4-s3.lisp --- the cube, increment 16: assembling the CLASSICAL group
;;;; pi_4(S^3) = Z/2 -- cyclic + order-2, from cup squares and classical
;;;; theorems -- while the CUBICAL normalisation stays honestly stalled.
;;;;
;;;; inc15 computed the Brunerie NUMBER (2 = the cup-square coefficient).  This
;;;; increment assembles the whole GROUP pi_4(S^3) = Z/2, classically, from three
;;;; pieces -- two computed cup squares and the derivations already in hand:
;;;;
;;;;   (1) CYCLIC.  inc10's Freudenthal edge surjection pi_3(S^2) ->> pi_4(S^3)
;;;;       makes pi_4(S^3) a quotient of Z -- cyclic.
;;;;   (2) ORDER DIVIDES 2.  The generator is eta_3 = Sigma^2 eta (the double
;;;;       suspension of the Hopf map).  Then
;;;;          2 eta_3 = Sigma^2 (2 eta) = Sigma^2 [iota_2, iota_2] = 0,
;;;;       because 2 eta = [iota_2, iota_2] (inc15, the cup-square Whitehead
;;;;       square) and WHITEHEAD PRODUCTS SUSPEND TO ZERO (Sigma[a,b]=0).  So
;;;;       the generator has order dividing 2.
;;;;   (3) NONTRIVIAL (order =/= 1).  eta is stably non-trivial, DETECTED by the
;;;;       Steenrod square Sq^2 on its cofibre CP^2 = S^2 cup_eta e^4:
;;;;          H*(CP^2) = Z[u]/u^3,   Sq^2(u) = u^2 = generator =/= 0,
;;;;       which is again a CUP SQUARE we compute.  (If eta were null, Sq^2 would
;;;;       vanish on the cofibre.)
;;;;   (1)+(2)+(3)  ==>  pi_4(S^3) = Z/2.
;;;;
;;;; THE HONEST LINE (unchanged and central).  This is the CLASSICAL computation
;;;; -- cohomology cup squares + Freudenthal + "Whitehead suspends to zero",
;;;; mathematics known since the 1950s.  It is NOT Brunerie's thesis result:
;;;; NORMALISING a specific cubical term for pi_4(S^3)=Z/2 INSIDE the type theory.
;;;; We do NOT touch the cubical loop-space table: pi-n (:sphere 3) 4 STAYS
;;;; :stalled -- the kernel still does not compute this group cubically.  Two
;;;; routes to the same Z/2; the classical one is assembled here, the cubical
;;;; normalisation remains the open frontier.  Nothing faked.

(in-package #:rosette-cubical-core)

;;; ---- (3) Nontriviality: H*(CP^2) = Z[u]/u^3 and Sq^2(u) = u^2 ---------
;;;
;;; CP^2 is the cofibre of the Hopf map eta : S^3 -> S^2.  Its cohomology ring is
;;; Z[u]/u^3 with u in degree 2 and u^2 the degree-4 generator.  Sq^2(u) = u^2
;;; (the cup square of a degree-2 class), and u^2 =/= 0 detects eta as non-null.

(defstruct (cp2-class (:constructor make-cp2-class (c0 cu cuu)))
  "An element of H*(CP^2) = Z[u]/u^3: coefficients of 1, u (deg 2), u^2 (deg 4)."
  (c0 0 :type integer :read-only t)
  (cu 0 :type integer :read-only t)
  (cuu 0 :type integer :read-only t))

(defun cp2-cup (x y)
  "The cup product on H*(CP^2): u . u = u^2, and anything of total degree > 4
vanishes (u^3 = 0)."
  (make-cp2-class
   (* (cp2-class-c0 x) (cp2-class-c0 y))                              ; 1
   (+ (* (cp2-class-c0 x) (cp2-class-cu y))                           ; u
      (* (cp2-class-cu x) (cp2-class-c0 y)))
   (+ (* (cp2-class-c0 x) (cp2-class-cuu y))                          ; u^2
      (* (cp2-class-cuu x) (cp2-class-c0 y))
      (* (cp2-class-cu x) (cp2-class-cu y)))))                        ; u . u

(defun cp2-sq2-of-u ()
  "Sq^2(u) = u^2 in H*(CP^2): the cup square of the degree-2 generator.  Its
coefficient of u^2 is 1 -- non-zero."
  (cp2-class-cuu (cp2-cup (make-cp2-class 0 1 0) (make-cp2-class 0 1 0))))

(defun eta-stably-nontrivial-p ()
  "eta is stably NON-trivial: Sq^2(u) = u^2 =/= 0 on the cofibre CP^2.  This is
the order =/= 1 input -- if eta were null, Sq^2 would vanish on the cofibre."
  (/= (cp2-sq2-of-u) 0))

;;; ---- (2) Order divides 2: Whitehead products suspend to zero ---------

(defun whitehead-product-suspends-to-zero-p ()
  "Classical theorem: the suspension of a Whitehead product is null,
Sigma[a,b] = 0.  (Encoded, as Freudenthal and the LES are encoded.)"
  t)

(defun pi4-s3-generator-order-divides-2-p ()
  "The generator eta_3 = Sigma^2 eta has order dividing 2:
   2 eta_3 = Sigma^2 (2 eta) = Sigma^2 [iota_2, iota_2] = 0,
using 2 eta = [iota_2, iota_2] (inc15, cup-square Whitehead square = 2) and
Sigma[a,b] = 0.  We check the two inputs."
  (and (= (whitehead-square-as-multiple-of-eta) 2)   ; 2 eta = [iota_2,iota_2] (inc15)
       (whitehead-product-suspends-to-zero-p)))        ; Sigma^2 of it vanishes

;;; ---- (1) Cyclic: Freudenthal edge surjection (inc10) -----------------

(defun pi4-s3-cyclic-p ()
  "pi_4(S^3) is cyclic: inc10's Freudenthal edge surjection pi_3(S^2) ->> pi_4(S^3)
exhibits it as a quotient of Z."
  (freudenthal-surj-p (sphere-connectivity 2) 3))      ; pi_3(S^2) ->> pi_4(S^3)

;;; ---- The classical assembly  pi_4(S^3) = Z/2 -------------------------

(defun pi4-s3-classical-order ()
  "The order of pi_4(S^3), classically: it is cyclic, the generator's order
divides 2 (Whitehead square suspends to 0), and is not 1 (Sq^2 =/= 0 on CP^2) --
so the order is exactly 2, the Brunerie number (inc15 cup square)."
  (if (and (pi4-s3-cyclic-p)
           (pi4-s3-generator-order-divides-2-p)
           (eta-stably-nontrivial-p))
      (classical-brunerie-number)                       ; = 2
      :undetermined))

(defun pi4-s3-is-Z-mod-2-classically-p ()
  "pi_4(S^3) = Z/2, CLASSICALLY assembled: cyclic (Freudenthal) + order dividing 2
(Whitehead square suspends to 0, with 2 eta = the cup-square 2) + non-trivial
(Sq^2(u)=u^2 =/= 0 on CP^2).  The order comes out 2."
  (and (pi4-s3-cyclic-p)
       (pi4-s3-generator-order-divides-2-p)
       (eta-stably-nontrivial-p)
       (eql (pi4-s3-classical-order) 2)))

(defun pi4-s3-classical-computed-cubical-stalls-p ()
  "THE HONEST FRONTIER, complete.  pi_4(S^3) = Z/2 is fully assembled by the
CLASSICAL route (cup squares + Freudenthal + Whitehead-suspends-to-zero --
1950s mathematics).  The CUBICAL normalisation (Brunerie's thesis: normalising a
type-theoretic term to 2) is NOT done: the cubical loop-space table is untouched,
pi-n (:sphere 3) 4 STAYS :stalled.  Two routes to the same Z/2 -- the classical
one runs here (T), the cubical one remains the open frontier."
  (and (pi4-s3-is-Z-mod-2-classically-p)               ; classical Z/2 assembled
       (eq (pi-n (list :sphere 3) 4) :stalled)))         ; cubical kernel still stalls
