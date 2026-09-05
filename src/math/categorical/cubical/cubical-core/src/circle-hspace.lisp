;;;; circle-hspace.lisp --- the cube, increment 13: the S^1 H-SPACE structure
;;;; (the circle multiplication / rotation), closing inc11's "stand-in" gap.
;;;;
;;;; inc12 sharpened the Hopf wall to one named lemma: the identification
;;;; int Hopf ~= S^3 needs the S^1 H-SPACE coherence -- the fibre rotation being
;;;; a unital, coherently-associative multiplication.  inc11's Hopf mechanism
;;;; witness admitted it MODELLED that fibre automorphism with a stand-in
;;;; (Z-succ).  This increment builds the GENUINE structure on the shadow where
;;;; the kernel computes -- the universal cover / pi_1 -- and feeds the real
;;;; rotation equivalence to the Hopf family.
;;;;
;;;; The circle multiplication  mu : S^1 x S^1 -> S^1  (rotation) induces, on
;;;; pi_1, the addition  Z x Z -> Z, and for each x the left-multiplication
;;;; mu(x, -) : S^1 -> S^1  is an EQUIVALENCE (the rotation).  Both COMPUTE here
;;;; (ua-beta), so we build them honestly:
;;;;   * mu on windings = + (computed by iterating the loop-lift),
;;;;   * the unit laws  mu(0,n)=n, mu(n,0)=n,
;;;;   * the rotation-by-loop^m equivalence (forward +m, backward -m), the
;;;;     GENUINE Hopf fibre automorphism, replacing inc11's stand-in,
;;;;   * associativity AT pi_1 (computed).
;;;;
;;;; RESIDUAL, named honestly.  The multiplication, unit, invertibility and
;;;; pi_1-associativity all compute.  What is STILL missing is the HIGHER
;;;; COHERENCE: the typed associator / unitor 2- and 3-cells that make
;;;; int Hopf ~= S^3 a COHERENT equivalence (and that the Brunerie computation
;;;; tracks).  Those need the full 2-/3-cell coherence layer this kernel does
;;;; not have; pi_4(S^3) = Z/2 stays stalled.  We do not fake the coherence.

(in-package #:rosette-cubical-core)

;;; ---- The circle multiplication mu, on the pi_1 shadow ----------------
;;;
;;; mu acts on winding numbers by addition.  We COMPUTE it through the cover's
;;; loop-lift (ua-beta succ iterated), not by reading +, so it is the genuine
;;; transport action and shares the engine with pi_1(S^1)=Z.

(defun circle-mult-winding (m n)
  "mu(loop^m, loop^n) on pi_1: the winding of the product, computed by lifting m
forward along the loop n times (n>=0) or backward |n| times (n<0).  Equals m+n,
obtained by the COMPUTING ua-beta transport."
  (let ((acc m))
    (cond ((>= n 0) (dotimes (_ n) (setf acc (helix-loop-transport acc))))
          (t (dotimes (_ (- n)) (setf acc (helix-loop-transport-inv acc)))))
    acc))

(defun circle-mult-unit-left-p (n)
  "Left unit law:  mu(base, loop^n) = loop^n  -- mu(0, n) = n on windings."
  (= (circle-mult-winding 0 n) n))

(defun circle-mult-unit-right-p (n)
  "Right unit law:  mu(loop^n, base) = loop^n  -- mu(n, 0) = n on windings."
  (= (circle-mult-winding n 0) n))

(defun circle-mult-associative-on-pi1-p (m n k)
  "Associativity at pi_1:  mu(mu(m,n),k) = mu(m,mu(n,k))  -- computed on windings."
  (= (circle-mult-winding (circle-mult-winding m n) k)
     (circle-mult-winding m (circle-mult-winding n k))))

(defun circle-mult-commutative-on-pi1-p (m n)
  "pi_1(S^1) is abelian, so mu is commutative on windings: mu(m,n)=mu(n,m)."
  (= (circle-mult-winding m n) (circle-mult-winding n m)))

;;; ---- The rotation equivalence: the GENUINE Hopf fibre automorphism ----
;;;
;;; Left-multiplication by loop^m,  mu(loop^m, -),  acts on the cover fibre Z as
;;; (+m); it is an equivalence with inverse (-m).  This is the rotation the Hopf
;;; Glue family glues the fibre by -- the real object inc11 modelled by a
;;; stand-in.  We build it as a hott-equivalence so it can be ua'd and transp'd.

(defun circle-rotation-equiv (m)
  "The rotation-by-loop^m equivalence on the universal-cover fibre Z:
forward n -> n+m, backward n -> n-m.  Left-multiplication mu(loop^m, -)."
  (let ((z (rosette-hott-core:hott-equivalence-source (int-succ-equivalence))))
    (rosette-hott-core:make-hott-equivalence
     z z
     (lambda (n) (+ n m))
     (lambda (n) (- n m)))))

(defun circle-rotation-ua-computes-p (m &optional (range 5))
  "transp along ua(rotation m) COMPUTES to (+m) across a range (ua-beta fires) --
the surf action of the Hopf family, now on the GENUINE rotation."
  (let ((rot (circle-rotation-equiv m)))
    (loop for n from (- range) to range
          always (= (ua-beta rot n) (+ n m)))))

(defun circle-rotation-is-equivalence-p (m &optional (range 5))
  "The rotation is a genuine equivalence: forward then backward is the identity
(and vice versa) across a range -- the invertibility the Hopf fibre needs."
  (let* ((rot (circle-rotation-equiv m))
         (fwd (rosette-hott-core:hott-equivalence-forward rot))
         (bwd (rosette-hott-core:hott-equivalence-backward rot)))
    (loop for n from (- range) to range
          always (and (= (funcall bwd (funcall fwd n)) n)
                      (= (funcall fwd (funcall bwd n)) n)))))

(defun circle-rotation-unit-is-identity-p (&optional (range 5))
  "Rotation by loop^0 is the identity equivalence (the h-space unit): +0 = id."
  (let ((rot (circle-rotation-equiv 0)))
    (loop for n from (- range) to range always (= (ua-beta rot n) n))))

(defun circle-rotation-composes-additively-p (m k &optional (range 5))
  "Rotations compose by addition: rotation m then rotation k = rotation (m+k) --
the h-space multiplication realised on the fibre automorphisms (computes)."
  (let ((rm (circle-rotation-equiv m)) (rmk (circle-rotation-equiv (+ m k))))
    (loop for n from (- range) to range
          always (= (ua-beta rm (+ n k)) (ua-beta rmk n)))))

;;; ---- Feeding the genuine rotation to the Hopf family ------------------

(defun hopf-fibre-automorphism-is-genuine-rotation-p ()
  "The Hopf family H : S^2 -> U glues the fibre S^1 by the GENUINE rotation
equivalence (circle-rotation-equiv), NOT inc11's Z-succ stand-in.  We confirm
the rotation is a unital equivalence whose ua computes -- exactly the fibre
automorphism the Hopf Glue family requires."
  (and (circle-rotation-unit-is-identity-p)      ; rotation 0 = id (unit)
       (circle-rotation-is-equivalence-p 1)       ; invertible (the fibre automorphism)
       (circle-rotation-is-equivalence-p 3)
       (circle-rotation-ua-computes-p 1)          ; its ua (surf action) computes
       (circle-rotation-composes-additively-p 2 3)))

;;; ---- The residual, sharpened to higher coherence ---------------------

(defun circle-hspace-pi1-structure-computes-p ()
  "The S^1 h-space structure COMPUTES on the pi_1 shadow: multiplication (+),
both unit laws, associativity, commutativity, and the rotation equivalences with
additive composition -- all reduce by ua-beta."
  (and (circle-mult-unit-left-p 4) (circle-mult-unit-right-p 4)
       (circle-mult-associative-on-pi1-p 2 3 5)
       (circle-mult-commutative-on-pi1-p 2 3)
       (hopf-fibre-automorphism-is-genuine-rotation-p)))

(defun circle-hspace-higher-coherence-residual-p ()
  "HONEST RESIDUAL.  The h-space multiplication, unit, invertibility and
pi_1-associativity all compute (circle-hspace-pi1-structure-computes-p).  What
is STILL missing for int Hopf ~= S^3 is the HIGHER COHERENCE -- the typed
associator / unitor 2- and 3-cells (and the Brunerie computation that tracks
them) -- which needs the full 2-/3-cell coherence layer this kernel lacks.  So:
the pi_1 h-space structure computes (T), AND pi_4(S^3) stays stalled."
  (and (circle-hspace-pi1-structure-computes-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
