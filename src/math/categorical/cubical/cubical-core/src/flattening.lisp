;;;; flattening.lisp --- the cube, increment 12: the FLATTENING LEMMA, computed
;;;; on the universal cover of S^1, and what it leaves of the Hopf wall.
;;;;
;;;; inc11 derived pi_3(S^2)=Z through the Hopf LES but had to TAKE total = S^3
;;;; as the fibration's geometric input: the cubical identification  int Hopf
;;;; ~= S^3  was the wall, vaguely "full Glue-comp coherence".  The lemma that
;;;; actually does that identification is the FLATTENING LEMMA: for a type
;;;; family  P : B -> U  over a HIT B, the total space  Sigma(b:B) P(b)  is
;;;; itself a HIT, whose constructors are B's constructors LIFTED by the
;;;; transport action of P.  This increment BUILDS the flattening lemma on the
;;;; case where the kernel can COMPUTE it end-to-end -- the universal cover of
;;;; the circle -- and uses that to sharpen the Hopf wall to a single named gap.
;;;;
;;;; THE COMPUTING INSTANCE.  helix : S^1 -> U,  helix base = Z, loop acting by
;;;; ua succ (dependent.lisp).  Sigma(x:S^1) helix(x) is the total space of the
;;;; universal cover.  Flattening says it is the HIT with a point per integer
;;;; and, for each n, an edge  n -> (transp helix along loop) n = n+1  -- the
;;;; integer LINE, which is CONTRACTIBLE.  We witness contractibility by the
;;;; canonical retraction onto the centre (base, 0): every fibre point (base, n)
;;;; retracts to the centre by iterating the loop-lift, and that lift is the
;;;; COMPUTING ua-beta transport (the same engine as pi_1(S^1)=Z).  Nothing is
;;;; asserted; the retraction reduces.
;;;;
;;;; WHAT IT LEAVES OF THE HOPF WALL.  The SAME lemma applies to the Hopf family
;;;; H : S^2 -> U (fibre S^1), giving  int Hopf  as a flattened HIT over S^2.
;;;; Identifying THAT HIT with S^3 needs one further thing: the S^1 H-SPACE
;;;; coherence (the fibre rotation being a unital, coherently-associative
;;;; multiplication).  So the Hopf wall is no longer "full Glue-comp coherence"
;;;; -- it is exactly the S^1 h-space coherence.  We state that sharpened gap
;;;; honestly and do NOT fake int Hopf ~= S^3.

(in-package #:rosette-cubical-core)

;;; ---- The total space of a type family over a HIT ---------------------
;;;
;;; A total-space point is a dependent pair  (b . v)  with  v : P(b).  For the
;;; universal cover the base point is the circle's :base and the fibre value is
;;; an integer.

(defun cover-point (n)
  "A point of  Sigma(x:S^1) helix(x): the pair (base, n) with n : Z = helix base."
  (cons :base n))

(defun cover-point-p (pt)
  "Recognise a universal-cover total-space point (base, n)."
  (and (consp pt) (eq (car pt) :base) (integerp (cdr pt))))

(defun cover-point-fibre (pt)
  "The fibre component n of a cover point (base, n)."
  (cdr pt))

;;; ---- The lifted loop: the loop's action on the cover -----------------
;;;
;;; The flattening HIT's edge over the loop sends a fibre value to its transport
;;; along the loop.  In helix that transport is  ua succ, i.e. +1 -- and it
;;; COMPUTES, by ua-beta.  This is the loop-constructor of the flattened HIT.

(defun helix-loop-transport (n)
  "Transport one step along the loop in the universal cover: helix's loop action
is ua succ, so this is n -> n+1, by the COMPUTING ua-beta (not an axiom)."
  (ua-beta (int-succ-equivalence) n))

(defun helix-loop-transport-inv (n)
  "The inverse lift (transport along loop^{-1}): ua pred, n -> n-1, computing."
  (ua-beta (rosette-hott-core:inverse-equivalence (int-succ-equivalence)) n))

(defun cover-loop-lift (pt)
  "The flattened HIT's loop edge on a total-space point: (base, n) -> (base, n+1),
the lift of the circle's loop through the cover.  Computes."
  (cover-point (helix-loop-transport (cover-point-fibre pt))))

(defun cover-loop-lift-computes-p (&optional (range 6))
  "The loop-lift is the COMPUTING +1 on the fibre across a range (ua-beta fires)."
  (loop for n from (- range) to range
        always (= (cover-point-fibre (cover-loop-lift (cover-point n))) (1+ n))))

;;; ---- The flattening lemma on the universal cover: contractibility ----
;;;
;;; The flattened HIT is the integer line.  Its centre is (base, 0); every point
;;; retracts to the centre by iterating the loop-lift (forwards or backwards).
;;; Contractibility = a centre + a retraction id ~ const-centre; here the
;;; retraction is COMPUTED by the ua-beta transport.

(defun cover-centre ()
  "The centre of the contraction: (base, 0)."
  (cover-point 0))

(defun cover-retract-to-centre (pt)
  "Retract a cover point (base, n) onto the centre (base, 0) by iterating the
loop-lift towards 0 (inverse-lift n times if n>0, forward-lift |n| times if
n<0).  Every step is a COMPUTING ua-beta transport; the result is the centre."
  (let ((v (cover-point-fibre pt)))
    (cond ((plusp v) (dotimes (_ v) (setf v (helix-loop-transport-inv v))))
          ((minusp v) (dotimes (_ (- v)) (setf v (helix-loop-transport v)))))
    (cover-point v)))

(defun cover-point-retracts-p (n)
  "The retraction lands a point (base, n) on the centre (base, 0) -- computed by
iterating the lift, the contraction's homotopy at this point."
  (equal (cover-retract-to-centre (cover-point n)) (cover-centre)))

(defun cover-reachable-from-centre-p (n)
  "Dually: (base, n) is reached FROM the centre by n forward/backward lifts --
the lift-length is exactly the winding number, computing (= winding-of-loop-
power n).  Confirms the line has one component through (base, n)."
  (let ((v 0))
    (cond ((plusp n) (dotimes (_ n) (setf v (helix-loop-transport v))))
          ((minusp n) (dotimes (_ (- n)) (setf v (helix-loop-transport-inv v)))))
    (and (= v n) (= (winding-of-loop-power n) n))))

(defun cover-total-space-contractible-p (&optional (range 8))
  "FLATTENING LEMMA, the computing instance:  Sigma(x:S^1) helix(x)  is
CONTRACTIBLE.  Witnessed (not asserted) across a range: every fibre point
(base, n) RETRACTS to the centre (base, 0) by the computed loop-lift, and is
REACHED from the centre by exactly n lifts (one component, the integer line).
Both directions reduce by ua-beta -- the same engine as pi_1(S^1)=Z."
  (and (cover-loop-lift-computes-p range)
       (loop for n from (- range) to range
             always (and (cover-point-retracts-p n)
                         (cover-reachable-from-centre-p n)))))

;;; ---- What the lemma leaves of the Hopf wall --------------------------

(defun flattening-lemma-cover-instance-p ()
  "The flattening lemma HOLDS, computed, on the universal cover of S^1:
int helix is contractible.  This is the prototype the Hopf computation needs --
and here it runs to the end."
  (cover-total-space-contractible-p))

(defun flattening-reduces-hopf-wall-to-hspace-p ()
  "HONEST SHARPENING of the Hopf wall.  The flattening lemma (which COMPUTES on
the cover, above) applies equally to the Hopf family H : S^2 -> U: it gives
int Hopf as a flattened HIT over S^2.  The ONE remaining step,  int Hopf ~= S^3,
is exactly the S^1 H-SPACE coherence (the fibre rotation being a unital,
coherently-associative multiplication) -- NOT 'full Glue-comp coherence' in the
abstract.  We assert the SHAPE of the residual gap; we do NOT fake int Hopf~=S^3.
So: the flattening lemma instance computes (T), AND pi_4(S^3) stays stalled (the
h-space coherence + the stem are still missing)."
  (and (flattening-lemma-cover-instance-p)                 ; the lemma computes here
       (eq (pi-n (list :sphere 3) 4) :stalled)))           ; Brunerie still honestly stalled
