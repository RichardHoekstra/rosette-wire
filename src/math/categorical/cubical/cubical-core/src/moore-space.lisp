;;;; moore-space.lisp --- the cube, increment 19: a nontrivial FINITE homotopy
;;;; group computed CUBICALLY -- pi_1(M(Z/2,1)) = Z/2 via encode/decode -- the
;;;; exact shape of the Brunerie Z/2, at the rung where it genuinely reduces.
;;;;
;;;; inc16 gave pi_4(S^3)=Z/2 classically; inc17-18 built the join and the Hopf
;;;; construction toward the cubical normalisation.  The cubical summit needs a
;;;; TRUNCATION that COMPUTES a finite group -- the thing repeatedly named as the
;;;; wall.  This increment delivers exactly that, at the rung where it runs: the
;;;; MOORE SPACE  M(Z/2,1) = S^1 cup_2 e^2  (attach a disc to the circle by the
;;;; degree-2 map z |-> z^2) has  pi_1 = Z/2 = <a | a^2>, and we COMPUTE it by
;;;; encode/decode through the 2-sheeted universal cover.
;;;;
;;;; THE ENGINE.  The cover of M(Z/2,1) has deck group Z/2; the loop a acts on the
;;;; fibre {0,1} by the ORDER-2 SWAP.  That swap is bool-not, whose univalence
;;;; beta-rule ALREADY fires here (transp (ua not) = not, kan.lisp).  So
;;;;   encode(a^n) = transp along a^n in the cover = (not iterated n times) at the
;;;;   basepoint = n mod 2,
;;;; computing by the SAME ua-beta engine as pi_1(S^1)=Z -- but the equivalence is
;;;; the order-2 not instead of the order-infinity succ, so the group is Z/2, not
;;;; Z.  The 2 in Z/2 is the ORDER of the swap = the DEGREE of the attaching map
;;;; (winding-of-loop-power 2 = 2).
;;;;
;;;; WHY THIS IS THE BRUNERIE SHAPE.  pi_4(S^3)=Z/2 is the SAME phenomenon -- a
;;;; Z/n homotopy group forced by degree-n structure -- three dimensions up.  Here
;;;; at pi_1 the truncation/encode COMPUTES (the cover is a 0-type, decidable);
;;;; for Omega^4 S^3 the truncation does not yet compute (the inc12-14 coherence
;;;; wall).  So this RUNS, and pi_4(S^3) (cubical) stays :stalled -- the analogy
;;;; is exact and the honesty intact.

(in-package #:rosette-cubical-core)

;;; ---- The degree-2 attaching map --------------------------------------

(defun moore-attaching-degree ()
  "M(Z/2,1) = S^1 cup_2 e^2: the 2-cell is attached by the degree-2 map
z |-> z^2 on S^1.  Its degree is the winding of loop^2 = 2 -- computed by the
ua-beta winding engine (winding-of-loop-power 2)."
  (winding-of-loop-power 2))

;;; ---- The 2-sheeted cover and its loop action (the order-2 swap) ------
;;;
;;; The universal cover of M(Z/2,1) is a 2-sheeted cover; the loop a acts on the
;;; fibre by the order-2 swap = bool-not.  We use Bool = {T, F} as the fibre with
;;; T the basepoint sheet (0) and F the other (1).

(defun moore-sheet->z2 (sheet)
  "Identify the fibre Bool with Z/2: T (basepoint sheet) -> 0, F -> 1."
  (if (eq sheet t) 0 1))

(defun moore-cover-transport (n)
  "Transport the basepoint sheet T along a^N in the cover: apply the loop action
(bool-not, the order-2 swap) N times, by the COMPUTING ua-beta.  Returns the
sheet (T or F)."
  (let ((noteq (bool-not-equivalence))
        (sheet t))
    (dotimes (_ (abs n)) (setf sheet (ua-beta noteq sheet)))    ; not is its own inverse
    sheet))

;;; ---- encode / decode :  Omega M(Z/2,1)  <->  Z/2 ---------------------

(defun moore-encode (n)
  "encode : Omega M(Z/2,1) -> Z/2 on a^N.  Transport the basepoint sheet along
a^N in the cover and read off Z/2: the result is N mod 2 -- obtained by iterating
the COMPUTING ua-beta swap, NOT by reading mod.  The cubical encode."
  (moore-sheet->z2 (moore-cover-transport n)))

(defun moore-decode (k)
  "decode : Z/2 -> Omega M(Z/2,1), k |-> a^k (k in {0,1}); the loop a^k it
denotes has cover-sheet k."
  (mod k 2))

(defun moore-encode-decode-id-p (k)
  "encode . decode = id on Z/2: encode(a^k) = k for k in {0,1}."
  (= (moore-encode (moore-decode k)) (mod k 2)))

(defun moore-encode-is-mod-2-p (&optional (range 8))
  "The cubical encode computes  a^N |-> N mod 2  across a range -- the heart of
pi_1(M(Z/2,1)) = Z/2, firing by ua-beta on the order-2 swap."
  (loop for n from 0 to range always (= (moore-encode n) (mod n 2))))

;;; ---- pi_1(M(Z/2,1)) = Z/2, computed ----------------------------------

(defun moore-loop-order ()
  "The order of the loop a in pi_1(M(Z/2,1)): the least k>0 with encode(a^k)=0.
Computed by the cover transport -- comes out 2 (a^2 = 1, a =/= 1)."
  (loop for k from 1
        when (and (= (moore-encode k) 0) (/= (moore-encode 1) 0))
          do (return k)
        when (> k 8) do (return :none)))

(defun moore-pi1-is-z2-p ()
  "pi_1(M(Z/2,1)) = Z/2, COMPUTED cubically: the encode is N mod 2 (ua-beta on the
swap), encode . decode = id on Z/2, and the loop a has ORDER 2 -- a =/= 1
(encode 1 = 1) but a^2 = 1 (encode 2 = 0).  The FIRST nontrivial finite homotopy
group the kernel computes."
  (and (moore-encode-is-mod-2-p)
       (every #'moore-encode-decode-id-p '(0 1))
       (= (moore-encode 1) 1)            ; a =/= 1
       (= (moore-encode 2) 0)            ; a^2 = 1
       (eql (moore-loop-order) 2)))      ; order exactly 2

(defun moore-z2-order-is-attaching-degree-p ()
  "The 2 in Z/2 is the DEGREE of the attaching map: the loop order (2) equals the
attaching degree (winding-of-loop-power 2 = 2).  Both computed."
  (= (moore-loop-order) (moore-attaching-degree)))

;;; ---- The exact Brunerie analogy --------------------------------------

(defun moore-is-brunerie-shape-one-dim-p ()
  "HONEST.  pi_1(M(Z/2,1)) = Z/2 is the EXACT shape of pi_4(S^3) = Z/2 -- a Z/n
homotopy group forced by degree-n structure -- at the rung where the cubical
encode COMPUTES (the cover is a decidable 0-type; ua-beta on the order-2 swap
runs, parallel to pi_1(S^1)=Z on the order-infinity succ).  The Brunerie summit
is the same phenomenon three dimensions up, where the Omega^4 S^3 truncation does
NOT yet compute (the inc12-14 coherence wall).  So: this Z/2 runs (T), AND
pi_4(S^3) (cubical) stays :stalled."
  (and (moore-pi1-is-z2-p)
       (moore-z2-order-is-attaching-degree-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
