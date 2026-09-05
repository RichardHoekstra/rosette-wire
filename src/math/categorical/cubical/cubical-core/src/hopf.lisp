;;;; hopf.lisp --- the cube, increment 11: the Hopf fibration S^1 -> S^3 -> S^2,
;;;; its long exact sequence, and pi_3(S^2) = Z -- the FIRST off-diagonal cell.
;;;;
;;;; Increment 10 filled the pi_n(S^k) table up to the diagonal and left the
;;;; STRICTLY-ABOVE-diagonal stems stalled.  This increment crosses the first of
;;;; them, pi_3(S^2) = Z (the Hopf map), the honest way: through the LONG EXACT
;;;; SEQUENCE of the Hopf fibration  S^1 -> S^3 -> S^2.
;;;;
;;;; The LES of a fibration  F -> E -> B  is
;;;;   ... -> pi_n(F) -> pi_n(E) -> pi_n(B) -> pi_{n-1}(F) -> ...
;;;; By exactness, the map  pi_n(E) -> pi_n(B)  is an ISO whenever both
;;;; neighbouring fibre groups  pi_n(F)  and  pi_{n-1}(F)  vanish.  For the Hopf
;;;; fibration at n = 3:  pi_3(S^1) = 0  and  pi_2(S^1) = 0  (the circle is
;;;; aspherical -- COMPUTED, loop-space-class), so
;;;;   pi_3(S^3) ~= pi_3(S^2),  and  pi_3(S^3) = Z  (inc10 diagonal, DERIVED),
;;;; hence  pi_3(S^2) = Z.
;;;;
;;;; HONEST ENGINES.  The inputs are: the circle's vanishing groups (a genuine
;;;; computation), pi_3(S^3)=Z (an inc10 derivation), and the LES exactness (a
;;;; theorem we ENCODE and apply, like Freudenthal).  The geometric INPUT is the
;;;; Hopf fibration itself -- its total space being S^3.  Constructing  int Hopf
;;;; ~= S^3  CUBICALLY (the flattening lemma / full Glue-comp coherence) is the
;;;; WALL; we take total = S^3 as the fibration's definition and do NOT fake the
;;;; computed equivalence.  We DO exercise the local cubical content (the Hopf
;;;; type family over S^2 built by Glue, the fibre reducing, the surf action's
;;;; ua computing) as a mechanism witness, clearly labelled as such.
;;;;
;;;; pi_4(S^3) = Z/2 (Brunerie) STILL stalls: the Hopf LES at n=4 only RELATES it
;;;; to pi_4(S^2) (pi_4(S^3) ~= pi_4(S^2), since pi_4(S^1)=pi_3(S^1)=0), pinning
;;;; NEITHER -- an honest partial fact, not the value.

(in-package #:rosette-cubical-core)

;;; ---- The fibration  F -> E -> B  -------------------------------------

(defstruct (fibration (:constructor %make-fibration (name fibre total base)))
  "A fibration  FIBRE -> TOTAL -> BASE.  Each slot is a space descriptor the
loop-space machinery understands (:circle or (:sphere k))."
  (name  nil :read-only t)
  (fibre nil :read-only t)
  (total nil :read-only t)
  (base  nil :read-only t))

(defun make-hopf-fibration ()
  "The Hopf fibration  S^1 -> S^3 -> S^2.  We name the fibre :circle (so the
circle's COMPUTED aspherical groups feed the LES directly), the total space
S^3, and the base S^2.  total = S^3 is the fibration's geometric DEFINITION
 (constructing int Hopf ~= S^3 cubically is the wall, see file header)."
  (%make-fibration :hopf :circle (list :sphere 3) (list :sphere 2)))

;;; ---- The long exact sequence of a fibration --------------------------
;;;
;;;   ... -> pi_n(F) -> pi_n(E) -> pi_n(B) -> pi_{n-1}(F) -> ...
;;; We do not prove the LES; we encode its EXACTNESS and read off consequences,
;;; exactly as inc10 encoded the Freudenthal range.

(defun les-pi (space n)
  "pi_n(SPACE) as a group class for the LES, with pi_0 of a connected space
trivial (we only need n >= 1 here, plus the convention pi_0 = :trivial for the
path-connected fibre/total/base of the Hopf fibration)."
  (if (<= n 0) :trivial (pi-n space n)))

(defun les-mid-map-iso-p (fib n)
  "By exactness of  pi_n(F) -> pi_n(E) -> pi_n(B) -> pi_{n-1}(F), the middle map
pi_n(E) -> pi_n(B) is an ISOMORPHISM when both neighbouring fibre groups vanish:
pi_n(F) = 0  AND  pi_{n-1}(F) = 0.  (Then it is injective -- kernel = image of
pi_n(F)=0 -- and surjective -- cokernel maps into pi_{n-1}(F)=0.)"
  (let ((f (fibration-fibre fib)))
    (and (eq (les-pi f n) :trivial)
         (eq (les-pi f (1- n)) :trivial))))

(defun les-segment-exact-p (fib n &key (test #'eq))
  "A sanity witness that the kernel's group classes make the relevant LES
segment well-formed: each of pi_n(F), pi_n(E), pi_n(B), pi_{n-1}(F) is a
representable group class (not :stalled) so exactness can be applied at n."
  (declare (ignore test))
  (let ((f (fibration-fibre fib)) (e (fibration-total fib)) (b (fibration-base fib)))
    (and (not (eq (les-pi f n) :stalled))
         (not (eq (les-pi e n) :stalled))
         ;; pi_n(B) may be the very thing we are solving for; require only the
         ;; fibre/total side + the lower fibre group to be representable.
         (not (eq (les-pi f (1- n)) :stalled)))))

;;; ---- pi_3(S^2) = Z, derived through the Hopf LES ---------------------

(defun hopf-derived-pi3-base ()
  "DERIVE pi_3(S^2) through the Hopf LES, WITHOUT reading the (patched) table:
the middle map pi_3(S^3) -> pi_3(S^2) is an iso (les-mid-map-iso-p, since
pi_3(S^1)=pi_2(S^1)=0), so pi_3(S^2) equals pi_3(S^3) = pi_3(E).  Returns that
derived group class."
  (let ((fib (make-hopf-fibration)))
    (if (les-mid-map-iso-p fib 3)
        (les-pi (fibration-total fib) 3)        ; pi_3(S^3) = Z (inc10 diagonal)
        :stalled)))

(defun hopf-pi3-s2-is-Z-p ()
  "pi_3(S^2) = Z (the Hopf map), DERIVED.  The circle's pi_3 and pi_2 vanish
(computed), so the Hopf LES middle map pi_3(S^3) -> pi_3(S^2) is an iso, and
pi_3(S^3) = Z (inc10), giving pi_3(S^2) = Z.  We also check the patched table
agrees (consistency)."
  (let ((fib (make-hopf-fibration)))
    (and (les-segment-exact-p fib 3)
         (les-mid-map-iso-p fib 3)
         (eq (les-pi (fibration-total fib) 3) :Z)   ; pi_3(S^3)=Z is the source
         (eq (hopf-derived-pi3-base) :Z)            ; derivation yields Z
         (eq (pi-n (list :sphere 2) 3) :Z))))       ; table agrees

;;; ---- The local cubical content: the Hopf type family by Glue ---------
;;;
;;; The Hopf fibration is presented cubically as a type family  H : S^2 -> U
;;; with  H base = S^1 (the fibre) and the 2-cell surf acting on the fibre by a
;;; self-equivalence (the genuine S^1-rotation).  We EXERCISE that mechanism --
;;; a Glue line over a fibre self-equivalence whose transp computes by ua-beta --
;;; as a witness that the cubical machine handles the surf action.  We model the
;;; fibre automorphism with an available equivalence (Z-succ); the genuine
;;; S^1-rotation and the total-space equivalence int Hopf ~= S^3 are the wall.

(defun hopf-family-fibre-at-base-p ()
  "The Hopf family H : S^2 -> U has  H base = S^1 (the fibre).  Structural: the
fibration's fibre descriptor is :circle (= S^1)."
  (eq (fibration-fibre (make-hopf-fibration)) :circle))

(defun hopf-surf-action-ua-computes-p (&optional (range 4))
  "The surf 2-cell acts on the fibre by ua of a self-equivalence, and that ua
COMPUTES (ua-beta fires) -- the local cubical content of the Hopf family.  We
witness the mechanism with a concrete fibre automorphism (Z-succ as a stand-in
for the S^1-rotation): transp along its ua glue line reduces to the map itself
across a range."
  (let ((e (int-succ-equivalence)))
    (loop for k from (- range) to range
          always (= (ua-beta e k) (1+ k)))))

;;; ---- The wall, still located: pi_4(S^3) = Z/2 ------------------------

(defun hopf-les-relates-pi4-s3-s2-p ()
  "HONEST PARTIAL FACT about the Brunerie group.  The Hopf LES at n = 4,
  pi_4(S^1) -> pi_4(S^3) -> pi_4(S^2) -> pi_3(S^1),
has pi_4(S^1) = 0 and pi_3(S^1) = 0 (circle aspherical), so the middle map
pi_4(S^3) -> pi_4(S^2) is an ISO:  pi_4(S^3) ~= pi_4(S^2).  This RELATES the two
unknowns but pins NEITHER -- pi_4(S^3) = Z/2 stays stalled (the value needs the
stem, not the relation)."
  (let ((fib (make-hopf-fibration)))
    (and (les-mid-map-iso-p fib 4)                 ; pi_4(S^3) ~= pi_4(S^2)
         (eq (pi-n (list :sphere 3) 4) :stalled)   ; value of pi_4(S^3) still stalled
         (eq (pi-n (list :sphere 2) 4) :stalled)))) ; ... and pi_4(S^2) too
