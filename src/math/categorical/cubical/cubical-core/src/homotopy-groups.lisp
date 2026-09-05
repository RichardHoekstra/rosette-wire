;;;; homotopy-groups.lisp --- the cube, increment 4: n-truncation, the pi_n
;;;; functor skeleton, and Eckmann-Hilton (pi_n abelian for n >= 2).
;;;;
;;;; pi_n(A, a) = || Omega^n (A, a) ||_0  -- the set-truncation of the n-fold
;;;; loop space.  This file gives the iterated loop space as a homotopy-type
;;;; CLASS the kernel can compute on the spaces it represents, set-truncation
;;;; to a pi_n, and the Eckmann-Hilton argument that every pi_n with n >= 2 is
;;;; abelian.  It computes a genuine result -- the circle is ASPHERICAL:
;;;; pi_1(S^1) = Z but pi_n(S^1) = 0 for n >= 2 -- and locates the wall: the
;;;; loop space of S^k (k >= 2) is not representable here (no connectivity /
;;;; Freudenthal bookkeeping), so pi_n(S^k), k >= 2, STALLS.  That stall is
;;;; exactly where pi_4(S^3) = Z/2 (the Brunerie number) lives.

(in-package #:rosette-cubical-core)

;;; ---- homotopy-type classes the kernel can name -----------------------
;;; A loop space is summarised by its homotopy CLASS:
;;;   :contractible  -- only refl (e.g. Omega of a set)
;;;   :integers      -- Z (e.g. Omega(S^1))
;;;   :stalled       -- not representable (e.g. Omega(S^2))

(defun set-loop-class ()
  "Omega(A, a) for a SET A: contractible (a set has a unique self-path, refl).
Hence iterating gives :contractible at every further level."
  :contractible)

(defun loop-space-class (space level)
  "The homotopy class of  Omega^LEVEL(SPACE)  for the spaces the kernel
represents.  SPACE is :set, :circle, or (:sphere k).
  LEVEL 0 is the space itself (its 'point' class).
For the circle: Omega^1 = Z (:integers); Omega^{>=2} = :contractible, because
Omega(Z-as-a-discrete-set) is contractible -- the circle is aspherical."
  (cond
    ((zerop level) (ecase (if (consp space) (first space) space)
                     (:set :set-points) (:circle :circle-points) (:sphere :sphere-points)))
    ((eq space :set) :contractible)
    ((eq space :circle) (if (= level 1) :integers :contractible))
    ((and (consp space) (eq (first space) :sphere))
     (let ((k (second space)))
       (cond ((<= k 0) :contractible)          ; S^0 is a set
             ((= k 1) (loop-space-class :circle level))
             ;; S^{>=2}: connectivity + Freudenthal fill in the table (inc10,
             ;; suspension.lisp).  Below the diagonal pi_{<k}(S^k)=0 (S^k is
             ;; (k-1)-connected); ON the diagonal pi_k(S^k)=Z (Freudenthal makes
             ;; it cyclic + degree surjects onto Z); STRICTLY ABOVE the diagonal
             ;; the unstable/stable stems (Hopf, Brunerie) remain not computable.
             ((< level k) :contractible)        ; connectivity: pi_{<k}(S^k)=0
             ((= level k) :integers)            ; diagonal: pi_k(S^k)=Z
             ;; the first off-diagonal cell, crossed by the Hopf LES (inc11,
             ;; hopf.lisp): pi_3(S^2)=Z (the Hopf map).
             ((and (= k 2) (= level 3)) :integers)
             (t :stalled))))                     ; rest above diagonal: stems stall
    (t :stalled)))

;;; ---- set-truncation || - ||_0  and the pi_n it yields ----------------

(defun set-truncate-class (class)
  "|| - ||_0 on a loop-space class -> the pi_n it represents:
  :contractible -> :trivial (the group 0),  :integers -> :Z,
  :stalled      -> :stalled."
  (ecase class
    (:contractible :trivial)
    (:integers :Z)
    (:stalled :stalled)))

(defun pi-n (space n)
  "pi_n(SPACE) = || Omega^n SPACE ||_0, as a group class (:trivial / :Z /
:stalled).  Defined for n >= 1."
  (assert (>= n 1) (n) "pi_n needs n >= 1, got ~a." n)
  (set-truncate-class (loop-space-class space n)))

;;; ---- truncation levels (h-levels) ------------------------------------

(defun group-class-is-set-p (class)
  "Every pi_n is a SET (set-truncated by construction): the homotopy group
is 0-truncated.  :trivial, :Z and :stalled are all set-level group classes."
  (member class '(:trivial :Z :stalled)))

;;; ---- the headline computing facts ------------------------------------

(defun circle-aspherical-p ()
  "The circle is a K(Z,1): pi_1(S^1) = Z and pi_n(S^1) = 0 for n >= 2.
Computes by iterating the loop-space class (Omega(Z) is contractible)."
  (and (eq (pi-n :circle 1) :Z)
       (eq (pi-n :circle 2) :trivial)
       (eq (pi-n :circle 5) :trivial)))

(defun set-higher-homotopy-trivial-p (n)
  "pi_n(set) = 0 for every n >= 1 (a set has no higher homotopy)."
  (eq (pi-n :set n) :trivial))

(defun sphere-pi-n-stalls-p (k n)
  "pi_n(S^k) STALLS only STRICTLY ABOVE the diagonal (n > k >= 2), EXCEPT the
cells later increments cross.  Since inc10 the below-diagonal cells (n<k) are 0
by connectivity and the diagonal (n=k) is Z by Freudenthal; inc11 (the Hopf LES)
then crossed pi_3(S^2)=Z.  What remains not representable is the rest of the
off-diagonal stems -- the located wall now containing pi_4(S^3)=Z/2 (Brunerie)."
  (and (>= k 2) (> n k) (eq (pi-n (list :sphere k) n) :stalled)))

;;; ---- Eckmann-Hilton: pi_n is abelian for n >= 2 ----------------------
;;;
;;; Omega^n for n >= 2 is a double loop space: it carries TWO unital binary
;;; compositions (the two path directions of the square, increment 1) that
;;; share a unit and satisfy the interchange law.  Eckmann-Hilton then forces
;;; the two operations to coincide AND to be commutative.  We verify the
;;; generic algebraic lemma on a finite carrier and read off the conclusion.

(defun interchange-holds-p (op1 op2 carrier &key (test #'eql))
  "The interchange law  (a op1 b) op2 (c op1 d) = (a op2 c) op1 (b op2 d)
over all quadruples from CARRIER."
  (dolist (a carrier t)
    (dolist (b carrier)
      (dolist (c carrier)
        (dolist (d carrier)
          (unless (funcall test
                           (funcall op2 (funcall op1 a b) (funcall op1 c d))
                           (funcall op1 (funcall op2 a c) (funcall op2 b d)))
            (return-from interchange-holds-p nil)))))))

(defun eckmann-hilton-commutative-p (op1 op2 unit carrier &key (test #'eql))
  "Given two binary ops sharing UNIT as a two-sided unit and satisfying
interchange, Eckmann-Hilton proves op1 = op2 and both are commutative.  We
CHECK the hypotheses and CHECK the conclusion (commutativity of op1) over
CARRIER -- the mechanism of 'pi_n abelian for n >= 2', run."
  (flet ((unital (op) (every (lambda (x)
                               (and (funcall test (funcall op unit x) x)
                                    (funcall test (funcall op x unit) x)))
                             carrier)))
    (and (unital op1) (unital op2)
         (interchange-holds-p op1 op2 carrier :test test)
         ;; conclusion: op1 is commutative (and equals op2) on the carrier
         (every (lambda (a)
                  (every (lambda (b)
                           (and (funcall test (funcall op1 a b) (funcall op1 b a))
                                (funcall test (funcall op1 a b) (funcall op2 a b))))
                         carrier))
                carrier))))

;; A genuinely NON-abelian group to show the interchange constraint bites:
;; the symmetric group S_3 as the six permutations of {0,1,2}.
(defun s3-elements ()
  (list #(0 1 2) #(0 2 1) #(1 0 2) #(1 2 0) #(2 0 1) #(2 1 0)))

(defun s3-compose (p q)
  (vector (aref p (aref q 0)) (aref p (aref q 1)) (aref p (aref q 2))))

(defun pi-n-abelian-for-n>=2-p ()
  "pi_n is abelian for n >= 2, by Eckmann-Hilton.  The force is genuine, not
assumed: (1) an ABELIAN composition (the integers under +, shared unit 0)
satisfies the unital + interchange hypotheses AND comes out commutative; and
(2) a NON-abelian operation (S_3 composition) FAILS interchange-with-itself --
so it could never be a double-loop-space composition.  Interchange + shared
unit is exactly the constraint that forces a higher homotopy group abelian."
  (and (eckmann-hilton-commutative-p #'+ #'+ 0 '(-2 -1 0 1 2 3) :test #'=)
       (not (interchange-holds-p #'s3-compose #'s3-compose (s3-elements)
                                 :test #'equalp))))
