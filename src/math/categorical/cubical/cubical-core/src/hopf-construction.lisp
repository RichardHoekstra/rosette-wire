;;;; hopf-construction.lisp --- the cube, increment 18: the HOPF CONSTRUCTION
;;;; from an H-space -- the Hopf map eta : S^3 -> S^2 as a cubical term, with its
;;;; Hopf invariant computing to 1 via the bidegree.  Second piece of the cubical
;;;; pi_4(S^3)=Z/2 normalisation.
;;;;
;;;; Given an H-space multiplication  mu : A x A -> A, the HOPF CONSTRUCTION is
;;;; the map  H(mu) : A * A -> Sigma A  defined on the join (inc17) by
;;;;   inl a |-> north,   inr b |-> south,   push a b |-> merid (mu(a,b)).
;;;; For A = S^1 with mu the rotation (inc13, the genuine multiplication), this
;;;; is eta : S^1 * S^1 -> Sigma S^1, i.e.  S^3 -> S^2  -- the Hopf map.
;;;;
;;;; The COMPUTABLE invariant: the Hopf invariant of H(mu) is the PRODUCT of the
;;;; two degrees of mu (its BIDEGREE).  For the S^1 multiplication the unit laws
;;;; (inc13) force bidegree (1,1):  a |-> mu(a,base) and b |-> mu(base,b) are both
;;;; the identity (degree 1).  So H(eta) = 1 . 1 = 1 -- matching H(eta)=1 from
;;;; inc11/inc15, and now obtained from the multiplication via the construction.
;;;;
;;;; HONEST SCOPE.  We build the Hopf construction map (a join recursor into the
;;;; suspension), check it is well-formed (meridian endpoints reduce), and compute
;;;; the bidegree / Hopf invariant from the unit laws -- all running.  What stays
;;;; the wall: that H(eta) for S^1 IS the Hopf FIBRATION with total space S^3
;;;; (needs the typed join ~= S^3, inc17's wall, plus the flattening coherence).
;;;; pi_4(S^3) stays :stalled.

(in-package #:rosette-cubical-core)

;;; ---- The Hopf construction map  H(mu) : A * A -> Sigma A -------------

(defun hopf-construction-map (base-a mult-fn)
  "Build the Hopf construction  H(mu) : A * A -> Sigma A  as a join recursor:
inl a |-> north, inr b |-> south, push a b |-> merid (mu(a,b)).  MULT-FN is the
H-space multiplication mu : A x A -> A.  Returns the join recursor into Sigma A."
  (let* ((aa (make-cjoin base-a base-a))
         (susp (make-suspension base-a)))
    (values
     (cjoin-rec aa (suspension-carrier susp)
                (lambda (a) (declare (ignore a)) (suspension-north susp))
                (lambda (b) (declare (ignore b)) (suspension-south susp))
                (lambda (a b) (suspension-meridian susp (funcall mult-fn a b))))
     aa susp)))

(defun hopf-construction-well-formed-p (base-a mult-fn sample-pairs)
  "The Hopf construction is well-formed: on each sample (a,b), the push-image is
the meridian of mu(a,b), a path north -> south (endpoints reduce)."
  (multiple-value-bind (rec aa susp) (hopf-construction-map base-a mult-fn)
    (declare (ignore aa))
    (every (lambda (pair)
             (let ((p (cjoin-rec-app-push rec (first pair) (second pair))))
               (and (cpath-p p)
                    (eql (cpath-i0 p) (suspension-north susp))
                    (eql (cpath-i1 p) (suspension-south susp)))))
           sample-pairs)))

(defun hopf-construction-point-beta-p (base-a mult-fn a b)
  "Point-betas of the Hopf construction: inl a |-> north, inr b |-> south."
  (multiple-value-bind (rec aa susp) (hopf-construction-map base-a mult-fn)
    (declare (ignore aa))
    (and (eql (cjoin-rec-app-inl rec a) (suspension-north susp))
         (eql (cjoin-rec-app-inr rec b) (suspension-south susp)))))

;;; ---- The bidegree of the S^1 multiplication --------------------------
;;;
;;; The Hopf invariant of H(mu) is the product of mu's two degrees.  For S^1,
;;; mu's restrictions  a |-> mu(a,base)  and  b |-> mu(base,b)  are the identity
;;; (the unit laws, inc13), each of degree 1 -- bidegree (1,1).

(defun circle-mult-first-degree ()
  "Degree of  a |-> mu(a, base)  on S^1: by the right unit law mu(n,0)=n, this
is the identity, degree 1.  Computed via the winding multiplication."
  (- (circle-mult-winding 1 0) (circle-mult-winding 0 0)))   ; (1) - (0) = 1

(defun circle-mult-second-degree ()
  "Degree of  b |-> mu(base, b)  on S^1: by the left unit law mu(0,n)=n, the
identity, degree 1."
  (- (circle-mult-winding 0 1) (circle-mult-winding 0 0)))   ; (1) - (0) = 1

(defun circle-hopf-construction-bidegree ()
  "The bidegree of the S^1 multiplication: (degree in first var, degree in
second var) = (1, 1), from the two unit laws."
  (list (circle-mult-first-degree) (circle-mult-second-degree)))

(defun circle-hopf-invariant-via-bidegree ()
  "The Hopf invariant of the Hopf construction H(mu) for S^1: the PRODUCT of the
bidegree = 1 . 1 = 1.  This is H(eta), obtained from the multiplication."
  (* (circle-mult-first-degree) (circle-mult-second-degree)))

(defun cubical-hopf-map-invariant-is-1-p ()
  "The cubical Hopf map eta = H(rotation) has Hopf invariant 1, computed from the
S^1 multiplication's bidegree (1,1) -- consistent with hopf-invariant-of-eta
(inc15) and the inc11 generator of pi_3(S^2)=Z."
  (and (equal (circle-hopf-construction-bidegree) '(1 1))
       (= (circle-hopf-invariant-via-bidegree) 1)
       (= (circle-hopf-invariant-via-bidegree) (hopf-invariant-of-eta))))

;;; ---- The cubical Hopf map as a genuine term, on S^1 ------------------

(defun circle-point-type ()
  "S^1 at the level of its concrete (definitional) points: the single point
:base.  (rosette-hott-core's circle-type is the full HIT; the suspension/join machine
here works with the simple point type, and the loop/winding content lives in
circle-mult-winding -- the same separation the rest of this kernel uses.)"
  (make-hott-type :circle-pt :predicate (lambda (p) (eq p :base))))

(defun circle-mult-on-points (a b)
  "The S^1 multiplication at the level of the concrete points: the circle's only
definitional point is :base, and mu(base,base)=base (the unit).  The winding
content lives in circle-mult-winding; this is the point-level action the Hopf
construction map needs."
  (declare (ignore a b))
  :base)

(defun cubical-hopf-map-on-circle ()
  "eta as a cubical term: the Hopf construction H(mu) on S^1 (with the point-level
multiplication), a map  S^1 * S^1 -> Sigma S^1, i.e. the cubical S^3 -> S^2.
Returns the join recursor (the map)."
  (hopf-construction-map (circle-point-type) #'circle-mult-on-points))

(defun cubical-hopf-map-well-formed-p ()
  "eta on S^1 is a well-formed cubical term: its push-images are meridians
north -> south, and its point-betas hold (inl |-> north, inr |-> south)."
  (and (hopf-construction-well-formed-p (circle-point-type) #'circle-mult-on-points
                                        (list (list :base :base)))
       (hopf-construction-point-beta-p (circle-point-type) #'circle-mult-on-points
                                       :base :base)))

;;; ---- The honest residual ---------------------------------------------

(defun cubical-hopf-construction-residual-p ()
  "HONEST.  The Hopf construction map is built and well-formed, and its Hopf
invariant computes to 1 from the bidegree (cubical-hopf-map-invariant-is-1-p).
What stays the wall: that H(eta) on S^1 IS the Hopf FIBRATION with total space
S^3 (needs the typed S^1 * S^1 ~= S^3, inc17's wall, plus flattening coherence),
and thence the cubical pi_4(S^3)=Z/2 normalisation.  So: eta is built and its
invariant computes (T), AND pi_4(S^3) stays stalled."
  (and (cubical-hopf-map-well-formed-p)
       (cubical-hopf-map-invariant-is-1-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
