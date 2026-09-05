;;;; xform.lisp --- 2x3 affine algebra (associative), mirroring
;;;; rosette-rigid-pose compose/apply WITHOUT a 3D dep.
;;;;
;;;; A transform is (:mat A B C D E F) row-major:
;;;;   x' = A*x + B*y + E
;;;;   y' = C*x + D*y + F
;;;; The integer-translate path (:translate DX DY) is the ONLY one the
;;;; byte-exact executor and layout v1 emit; scale/rotate/mat live here as
;;;; future seams with associativity laws but are rounded at op emission
;;;; and OUT of the parity laws.

(in-package #:rosette-scene-ir)

(defun xform-identity ()
  "The identity affine."
  (list :mat 1 0 0 1 0 0))

(defun xform-translate (dx dy)
  "Pure translation by (DX DY).  On the integer path DX,DY stay integers."
  (list :mat 1 0 0 1 dx dy))

(defun xform-scale (sx sy)
  "Axis-aligned scale."
  (list :mat sx 0 0 sy 0 0))

(defun xform-rotate (theta)
  "Rotation by THETA radians CCW about the origin."
  (let ((c (cos theta)) (s (sin theta)))
    (list :mat c (- s) s c 0 0)))

(defun %mat (x)
  "Coerce a node xform form into 2x3 (values A B C D E F)."
  (ecase (car x)
    (:mat (destructuring-bind (a b c d e f) (cdr x) (values a b c d e f)))
    (:translate (destructuring-bind (dx dy) (cdr x)
                  (values 1 0 0 1 dx dy)))
    (:scale (destructuring-bind (sx sy) (cdr x) (values sx 0 0 sy 0 0)))
    (:rotate (destructuring-bind (th) (cdr x)
               (let ((c (cos th)) (s (sin th))) (values c (- s) s c 0 0))))
    (:compose (destructuring-bind (u v) (cdr x)
                (%mat (xform-compose u v))))))

(defun xform-compose (u v)
  "Compose U then V so that (xform-apply (xform-compose u v) p) ==
(xform-apply v (xform-apply u p)) -- i.e. U is applied first.  This makes
nested (transform u (transform v ...)) accumulate as (xform-compose u v)
down the tree.  Associative; xform-identity is a two-sided unit."
  (multiple-value-bind (a1 b1 c1 d1 e1 f1) (%mat u)
    (multiple-value-bind (a2 b2 c2 d2 e2 f2) (%mat v)
      ;; result = V . U  (apply U first, then V)
      (list :mat
            (+ (* a2 a1) (* b2 c1))
            (+ (* a2 b1) (* b2 d1))
            (+ (* c2 a1) (* d2 c1))
            (+ (* c2 b1) (* d2 d1))
            (+ (* a2 e1) (* b2 f1) e2)
            (+ (* c2 e1) (* d2 f1) f2)))))

(defun xform-apply (x px py)
  "Apply affine X to point (PX PY); returns (values X' Y')."
  (multiple-value-bind (a b c d e f) (%mat x)
    (values (+ (* a px) (* b py) e)
            (+ (* c px) (* d py) f))))

(defun %translate-delta (x)
  "If X is a pure integer translate, return (values DX DY); else NIL.
The executor's byte-exact fast path."
  (multiple-value-bind (a b c d e f) (%mat x)
    (when (and (eql a 1) (eql b 0) (eql c 0) (eql d 1)
               (integerp e) (integerp f))
      (values e f))))

;;; ---- clip-rect algebra ----

(defun clip-intersect (r1 r2)
  "Intersection of two (:rect X Y W H) or plain (X Y W H) rects, as a plain
list (X Y W H).  Empty intersection returns a zero-area rect (X Y 0 0)."
  (flet ((coords (r) (if (eq (car r) :rect) (cdr r) r)))
    (destructuring-bind (x1 y1 w1 h1) (coords r1)
      (destructuring-bind (x2 y2 w2 h2) (coords r2)
        (let* ((ax (max x1 x2)) (ay (max y1 y2))
               (bx (min (+ x1 w1) (+ x2 w2)))
               (by (min (+ y1 h1) (+ y2 h2)))
               (w (max 0 (- bx ax))) (h (max 0 (- by ay))))
          (list ax ay w h))))))
