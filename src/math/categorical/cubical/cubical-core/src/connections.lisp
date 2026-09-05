;;;; connections.lisp --- the cube, increment 6: connection squares.
;;;;
;;;; The interval is a De Morgan algebra (increment 0): it has connections,
;;;; the meet i /\ j and join i \/ j of two dimensions.  Feeding a connection
;;;; to a PATH turns the path into a SQUARE -- a CONNECTION SQUARE -- and these
;;;; are the canonical fillers that make the groupoid laws hold as genuine
;;;; 2-cells (increment 1 left the associator degenerate for want of them).
;;;;
;;;; For a path  p : a = b:
;;;;   the AND-connection  lambda i j. p(i /\ j)  has faces
;;;;       (i=0) refl a,  (j=0) refl a,  (i=1) p,  (j=1) p
;;;;     -- it fills the box exhibiting  refl . p ~ p  (left-unit as a 2-cell);
;;;;   the OR-connection   lambda i j. p(i \/ j)   has faces
;;;;       (i=1) refl b,  (j=1) refl b,  (i=0) p,  (j=0) p
;;;;     -- the box for  p . refl ~ p  (right-unit as a 2-cell).
;;;;
;;;; This closes the increment-1 gap that groupoid laws were endpoint-checked:
;;;; the unit laws now have an explicit connection-square witness, the De
;;;; Morgan structure of I doing the work.  THE WALL still ahead (toward
;;;; pi_4(S^3)=Z/2): connections used INSIDE comp for the Pi/Sigma/Path formers,
;;;; the Glue comp, and the higher hcomp filling S^2.

(in-package #:rosette-cubical-core)

(defun connection-and-square (path)
  "The connection square  lambda i j. PATH(i /\\ j), built from the interval
meet.  Faces: (i=0) and (j=0) are refl at PATH's start; (i=1) and (j=1) are
PATH itself.  Witnesses left-unit  refl . p ~ p  as a 2-cell."
  (make-csquare (cpath-type path)
                (lambda (i j) (path-app path (eval-interval (imeet i j) nil)))))

(defun connection-or-square (path)
  "The connection square  lambda i j. PATH(i \\/ j), built from the interval
join.  Faces: (i=1) and (j=1) are refl at PATH's end; (i=0) and (j=0) are
PATH itself.  Witnesses right-unit  p . refl ~ p  as a 2-cell."
  (make-csquare (cpath-type path)
                (lambda (i j) (path-app path (eval-interval (ijoin i j) nil)))))

(defun connection-and-left-unit-p (path &key (test #'eql))
  "Verify the AND-connection's faces: the (i=0)/(j=0) faces are constant at
PATH's start (the refl side) and the (i=1)/(j=1) faces reproduce PATH (its
endpoints b).  This is left-unit  refl . p ~ p  read off the square."
  (let* ((sq (connection-and-square path))
         (a (cpath-i0 path))
         (b (cpath-i1 path)))
    (and ;; i=0 face is constant a (refl at the start)
         (funcall test (cpath-i0 (csquare-face-r0 sq)) a)
         (funcall test (cpath-i1 (csquare-face-r0 sq)) a)
         ;; j=0 face is constant a too
         (funcall test (cpath-i0 (csquare-face-s0 sq)) a)
         (funcall test (cpath-i1 (csquare-face-s0 sq)) a)
         ;; i=1 face IS the path (endpoints a .. b)
         (funcall test (cpath-i0 (csquare-face-r1 sq)) a)
         (funcall test (cpath-i1 (csquare-face-r1 sq)) b)
         ;; j=1 face IS the path too
         (funcall test (cpath-i1 (csquare-face-s1 sq)) b)
         (square-boundary-coherent-p sq :test test))))

(defun connection-or-right-unit-p (path &key (test #'eql))
  "Verify the OR-connection's faces: the (i=1)/(j=1) faces are constant at
PATH's end (the refl side) and the (i=0)/(j=0) faces reproduce PATH.  This is
right-unit  p . refl ~ p  read off the square."
  (let* ((sq (connection-or-square path))
         (a (cpath-i0 path))
         (b (cpath-i1 path)))
    (and ;; i=1 face is constant b (refl at the end)
         (funcall test (cpath-i0 (csquare-face-r1 sq)) b)
         (funcall test (cpath-i1 (csquare-face-r1 sq)) b)
         ;; j=1 face is constant b too
         (funcall test (cpath-i0 (csquare-face-s1 sq)) b)
         (funcall test (cpath-i1 (csquare-face-s1 sq)) b)
         ;; i=0 face IS the path (a .. b)
         (funcall test (cpath-i0 (csquare-face-r0 sq)) a)
         (funcall test (cpath-i1 (csquare-face-r0 sq)) b)
         ;; j=0 face IS the path too
         (funcall test (cpath-i0 (csquare-face-s0 sq)) a)
         (square-boundary-coherent-p sq :test test))))
