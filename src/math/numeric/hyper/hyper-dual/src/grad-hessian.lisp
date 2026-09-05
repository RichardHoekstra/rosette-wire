(in-package #:rosette-hyper-dual)

;;;; Multivariate gradient and Hessian via hyper-dual seed batching.
;;;;
;;;; The hyper-dual ring carries exactly two nilpotent perturbation
;;;; directions (e1, e2).  Seeding coordinate i with e1 and coordinate j with
;;;; e2 and evaluating a scalar f : R^n -> R makes the e1e2 component of the
;;;; result exactly d2f/(dx_i dx_j) (and the e1 component df/dx_i).  So the full
;;;; symmetric Hessian is available in n(n+1)/2 forward passes -- one hyper-dual
;;;; evaluation per distinct entry -- with the gradient falling out of the n
;;;; diagonal passes.  This centralises the row-at-a-time pattern that
;;;; rosette-natural-gradient / rosette-information-geometry / rosette-hmc-inference /
;;;; rosette-ricci-flow / rosette-tov-interior each previously open-coded.
;;;;
;;;; For a single-pass alternative that also reaches mixed partials of any
;;;; order, see the multivariate jets in mvjet.lisp (mvjet-grad-hessian).

(defun %hd-inputs (x)
  "Simple-vector of HD-FROM-REAL lifts of the real sequence X."
  (let* ((n (length x))
         (v (make-array n)))
    (dotimes (k n v)
      (setf (aref v k) (hd-from-real (elt x k))))))

(defun hd-gradient (f x)
  "Exact gradient of a scalar F : R^n -> R at the real sequence X.
F receives a SIMPLE-VECTOR of N hyper-dual numbers (indexed by AREF) and
returns a hyper-dual.  Returns (values GRADIENT F-VALUE) where GRADIENT is a
\(SIMPLE-ARRAY DOUBLE-FLOAT (N)).  Uses N forward passes, one perturbed
coordinate each."
  (let* ((n (length x))
         (grad (make-array n :element-type 'double-float :initial-element 0d0))
         (val 0d0))
    (dotimes (i n (values grad val))
      (let ((inputs (%hd-inputs x)))
        (setf (aref inputs i) (make-hd (elt x i) 1d0 0d0 0d0))
        (let ((r (funcall f inputs)))
          (setf (aref grad i) (hd-d1 r))
          (when (zerop i) (setf val (hd-value r))))))))

(defun hd-grad-hessian (f x)
  "Exact gradient and full Hessian of a scalar F : R^n -> R at the real
sequence X.  F receives a SIMPLE-VECTOR of N hyper-dual numbers (indexed by
AREF) and returns a hyper-dual.  Returns

  (values GRADIENT HESSIAN F-VALUE)

with GRADIENT a (SIMPLE-ARRAY DOUBLE-FLOAT (N)), HESSIAN a symmetric
\(SIMPLE-ARRAY DOUBLE-FLOAT (N N)), and F-VALUE a DOUBLE-FLOAT.  Uses
N(N+1)/2 forward passes -- one hyper-dual evaluation per distinct Hessian
entry; the diagonal passes also yield the gradient."
  (let* ((n (length x))
         (grad (make-array n :element-type 'double-float :initial-element 0d0))
         (hess (make-array (list n n) :element-type 'double-float
                                      :initial-element 0d0))
         (val 0d0))
    (loop for i from 0 below n do
      (loop for j from i below n do
        (let ((inputs (%hd-inputs x)))
          (if (= i j)
              ;; Diagonal: seed coordinate i with both e1 and e2 (the
              ;; univariate second-derivative seed) -> e1 = df/dx_i,
              ;; e12 = d2f/dx_i^2.
              (setf (aref inputs i) (make-hd (elt x i) 1d0 1d0 0d0))
              ;; Off-diagonal: e1 on i, e2 on j -> e12 = d2f/(dx_i dx_j).
              (progn
                (setf (aref inputs i) (make-hd (elt x i) 1d0 0d0 0d0))
                (setf (aref inputs j) (make-hd (elt x j) 0d0 1d0 0d0))))
          (let ((r (funcall f inputs)))
            (when (and (zerop i) (zerop j)) (setf val (hd-value r)))
            (when (= i j) (setf (aref grad i) (hd-d1 r)))
            (let ((hij (hd-d12 r)))
              (setf (aref hess i j) hij
                    (aref hess j i) hij))))))
    (values grad hess val)))
