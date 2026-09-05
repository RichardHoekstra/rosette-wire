(in-package #:rosette-hyper-dual)

;;;; Arbitrary-order univariate jet numbers: f(x), f'(x), …, f^(n)(x) in one
;;;; forward pass, for any n chosen at runtime.
;;;;
;;;; A jetn of order n at point x_0 holds the coefficient vector
;;;; (a_0 a_1 … a_n) of the truncated Taylor series
;;;;     a_0 + a_1·ε + a_2·ε² + … + a_n·εⁿ,    ε^{n+1} = 0
;;;; with a_k = f^(k)(x_0) / k! after evaluation.  The differentiation seed
;;;; at x_0 is (x_0, 1, 0, …, 0); f^(k) is recovered as k!·a_k by JETN-DERIVATIVE.
;;;;
;;;; jet3 and jet4 are the hand-unrolled n = 3 and n = 4 special cases of this
;;;; carrier; jetn generalises them via the same Taylor recurrences run over a
;;;; coefficient vector instead of named slots:
;;;;   mul   — Cauchy product            c_k = Σ_{i=0}^k a_i b_{k-i}
;;;;   recip — solve c·a = 1             c_k = -(1/a_0) Σ_{i=1}^k a_i c_{k-i}
;;;;   exp   — c' = a'·c                 c_k = (1/k) Σ_{j=1}^k j·a_j·c_{k-j}
;;;;   log   — a·c' = a'                 c_k = (1/a_0)(a_k − (1/k)Σ_{j=1}^{k-1} j·c_j·a_{k-j})
;;;;   sqrt  — c² = a                    c_k = (1/2c_0)(a_k − Σ_{i=1}^{k-1} c_i c_{k-i})

(deftype jetn-coeffs () '(simple-array double-float (*)))

(defstruct (jetn
            (:constructor %make-jetn (coeffs))
            (:print-function
             (lambda (j s d)
               (declare (ignore d))
               (let ((v (jetn-coeffs j)))
                 (format s "#<jetn order ~D:" (1- (length v)))
                 (dotimes (k (length v))
                   (format s " ~,4F~:[~;·ε~]~:[~;^~D~]"
                           (aref v k) (plusp k) (> k 1) k))
                 (format s ">")))))
  (coeffs (make-array 1 :element-type 'double-float :initial-element 0d0)
          :type jetn-coeffs))

;;; --- constructors & accessors --------------------------------------------

(defun make-jetn (coeffs)
  "Build a jetn from a sequence COEFFS of Taylor coefficients (a_0 … a_n).
Order is (length COEFFS) − 1.  Elements are coerced to DOUBLE-FLOAT."
  (let* ((n (1- (length coeffs)))
         (v (make-array (1+ n) :element-type 'double-float)))
    (when (minusp n) (error "make-jetn: need at least one coefficient"))
    (map-into v #'as-f64 coeffs)
    (%make-jetn v)))

(defun jetn-order (j)
  "Truncation order n (so the carrier holds n+1 coefficients)."
  (1- (length (jetn-coeffs j))))

(declaim (inline jetn-coeff))
(defun jetn-coeff (j k)
  "Taylor coefficient a_k = f^(k)(x_0)/k!  (0 if k exceeds the order)."
  (let ((v (jetn-coeffs j)))
    (if (< k (length v)) (aref v k) 0d0)))

(defun jetn-from-real (a n)
  "Lift real A to an order-N jet with no derivative perturbation."
  (let ((v (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
    (setf (aref v 0) (as-f64 a))
    (%make-jetn v)))

(defun jetn-deriv-seed (x n)
  "Seed for differentiation at point X to order N: the jet x + ε."
  (let ((v (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
    (setf (aref v 0) (as-f64 x))
    (when (>= n 1) (setf (aref v 1) 1d0))
    (%make-jetn v)))

(defun jetn-value (j) "f(x_0) = a_0." (jetn-coeff j 0))

(defun jetn-derivative (j k)
  "f^(k)(x_0) = k! · a_k."
  (let ((fact (let ((f 1d0)) (loop for i from 2 to k do (setf f (* f i))) f)))
    (* fact (jetn-coeff j k))))

(defun jetn-derivatives (f x n)
  "Evaluate F (a jetn → jetn function) at real X to order N.
Returns a (simple-array double-float (n+1)) of (f, f', f'', …, f^(n))."
  (let* ((j (funcall f (jetn-deriv-seed x n)))
         (out (make-array (1+ n) :element-type 'double-float)))
    (dotimes (k (1+ n) out)
      (setf (aref out k) (jetn-derivative j k)))))

;;; --- order reconciliation ------------------------------------------------

(defun %jetn-order2 (x y)
  "Common order of two operands, at least one of which is a jetn."
  (cond ((and (jetn-p x) (jetn-p y)) (max (jetn-order x) (jetn-order y)))
        ((jetn-p x) (jetn-order x))
        ((jetn-p y) (jetn-order y))
        (t (error "%jetn-order2: neither argument is a jetn"))))

(defun %as-jetn (x n)
  "Coerce X to a jetn of order N (lifting reals; reusing jetns as-is)."
  (if (jetn-p x) x (jetn-from-real x n)))

;;; --- arithmetic ----------------------------------------------------------

(defun jetn-add (x y)
  "Component-wise addition (operands lifted to a common order)."
  (let* ((n (%jetn-order2 x y))
         (a (jetn-coeffs (%as-jetn x n))) (b (jetn-coeffs (%as-jetn y n)))
         (c (make-array (1+ n) :element-type 'double-float)))
    (dotimes (k (1+ n)) (setf (aref c k) (+ (aref a k) (aref b k))))
    (%make-jetn c)))

(defun jetn-sub (x y)
  "Component-wise subtraction."
  (let* ((n (%jetn-order2 x y))
         (a (jetn-coeffs (%as-jetn x n))) (b (jetn-coeffs (%as-jetn y n)))
         (c (make-array (1+ n) :element-type 'double-float)))
    (dotimes (k (1+ n)) (setf (aref c k) (- (aref a k) (aref b k))))
    (%make-jetn c)))

(defun jetn-neg (x)
  "Component-wise negation."
  (if (numberp x)
      (jetn-neg (jetn-from-real x 0))
      (let* ((n (jetn-order x)) (a (jetn-coeffs x))
             (c (make-array (1+ n) :element-type 'double-float)))
        (dotimes (k (1+ n)) (setf (aref c k) (- (aref a k))))
        (%make-jetn c))))

(defun jetn-scale (x s)
  "Multiply every coefficient by the real scalar S."
  (let* ((s (as-f64 s)) (n (jetn-order x)) (a (jetn-coeffs x))
         (c (make-array (1+ n) :element-type 'double-float)))
    (dotimes (k (1+ n)) (setf (aref c k) (* s (aref a k))))
    (%make-jetn c)))

(defun jetn-mul (x y)
  "Cauchy product: c_k = Σ_{i=0}^k a_i b_{k-i}, truncated at the common order."
  (let* ((n (%jetn-order2 x y))
         (a (jetn-coeffs (%as-jetn x n))) (b (jetn-coeffs (%as-jetn y n)))
         (c (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
    (dotimes (k (1+ n))
      (let ((s 0d0))
        (dotimes (i (1+ k)) (incf s (* (aref a i) (aref b (- k i)))))
        (setf (aref c k) s)))
    (%make-jetn c)))

(defun jetn-recip (x)
  "1/x via c·x = 1: c_0 = 1/a_0, c_k = -(1/a_0) Σ_{i=1}^k a_i c_{k-i}."
  (if (numberp x)
      (jetn-recip (jetn-from-real x 0))
      (let* ((n (jetn-order x)) (a (jetn-coeffs x))
             (c (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
        (when (zerop (aref a 0)) (error "jetn-recip: division by zero (a_0 is 0)"))
        (setf (aref c 0) (/ 1d0 (aref a 0)))
        (loop for k from 1 to n do
          (let ((s 0d0))
            (loop for i from 1 to k do (incf s (* (aref a i) (aref c (- k i)))))
            (setf (aref c k) (- (/ s (aref a 0))))))
        (%make-jetn c))))

(defun jetn-div (x y)
  "x / y = x · (1/y)."
  (jetn-mul x (jetn-recip y)))

;;; --- transcendental ops via Taylor recurrence ----------------------------

(defun jetn-exp (x)
  "exp(x): c_0 = exp(a_0), c_k = (1/k) Σ_{j=1}^k j·a_j·c_{k-j}."
  (if (numberp x)
      (jetn-from-real (exp (as-f64 x)) 0)
      (let* ((n (jetn-order x)) (a (jetn-coeffs x))
             (c (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
        (setf (aref c 0) (exp (aref a 0)))
        (loop for k from 1 to n do
          (let ((s 0d0))
            (loop for j from 1 to k do (incf s (* j (aref a j) (aref c (- k j)))))
            (setf (aref c k) (/ s k))))
        (%make-jetn c))))

(defun jetn-log (x)
  "log(x): c_0 = log(a_0), c_k = (1/a_0)(a_k − (1/k) Σ_{j=1}^{k-1} j·c_j·a_{k-j})."
  (if (numberp x)
      (jetn-from-real (log (as-f64 x)) 0)
      (let* ((n (jetn-order x)) (a (jetn-coeffs x))
             (c (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
        (when (<= (aref a 0) 0d0) (error "jetn-log: non-positive a_0"))
        (setf (aref c 0) (log (aref a 0)))
        (loop for k from 1 to n do
          (let ((s 0d0))
            (loop for j from 1 to (1- k) do (incf s (* j (aref c j) (aref a (- k j)))))
            (setf (aref c k) (/ (- (aref a k) (/ s k)) (aref a 0)))))
        (%make-jetn c))))

(defun jetn-sqrt (x)
  "sqrt(x): c_0 = sqrt(a_0), c_k = (1/2c_0)(a_k − Σ_{i=1}^{k-1} c_i c_{k-i})."
  (if (numberp x)
      (jetn-from-real (sqrt (as-f64 x)) 0)
      (let* ((n (jetn-order x)) (a (jetn-coeffs x))
             (c (make-array (1+ n) :element-type 'double-float :initial-element 0d0)))
        (when (<= (aref a 0) 0d0) (error "jetn-sqrt: non-positive a_0"))
        (setf (aref c 0) (sqrt (aref a 0)))
        (loop for k from 1 to n do
          (let ((s 0d0))
            (loop for i from 1 to (1- k) do (incf s (* (aref c i) (aref c (- k i)))))
            (setf (aref c k) (/ (- (aref a k) s) (* 2d0 (aref c 0))))))
        (%make-jetn c))))

(defun jetn-power (x p)
  "x^P for real exponent P via exp(P·log x); requires a_0 > 0."
  (if (numberp x)
      (jetn-from-real (expt (as-f64 x) (as-f64 p)) 0)
      (jetn-exp (jetn-scale (jetn-log x) p))))
