(in-package #:rosette-hyper-dual)

;;;; Order-4 univariate jet numbers: f(x), f'(x), f''(x), f'''(x), f''''(x)
;;;; in one forward pass.
;;;;
;;;; A jet4 at point x_0 is the 5-tuple (a_0, a_1, a_2, a_3, a_4) corresponding
;;;; to the truncated Taylor series
;;;;     a_0 + a_1·ε + a_2·ε² + a_3·ε³ + a_4·ε⁴,    ε^5 = 0
;;;; with a_n = f^(n)(x_0) / n! after evaluation.  The seed for differentiation
;;;; is (x_0, 1, 0, 0, 0): then evaluating any univariate AST recovers
;;;; (f, f'/1, f''/2, f'''/6, f''''/24) componentwise.  f^(n) extracted as
;;;; n! · a_n via JET4-D1 … JET4-D4.
;;;;
;;;; Construction is the standard Taylor-truncation lift of jet3; arithmetic
;;;; is one extra convolution coefficient; transcendentals use the same
;;;; coefficient recurrence (n+1) c_{n+1} = ... extended to n = 3.

(defstruct (jet4
            (:constructor %make-jet4)
            (:print-function
             (lambda (j s d)
               (declare (ignore d))
               (format s "#<jet4 ~,4F + ~,4F·ε + ~,4F·ε² + ~,4F·ε³ + ~,4F·ε⁴>"
                       (jet4-a0 j) (jet4-a1 j) (jet4-a2 j)
                       (jet4-a3 j) (jet4-a4 j)))))
  (a0 0d0 :type double-float)
  (a1 0d0 :type double-float)
  (a2 0d0 :type double-float)
  (a3 0d0 :type double-float)
  (a4 0d0 :type double-float))

(declaim (inline make-jet4 j4 jet4-from-real jet4-deriv-seed
                 jet4-value jet4-d1 jet4-d2 jet4-d3 jet4-d4))

(defun make-jet4 (a0 a1 a2 a3 a4)
  "Construct an order-4 jet a_0 + a_1·ε + a_2·ε² + a_3·ε³ + a_4·ε⁴.
All arguments are coerced to DOUBLE-FLOAT."
  (%make-jet4 :a0 (as-f64 a0)
              :a1 (as-f64 a1)
              :a2 (as-f64 a2)
              :a3 (as-f64 a3)
              :a4 (as-f64 a4)))

(defun j4 (a0 a1 a2 a3 a4) (make-jet4 a0 a1 a2 a3 a4))

(defun jet4-from-real (a)
  "Lift a real number to a jet with no derivative perturbation."
  (make-jet4 (as-f64 a) 0d0 0d0 0d0 0d0))

(defun jet4-deriv-seed (x)
  "Seed for differentiation at point X: jet representing x + ε."
  (make-jet4 (as-f64 x) 1d0 0d0 0d0 0d0))

(defun jet4-value (j) (jet4-a0 j))
(defun jet4-d1 (j) (jet4-a1 j))               ; f'(x_0)     = 1!·a_1
(defun jet4-d2 (j) (* 2d0 (jet4-a2 j)))       ; f''(x_0)    = 2!·a_2
(defun jet4-d3 (j) (* 6d0 (jet4-a3 j)))       ; f'''(x_0)   = 3!·a_3
(defun jet4-d4 (j) (* 24d0 (jet4-a4 j)))      ; f''''(x_0)  = 4!·a_4

;;; --- arithmetic -----------------------------------------------------------

(defun jet4-add (x y)
  "Component-wise addition."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet4-from-real)
    (make-jet4 (+ (jet4-a0 x) (jet4-a0 y))
               (+ (jet4-a1 x) (jet4-a1 y))
               (+ (jet4-a2 x) (jet4-a2 y))
               (+ (jet4-a3 x) (jet4-a3 y))
               (+ (jet4-a4 x) (jet4-a4 y)))))

(defun jet4-sub (x y)
  "Component-wise subtraction."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet4-from-real)
    (make-jet4 (- (jet4-a0 x) (jet4-a0 y))
               (- (jet4-a1 x) (jet4-a1 y))
               (- (jet4-a2 x) (jet4-a2 y))
               (- (jet4-a3 x) (jet4-a3 y))
               (- (jet4-a4 x) (jet4-a4 y)))))

(defun jet4-neg (x)
  "Component-wise negation."
  (cond
    ((numberp x) (jet4-neg (jet4-from-real x)))
    (t (make-jet4 (- (jet4-a0 x))
                  (- (jet4-a1 x))
                  (- (jet4-a2 x))
                  (- (jet4-a3 x))
                  (- (jet4-a4 x))))))

(defun jet4-mul (x y)
  "Convolution: c_n = Σ_{i+j=n} a_i b_j  for n = 0..4."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet4-from-real)
    (let ((a0 (jet4-a0 x)) (a1 (jet4-a1 x)) (a2 (jet4-a2 x))
          (a3 (jet4-a3 x)) (a4 (jet4-a4 x))
          (b0 (jet4-a0 y)) (b1 (jet4-a1 y)) (b2 (jet4-a2 y))
          (b3 (jet4-a3 y)) (b4 (jet4-a4 y)))
      (make-jet4 (* a0 b0)
                 (+ (* a0 b1) (* a1 b0))
                 (+ (* a0 b2) (* a1 b1) (* a2 b0))
                 (+ (* a0 b3) (* a1 b2) (* a2 b1) (* a3 b0))
                 (+ (* a0 b4) (* a1 b3) (* a2 b2) (* a3 b1) (* a4 b0))))))

(defun jet4-recip (x)
  "1/x via coefficient recurrence (solve y·x = 1)."
  (cond
    ((numberp x) (jet4-recip (jet4-from-real x)))
    (t (let ((a0 (jet4-a0 x)) (a1 (jet4-a1 x)) (a2 (jet4-a2 x))
             (a3 (jet4-a3 x)) (a4 (jet4-a4 x)))
         (when (zerop a0)
           (error "jet4-recip: division by zero (a0 is 0)"))
         (let* ((c0 (/ 1d0 a0))
                (c1 (- (/ (* a1 c0) a0)))
                (c2 (- (/ (+ (* a2 c0) (* a1 c1)) a0)))
                (c3 (- (/ (+ (* a3 c0) (* a2 c1) (* a1 c2)) a0)))
                (c4 (- (/ (+ (* a4 c0) (* a3 c1) (* a2 c2) (* a1 c3)) a0))))
           (make-jet4 c0 c1 c2 c3 c4))))))

(defun jet4-div (x y)
  "Jet4 division: x / y = x · (1/y)."
  (jet4-mul x (jet4-recip y)))

;;; --- transcendental ops via Taylor recurrence ----------------------------
;;;
;;; For y = exp(x) we use y' = x'·y → (n+1) c_{n+1} = Σ (j+1) a_{j+1} c_{n-j}.

(defun jet4-exp (x)
  "exp(x) via (n+1) c_{n+1} = Σ_{j=0}^{n} (j+1) a_{j+1} c_{n-j}, n = 0..3."
  (cond
    ((numberp x) (jet4-from-real (exp (as-f64 x))))
    (t (let* ((a0 (jet4-a0 x)) (a1 (jet4-a1 x)) (a2 (jet4-a2 x))
              (a3 (jet4-a3 x)) (a4 (jet4-a4 x))
              (c0 (exp a0))
              (c1 (* c0 a1))
              (c2 (/ (+ (* 2d0 a2 c0) (* a1 c1)) 2d0))
              (c3 (/ (+ (* 3d0 a3 c0) (* 2d0 a2 c1) (* a1 c2)) 3d0))
              (c4 (/ (+ (* 4d0 a4 c0) (* 3d0 a3 c1) (* 2d0 a2 c2) (* a1 c3))
                     4d0)))
         (make-jet4 c0 c1 c2 c3 c4)))))

(defun jet4-log (x)
  "log(x): solve x·y' = x'  ⇒  a_0·n·c_n + a_1·(n−1)·c_{n−1} + … = n·a_n."
  (cond
    ((numberp x) (jet4-from-real (log (as-f64 x))))
    (t (let ((a0 (jet4-a0 x)) (a1 (jet4-a1 x)) (a2 (jet4-a2 x))
             (a3 (jet4-a3 x)) (a4 (jet4-a4 x)))
         (when (<= a0 0d0) (error "jet4-log: non-positive a0"))
         (let* ((c0 (log a0))
                (c1 (/ a1 a0))
                (c2 (/ (- (* 2d0 a2) (* a1 c1)) (* 2d0 a0)))
                (c3 (/ (- (* 3d0 a3) (* 2d0 a1 c2) (* a2 c1)) (* 3d0 a0)))
                (c4 (/ (- (* 4d0 a4)
                          (* 3d0 a1 c3) (* 2d0 a2 c2) (* a3 c1))
                       (* 4d0 a0))))
           (make-jet4 c0 c1 c2 c3 c4))))))

(defun jet4-sqrt (x)
  "sqrt(x): solve y² = x via convolution recurrence."
  (cond
    ((numberp x) (jet4-from-real (sqrt (as-f64 x))))
    (t (let ((a0 (jet4-a0 x)) (a1 (jet4-a1 x)) (a2 (jet4-a2 x))
             (a3 (jet4-a3 x)) (a4 (jet4-a4 x)))
         (when (<= a0 0d0) (error "jet4-sqrt: non-positive a0"))
         (let* ((c0 (sqrt a0))
                (c1 (/ a1 (* 2d0 c0)))
                (c2 (/ (- a2 (* c1 c1)) (* 2d0 c0)))
                (c3 (/ (- a3 (* 2d0 c1 c2)) (* 2d0 c0)))
                (c4 (/ (- a4 (* 2d0 c1 c3) (* c2 c2)) (* 2d0 c0))))
           (make-jet4 c0 c1 c2 c3 c4))))))

;;; --- convenience ----------------------------------------------------------

(defun jet4-derivatives (f x)
  "Evaluate F (a Lisp function jet4 → jet4) at point X (a real number).
Returns (values f f' f'' f''' f'''')."
  (let ((j (funcall f (jet4-deriv-seed x))))
    (values (jet4-value j)
            (jet4-d1 j)
            (jet4-d2 j)
            (jet4-d3 j)
            (jet4-d4 j))))
