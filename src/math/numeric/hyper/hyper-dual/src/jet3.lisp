(in-package #:rosette-hyper-dual)

;;;; Order-3 univariate jet numbers: f(x), f'(x), f''(x), f'''(x) in one
;;;; forward pass.
;;;;
;;;; Where hyper-dual handles k = 2 derivatives in 1-D, jet3 handles k = 3.
;;;; The same construction lifts mechanically to arbitrary order via Taylor
;;;; truncation; this file is the explicit k = 3 instantiation used for the
;;;; closed-form classes that require third-order sensitivities.
;;;;
;;;; Representation: a jet at point x_0 is the 4-tuple (a_0, a_1, a_2, a_3)
;;;; corresponding to the truncated Taylor series
;;;;     a_0 + a_1·ε + a_2·ε² + a_3·ε³,    ε^4 = 0
;;;; with a_n = f^(n)(x_0) / n! after evaluation.  The seed for
;;;; differentiation is (x_0, 1, 0, 0): then evaluating any cf-AST
;;;; recovers (f(x_0), f'(x_0), f''(x_0)/2, f'''(x_0)/6) componentwise,
;;;; with f^(n) extracted as n! · a_n.

(defstruct (jet3
            (:constructor %make-jet3)
            (:print-function
             (lambda (j s d)
               (declare (ignore d))
               (format s "#<jet3 ~,4F + ~,4F·ε + ~,4F·ε² + ~,4F·ε³>"
                       (jet3-a0 j) (jet3-a1 j) (jet3-a2 j) (jet3-a3 j)))))
  (a0 0d0 :type double-float)
  (a1 0d0 :type double-float)
  (a2 0d0 :type double-float)
  (a3 0d0 :type double-float))

(declaim (inline make-jet3 j3 jet3-from-real jet3-deriv-seed
                 jet3-value jet3-d1 jet3-d2 jet3-d3))

(defun make-jet3 (a0 a1 a2 a3)
  "Construct an order-3 jet a_0 + a_1·ε + a_2·ε² + a_3·ε³.
All arguments are coerced to DOUBLE-FLOAT."
  (%make-jet3 :a0 (as-f64 a0)
              :a1 (as-f64 a1)
              :a2 (as-f64 a2)
              :a3 (as-f64 a3)))

;; Upstream alias.
(defun j3 (a0 a1 a2 a3) (make-jet3 a0 a1 a2 a3))

(defun jet3-from-real (a)
  "Lift a real number to a jet with no derivative perturbation."
  (make-jet3 (as-f64 a) 0d0 0d0 0d0))

(defun jet3-deriv-seed (x)
  "Seed for differentiation at point X: jet representing x + ε."
  (make-jet3 (as-f64 x) 1d0 0d0 0d0))

(defun jet3-value (j) (jet3-a0 j))
(defun jet3-d1 (j) (jet3-a1 j))            ; f'(x_0) = 1!·a_1
(defun jet3-d2 (j) (* 2d0 (jet3-a2 j)))    ; f''(x_0) = 2!·a_2
(defun jet3-d3 (j) (* 6d0 (jet3-a3 j)))    ; f'''(x_0) = 3!·a_3

;;; --- arithmetic -----------------------------------------------------------

(defun jet3-add (x y)
  "Component-wise addition."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet3-from-real)
    (make-jet3 (+ (jet3-a0 x) (jet3-a0 y))
               (+ (jet3-a1 x) (jet3-a1 y))
               (+ (jet3-a2 x) (jet3-a2 y))
               (+ (jet3-a3 x) (jet3-a3 y)))))

(defun jet3-sub (x y)
  "Component-wise subtraction."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet3-from-real)
    (make-jet3 (- (jet3-a0 x) (jet3-a0 y))
               (- (jet3-a1 x) (jet3-a1 y))
               (- (jet3-a2 x) (jet3-a2 y))
               (- (jet3-a3 x) (jet3-a3 y)))))

(defun jet3-neg (x)
  "Component-wise negation."
  (cond
    ((numberp x) (jet3-neg (jet3-from-real x)))
    (t (make-jet3 (- (jet3-a0 x))
                  (- (jet3-a1 x))
                  (- (jet3-a2 x))
                  (- (jet3-a3 x))))))

(defun jet3-mul (x y)
  "Convolution: c_n = Σ_{i+j=n} a_i b_j."
  (multiple-value-bind (x y) (lift-real-binary-args x y #'jet3-from-real)
    (let ((a0 (jet3-a0 x)) (a1 (jet3-a1 x))
          (a2 (jet3-a2 x)) (a3 (jet3-a3 x))
          (b0 (jet3-a0 y)) (b1 (jet3-a1 y))
          (b2 (jet3-a2 y)) (b3 (jet3-a3 y)))
      (make-jet3 (* a0 b0)
                 (+ (* a0 b1) (* a1 b0))
                 (+ (* a0 b2) (* a1 b1) (* a2 b0))
                 (+ (* a0 b3) (* a1 b2) (* a2 b1) (* a3 b0))))))

(defun jet3-recip (x)
  "1/x via coefficient recurrence (solve y·x = 1)."
  (cond
    ((numberp x) (jet3-recip (jet3-from-real x)))
    (t (let ((a0 (jet3-a0 x)) (a1 (jet3-a1 x))
             (a2 (jet3-a2 x)) (a3 (jet3-a3 x)))
         (when (zerop a0)
           (error "jet3-recip: division by zero (a0 is 0)"))
         (let* ((c0 (/ 1d0 a0))
                (c1 (- (/ (* a1 c0) a0)))
                (c2 (- (/ (+ (* a2 c0) (* a1 c1)) a0)))
                (c3 (- (/ (+ (* a3 c0) (* a2 c1) (* a1 c2)) a0))))
           (make-jet3 c0 c1 c2 c3))))))

(defun jet3-div (x y)
  "Jet3 division: x / y = x · (1/y)."
  (jet3-mul x (jet3-recip y)))

;;; --- transcendental ops via Taylor recurrence ----------------------------
;;;
;;; For y = exp(x) we use y' = x'·y.  Convoluting term-by-term gives
;;;   (n+1)·c_{n+1} = Σ_{j=0}^{n} (j+1)·a_{j+1}·c_{n-j}
;;; which we expand explicitly for n = 0, 1, 2 below.

(defun jet3-exp (x)
  "exp(x) via the recurrence (n+1)·c_{n+1} = Σ (j+1)·a_{j+1}·c_{n-j}."
  (cond
    ((numberp x) (jet3-from-real (exp (as-f64 x))))
    (t (let* ((a0 (jet3-a0 x)) (a1 (jet3-a1 x))
              (a2 (jet3-a2 x)) (a3 (jet3-a3 x))
              (c0 (exp a0))
              (c1 (* c0 a1))
              (c2 (/ (+ (* 2d0 a2 c0) (* a1 c1)) 2d0))
              (c3 (/ (+ (* 3d0 a3 c0) (* 2d0 a2 c1) (* a1 c2)) 3d0)))
         (make-jet3 c0 c1 c2 c3)))))

(defun jet3-log (x)
  "log(x): solve y' = x'/x  ⇔  x·y' = x'."
  (cond
    ((numberp x) (jet3-from-real (log (as-f64 x))))
    (t (let ((a0 (jet3-a0 x)) (a1 (jet3-a1 x))
             (a2 (jet3-a2 x)) (a3 (jet3-a3 x)))
         (when (<= a0 0d0) (error "jet3-log: non-positive a0"))
         (let* ((c0 (log a0))
                ;; from a_0 c_1 = a_1
                (c1 (/ a1 a0))
                ;; from a_0 · 2 c_2 + a_1 c_1 = 2 a_2
                (c2 (/ (- (* 2d0 a2) (* a1 c1)) (* 2d0 a0)))
                ;; from a_0 · 3 c_3 + a_1 · 2 c_2 + a_2 c_1 = 3 a_3
                (c3 (/ (- (* 3d0 a3) (* 2d0 a1 c2) (* a2 c1)) (* 3d0 a0))))
           (make-jet3 c0 c1 c2 c3))))))

(defun jet3-sqrt (x)
  "sqrt(x): solve y² = x  ⇔  2·y·y' = x'."
  (cond
    ((numberp x) (jet3-from-real (sqrt (as-f64 x))))
    (t (let ((a0 (jet3-a0 x)) (a1 (jet3-a1 x))
             (a2 (jet3-a2 x)) (a3 (jet3-a3 x)))
         (when (<= a0 0d0) (error "jet3-sqrt: non-positive a0"))
         (let* ((c0 (sqrt a0))
                ;; 2·c_0·c_1 = a_1
                (c1 (/ a1 (* 2d0 c0)))
                ;; 2·c_0·c_2 + c_1² = a_2
                (c2 (/ (- a2 (* c1 c1)) (* 2d0 c0)))
                ;; 2·c_0·c_3 + 2·c_1·c_2 = a_3
                (c3 (/ (- a3 (* 2d0 c1 c2)) (* 2d0 c0))))
           (make-jet3 c0 c1 c2 c3))))))

;;; --- convenience ----------------------------------------------------------

(defun jet3-derivatives (f x)
  "Evaluate F (a Lisp function jet3 → jet3) at point X (a real number).
Returns (values f f' f'' f''')."
  (let ((j (funcall f (jet3-deriv-seed x))))
    (values (jet3-value j)
            (jet3-d1 j)
            (jet3-d2 j)
            (jet3-d3 j))))
