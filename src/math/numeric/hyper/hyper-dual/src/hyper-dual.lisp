(in-package #:rosette-hyper-dual)

;;;; Hyper-dual numbers for exact first- and second-order autodiff
;;;; (Fike & Alonso 2011 convention).
;;;;
;;;; A hyper-dual number is x = a + b·ε₁ + c·ε₂ + d·ε₁ε₂ with
;;;; ε₁² = ε₂² = 0 and ε₁·ε₂ kept as a separate basis element.
;;;; Evaluating a univariate function f at the seed
;;;;
;;;;     X = (x, 1, 1, 0)  =  x + ε₁ + ε₂ + 0·ε₁ε₂
;;;;
;;;; yields f(X) = (f(x), f'(x), f'(x), f''(x)) — exact first and second
;;;; derivatives with no truncation error and no separate derivative pass.
;;;;
;;;; This file is the extracted core of rosette-hyper-dual; consumers (rosette-cf-eval,
;;;; rosette-action-functional, rosette-schadner-iv, rosette-cech-sheaf) all build on it.

(defstruct (hyper-dual
            (:constructor %make-hyper-dual)
            (:print-function
             (lambda (h s d)
               (declare (ignore d))
               (format s "#<hd ~,4F + ~,4F·ε₁ + ~,4F·ε₂ + ~,4F·ε₁ε₂>"
                       (hyper-dual-real h)
                       (hyper-dual-e1 h)
                       (hyper-dual-e2 h)
                       (hyper-dual-e12 h)))))
  (real 0d0 :type double-float)
  (e1   0d0 :type double-float)
  (e2   0d0 :type double-float)
  (e12  0d0 :type double-float))

(declaim (inline make-hd hd-make hd-from-real hd-deriv-seed
                 hd-value hd-d1 hd-d2 hd-d12
                 hd-first-derivative hd-second-derivative))

(defun make-hd (real e1 e2 e12)
  "Construct a hyper-dual number a + b·ε₁ + c·ε₂ + d·ε₁ε₂.
All arguments are coerced to DOUBLE-FLOAT."
  (%make-hyper-dual :real (as-f64 real)
                    :e1   (as-f64 e1)
                    :e2   (as-f64 e2)
                    :e12  (as-f64 e12)))

;; Alias kept for compatibility with earlier callers.
(defun hd-make (real e1 e2 e12) (make-hd real e1 e2 e12))

(defun hd-from-real (a)
  "Lift a real number to a hyper-dual with no derivative perturbation."
  (make-hd (as-f64 a) 0d0 0d0 0d0))

(defun hd-deriv-seed (x)
  "Seed for univariate first+second derivative at point X.
After evaluation, real = f(x), e1 = e2 = f'(x), e12 = f''(x)."
  (make-hd (as-f64 x) 1d0 1d0 0d0))

(defun hd-value (h) (hyper-dual-real h))
(defun hd-d1    (h) (hyper-dual-e1 h))
(defun hd-d2    (h) (hyper-dual-e2 h))
(defun hd-d12   (h) (hyper-dual-e12 h))

;; Long-form aliases preserved.
(defun hd-first-derivative (h)  (hyper-dual-e1 h))
(defun hd-second-derivative (h) (hyper-dual-e12 h))

(defmacro with-hd-components ((a b c d h) &body body)
  "Bind A, B, C, D to H's real, e1, e2, and e12 components."
  `(let ((,a (hyper-dual-real ,h))
         (,b (hyper-dual-e1 ,h))
         (,c (hyper-dual-e2 ,h))
         (,d (hyper-dual-e12 ,h)))
     ,@body))

;;; --- arithmetic -----------------------------------------------------------

(defun hd-add (x y)
  "Hyper-dual addition.  Numeric arguments are auto-lifted via HD-FROM-REAL."
  (flet ((add-hd (x y)
           (declare (type hyper-dual x y))
           (make-hd (+ (hyper-dual-real x) (hyper-dual-real y))
                    (+ (hyper-dual-e1 x)   (hyper-dual-e1 y))
                    (+ (hyper-dual-e2 x)   (hyper-dual-e2 y))
                    (+ (hyper-dual-e12 x)  (hyper-dual-e12 y)))))
    (if (and (hyper-dual-p x) (hyper-dual-p y))
        (add-hd x y)
        (multiple-value-bind (x y) (lift-real-binary-args x y #'hd-from-real)
          (add-hd x y)))))

(defun hd-sub (x y)
  "Hyper-dual subtraction."
  (flet ((sub-hd (x y)
           (declare (type hyper-dual x y))
           (make-hd (- (hyper-dual-real x) (hyper-dual-real y))
                    (- (hyper-dual-e1 x)   (hyper-dual-e1 y))
                    (- (hyper-dual-e2 x)   (hyper-dual-e2 y))
                    (- (hyper-dual-e12 x)  (hyper-dual-e12 y)))))
    (if (and (hyper-dual-p x) (hyper-dual-p y))
        (sub-hd x y)
        (multiple-value-bind (x y) (lift-real-binary-args x y #'hd-from-real)
          (sub-hd x y)))))

(defun hd-neg (x)
  "Hyper-dual negation."
  (cond
    ((numberp x) (hd-neg (hd-from-real x)))
    (t (make-hd (- (hyper-dual-real x))
                (- (hyper-dual-e1 x))
                (- (hyper-dual-e2 x))
                (- (hyper-dual-e12 x))))))

(defun hd-mul (x y)
  "Hyper-dual product.  Extends ordinary multiplication with the mixed
ε₁·ε₂ cross-term that captures second derivatives."
  (flet ((mul-hd (x y)
           (declare (type hyper-dual x y))
           (with-hd-components (a b c d x)
             (with-hd-components (a* b* c* d* y)
               (make-hd (* a a*)
                        (+ (* a b*) (* a* b))
                        (+ (* a c*) (* a* c))
                        (+ (* a d*) (* a* d) (* b c*) (* b* c)))))))
    (if (and (hyper-dual-p x) (hyper-dual-p y))
        (mul-hd x y)
        (multiple-value-bind (x y) (lift-real-binary-args x y #'hd-from-real)
          (mul-hd x y)))))

(defun hd-recip (x)
  "1 / x.  Matches f(x) = 1/x, f'(x) = −1/x², f''(x) = 2/x³."
  (cond
    ((numberp x) (hd-recip (hd-from-real x)))
    (t (with-hd-components (a b c d x)
         (when (zerop a)
           (error "hd-recip: division by zero (real part is 0)"))
         (let* ((inv-a  (/ 1d0 a))
                (inv-a2 (* inv-a inv-a))
                (inv-a3 (* inv-a2 inv-a)))
           (make-hd inv-a
                    (- (* b inv-a2))
                    (- (* c inv-a2))
                    (+ (- (* d inv-a2))
                       (* 2d0 b c inv-a3))))))))

(defun hd-div (x y)
  "Hyper-dual division: x / y = x · (1/y)."
  (hd-mul x (hd-recip y)))

(defun hd-exp (x)
  "exp(a + b·ε₁ + c·ε₂ + d·ε₁ε₂)
   = exp(a)·(1 + b·ε₁ + c·ε₂ + (d + b·c)·ε₁ε₂)."
  (cond
    ((numberp x) (hd-from-real (exp (as-f64 x))))
    (t (with-hd-components (a b c d x)
         (let ((e (exp a)))
           (make-hd e
                    (* e b)
                    (* e c)
                    (* e (+ d (* b c)))))))))

(defun hd-log (x)
  "log(a + b·ε₁ + c·ε₂ + d·ε₁ε₂)
   = log(a) + (b/a)·ε₁ + (c/a)·ε₂ + (d/a − b·c/a²)·ε₁ε₂."
  (cond
    ((numberp x) (hd-from-real (log (as-f64 x))))
    (t (with-hd-components (a b c d x)
         (when (<= a 0d0) (error "hd-log: non-positive real part"))
         (let* ((inv-a  (/ 1d0 a))
                (inv-a2 (* inv-a inv-a)))
           (make-hd (log a)
                    (* b inv-a)
                    (* c inv-a)
                    (- (* d inv-a) (* b c inv-a2))))))))

(defun hd-sqrt (x)
  "sqrt(a + b·ε₁ + c·ε₂ + d·ε₁ε₂).  Matches f(x) = √x, f'(x) = 1/(2√x),
f''(x) = −1/(4·x^{3/2})."
  (cond
    ((numberp x) (hd-from-real (sqrt (as-f64 x))))
    (t (with-hd-components (a b c d x)
         (when (<= a 0d0) (error "hd-sqrt: non-positive real part"))
         (let* ((sa (sqrt a))
                (half-inv-sa (/ 0.5d0 sa))
                (quarter-inv-a-sa (/ 0.25d0 (* a sa))))
           (make-hd sa
                    (* b half-inv-sa)
                    (* c half-inv-sa)
                    (- (* d half-inv-sa)
                       (* b c quarter-inv-a-sa))))))))

(defun hd-power (x p)
  "Real power x^p for hyper-dual x and real exponent p.
Implemented as exp(p · log x); requires the real part of x to be > 0."
  (let ((p-real (if (numberp p) (as-f64 p)
                    (error "hd-power: exponent must be a real number, got ~A." p))))
    (cond
      ((numberp x) (hd-from-real (expt (as-f64 x) p-real)))
      (t (hd-exp (hd-mul p-real (hd-log x)))))))

;;; --- general univariate lift via supplied derivatives --------------------

(defun hd-univariate (f f-prime f-second x)
  "Apply a smooth univariate function F to hyper-dual X using callables
F-PRIME and F-SECOND that compute f'(a) and f''(a) at the real part.
Pattern: result = (f(a), f'(a)·b, f'(a)·c, f'(a)·d + f''(a)·b·c)."
  (cond
    ((numberp x)
     (hd-from-real (funcall f (as-f64 x))))
    (t
     (with-hd-components (a b c d x)
       (let ((fa (funcall f a))
             (fp (funcall f-prime a))
             (fpp (funcall f-second a)))
         (make-hd fa
                  (* fp b)
                  (* fp c)
                  (+ (* fp d) (* fpp b c))))))))

(defun hd-sin (x)
  "Sine lifted to a hyper-dual value.  Its first derivative is COS and its
second derivative is -SIN."
  (hd-univariate #'sin #'cos (lambda (a) (- (sin a))) x))

(defun hd-cos (x)
  "Cosine lifted to a hyper-dual value.  Its first derivative is -SIN and its
second derivative is -COS."
  (hd-univariate #'cos
                 (lambda (a) (- (sin a)))
                 (lambda (a) (- (cos a)))
                 x))

;;; --- analytic functions used by the closed-form class --------------------

(defun %normal-pdf-prime (x)
  (* (- x) (normal-pdf x)))

(defun %normal-pdf-second (x)
  (* (- (* x x) 1d0) (normal-pdf x)))

(defun hd-normal-pdf (x)
  "Standard normal density φ lifted to hyper-dual.
φ' = −x·φ, φ'' = (x²−1)·φ."
  (hd-univariate #'normal-pdf
                 #'%normal-pdf-prime
                 #'%normal-pdf-second
                 x))

(defun hd-normal-cdf (x)
  "Standard normal CDF Φ(x) lifted to hyper-dual.  Φ' = φ, φ' = −x·φ."
  (hd-univariate #'normal-cdf
                 #'normal-pdf
                 #'%normal-pdf-prime
                 x))

(defun hd-inverse-normal-cdf (p)
  "Standard normal quantile Φ⁻¹(p).  By the implicit function theorem:
y'(p) = 1/φ(y), y''(p) = y / φ(y)², where y = Φ⁻¹(p)."
  (hd-univariate #'inverse-normal-cdf
                 (lambda (a)
                   (let ((y (inverse-normal-cdf a)))
                     (/ 1d0 (normal-pdf y))))
                 (lambda (a)
                   (let* ((y (inverse-normal-cdf a))
                          (phi (normal-pdf y)))
                     (/ y (* phi phi))))
                 p))

;;; --- IG CDF partials and IG quantile lift --------------------------------

(defun %inverse-gaussian-cdf-terms (x mu lambda)
  (let* ((xd (as-f64 x))
         (mud (as-f64 mu))
         (lambdad (as-f64 lambda))
         (r (sqrt (/ lambdad xd)))
         (z1 (* r (- (/ xd mud) 1d0)))
         (z2 (* r (+ (/ xd mud) 1d0)))
         (exp-2l-mu (exp (/ (* 2d0 lambdad) mud))))
    (values xd mud lambdad r z1 z2 exp-2l-mu)))

(defun inverse-gaussian-cdf-partial-mu (x mu lambda)
  "Analytical ∂F_IG/∂μ evaluated at (x, μ, λ).
   F_IG(x; μ, λ) = Φ(z₁) + exp(2λ/μ)·Φ(−z₂),
   z₁ = r·(x/μ − 1),  z₂ = r·(x/μ + 1),  r = √(λ/x)."
  (multiple-value-bind (xd mud lambdad r z1 z2 exp-2l-mu)
      (%inverse-gaussian-cdf-terms x mu lambda)
    (let ((rx-over-mu2 (* r (/ xd (* mud mud)))))
      (+ (- (* rx-over-mu2 (normal-pdf z1)))
         (- (* (/ (* 2d0 lambdad) (* mud mud))
               exp-2l-mu (normal-cdf (- z2))))
         (* rx-over-mu2 exp-2l-mu (normal-pdf z2))))))

(defun inverse-gaussian-cdf-partial-lambda (x mu lambda)
  "Analytical ∂F_IG/∂λ evaluated at (x, μ, λ)."
  (multiple-value-bind (xd mud lambdad r z1 z2 exp-2l-mu)
      (%inverse-gaussian-cdf-terms x mu lambda)
    (declare (ignore xd r))
    (+ (* (/ z1 (* 2d0 lambdad)) (normal-pdf z1))
       (* (/ 2d0 mud) exp-2l-mu (normal-cdf (- z2)))
       (- (* (/ z2 (* 2d0 lambdad)) exp-2l-mu (normal-pdf z2))))))

(defun hd-inverse-gaussian-quantile (p-hd mu-hd lambda-hd)
  "First-order hyper-dual lift of inverse-gaussian-quantile.

Given hyper-dual P, MU, LAMBDA, returns hyper-dual Q satisfying
F_IG(real(Q); real(MU), real(LAMBDA)) = real(P) (real Newton convergence)
plus implicit-theorem first partials:
  dQ/dp = 1/f_IG(q),
  dQ/dμ = −F_μ(q)/f_IG(q),
  dQ/dλ = −F_λ(q)/f_IG(q).

The ε₁·ε₂ component captures first-order contributions from the
inputs' ε₁·ε₂ components (linear in the same partials) but does
NOT include the IG quantile's own second-derivative cross terms.
For uses where only first-order sensitivities are required this is
exact; full second-order Greeks through the IG quantile require an
extension that propagates the implicit second derivatives."
  (let* ((p-r (hyper-dual-real p-hd))
         (mu-r (hyper-dual-real mu-hd))
         (lambda-r (hyper-dual-real lambda-hd))
         (q-r (inverse-gaussian-quantile p-r mu-r lambda-r))
         (f-pdf (inverse-gaussian-pdf q-r mu-r lambda-r))
         (f-mu (inverse-gaussian-cdf-partial-mu q-r mu-r lambda-r))
         (f-lambda (inverse-gaussian-cdf-partial-lambda q-r mu-r lambda-r))
         (dq-dp (/ 1d0 f-pdf))
         (dq-dmu (- (/ f-mu f-pdf)))
         (dq-dlambda (- (/ f-lambda f-pdf))))
    (make-hd q-r
             (+ (* dq-dp (hyper-dual-e1 p-hd))
                (* dq-dmu (hyper-dual-e1 mu-hd))
                (* dq-dlambda (hyper-dual-e1 lambda-hd)))
             (+ (* dq-dp (hyper-dual-e2 p-hd))
                (* dq-dmu (hyper-dual-e2 mu-hd))
                (* dq-dlambda (hyper-dual-e2 lambda-hd)))
             (+ (* dq-dp (hyper-dual-e12 p-hd))
                (* dq-dmu (hyper-dual-e12 mu-hd))
                (* dq-dlambda (hyper-dual-e12 lambda-hd))))))

;;; --- convenience ----------------------------------------------------------

(defun hd-derivatives (f x)
  "Evaluate F (a Lisp function expecting and returning hyper-dual) at point
X (a real number) and return (values f(x) f'(x) f''(x))."
  (let ((result (funcall f (hd-deriv-seed x))))
    (values (hd-value result)
            (hd-first-derivative result)
            (hd-second-derivative result))))
