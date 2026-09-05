(in-package #:rosette-hyper-dual)

;;;; Exact arbitrary-order univariate jets: f(x), f'(x), …, f^(k)(x) in one
;;;; forward pass, carried in EXACT common-lisp rational arithmetic (integer /
;;;; ratio / bignum) with NO float coercion anywhere on the path.
;;;;
;;;; This is the exact counterpart of JET-N.  Where JET-N stores its Taylor
;;;; coefficients in a (simple-array double-float (*)) and coerces every
;;;; operand through AS-F64, EXACT-JET stores them in a general (simple-array t
;;;; (*)) whose elements are always CL `rational`s (integer, ratio, or bignum),
;;;; and every recurrence runs over `+ - * / expt` on rationals.  The result of
;;;;     (derivative-n-exact (lambda (j) (exact-jet-expt j 30)) 1 7)
;;;; is therefore the exact integer 30!/23!, not a float "close to" it.
;;;;
;;;; The carrier holds (a_0 a_1 … a_k), the truncated Taylor series
;;;;     a_0 + a_1·ε + a_2·ε² + … + a_k·εᵏ,   ε^{k+1} = 0,
;;;; with a_j = f^(j)(x_0)/j! after evaluation; f^(j) is recovered as j!·a_j.
;;;; The differentiation seed at x_0 is (x_0, 1, 0, …, 0).
;;;;
;;;; Recurrences (all exact — identical algebra to JET-N, no `float`/`as-f64`):
;;;;   mul   — Cauchy product   c_k = Σ_{i=0}^k a_i b_{k-i}
;;;;   recip — solve c·a = 1    c_0 = 1/a_0, c_k = -(1/a_0) Σ_{i=1}^k a_i c_{k-i}
;;;;   div   — x / y = x · (1/y)
;;;;   expt  — x^p (p a non-negative integer) by binary exponentiation over mul
;;;;
;;;; LOCATED WALL.  Exactness holds precisely on the field-with-integer-powers
;;;; fragment  {+, -, *, /, integer-expt}  and their compositions: everything
;;;; expressible as an exact rational function of the seed.  Transcendentals
;;;; (exp, log, sqrt, non-integer powers) are NOT exact over the rationals —
;;;; their Taylor coefficients are irrational — so they are deliberately absent
;;;; here; reach for JET-N (f64) when you need them.

(deftype exact-jet-coeffs () '(simple-array t (*)))

(defstruct (exact-jet
            (:constructor %make-exact-jet (coeffs))
            (:print-function
             (lambda (j s d)
               (declare (ignore d))
               (let ((v (exact-jet-coeffs j)))
                 (format s "#<exact-jet order ~D:" (1- (length v)))
                 (dotimes (k (length v))
                   (format s " ~A~:[~;·ε~]~:[~;^~D~]"
                           (aref v k) (plusp k) (> k 1) k))
                 (format s ">")))))
  (coeffs (make-array 1 :initial-element 0) :type exact-jet-coeffs))

;;; --- exactness guard -----------------------------------------------------

(declaim (inline ensure-exact))
(defun ensure-exact (x)
  "Assert X is an exact CL rational (integer / ratio / bignum) and return it.
The whole point of this carrier is that no float ever enters the recurrence;
this fails loudly rather than silently coercing, in the container-first spirit."
  (unless (rationalp x)
    (error "exact-jet: non-rational value ~S would break exactness" x))
  x)

;;; --- constructors & accessors --------------------------------------------

(defun make-exact-jet (coeffs)
  "Build an exact-jet from a sequence COEFFS of Taylor coefficients (a_0 … a_k).
Order is (length COEFFS) − 1.  Every element must be an exact CL rational."
  (let* ((k (1- (length coeffs))))
    (when (minusp k) (error "make-exact-jet: need at least one coefficient"))
    (let ((v (make-array (1+ k) :initial-element 0)))
      (map-into v #'ensure-exact coeffs)
      (%make-exact-jet v))))

(defun exact-jet-order (j)
  "Truncation order k (so the carrier holds k+1 coefficients)."
  (1- (length (exact-jet-coeffs j))))

(declaim (inline exact-jet-coeff))
(defun exact-jet-coeff (j k)
  "Taylor coefficient a_k = f^(k)(x_0)/k!  (exact 0 if k exceeds the order)."
  (let ((v (exact-jet-coeffs j)))
    (if (< k (length v)) (aref v k) 0)))

(defun exact-jet-from-rational (a k)
  "Lift exact rational A to an order-K jet with no derivative perturbation."
  (let ((v (make-array (1+ k) :initial-element 0)))
    (setf (aref v 0) (ensure-exact a))
    (%make-exact-jet v)))

(defun exact-jet-deriv-seed (x k)
  "Seed for differentiation at exact point X to order K: the jet x + ε."
  (let ((v (make-array (1+ k) :initial-element 0)))
    (setf (aref v 0) (ensure-exact x))
    (when (>= k 1) (setf (aref v 1) 1))
    (%make-exact-jet v)))

(defun exact-jet-value (j) "f(x_0) = a_0." (exact-jet-coeff j 0))

(defun %exact-factorial (k)
  "k! as an exact integer (bignum for large k)."
  (let ((f 1)) (loop for i from 2 to k do (setf f (* f i))) f))

(defun exact-jet-derivative (j k)
  "f^(k)(x_0) = k! · a_k, exact."
  (* (%exact-factorial k) (exact-jet-coeff j k)))

;;; --- order reconciliation ------------------------------------------------

(defun %exact-jet-order2 (x y)
  "Common order of two operands, at least one of which is an exact-jet."
  (cond ((and (exact-jet-p x) (exact-jet-p y))
         (max (exact-jet-order x) (exact-jet-order y)))
        ((exact-jet-p x) (exact-jet-order x))
        ((exact-jet-p y) (exact-jet-order y))
        (t (error "%exact-jet-order2: neither argument is an exact-jet"))))

(defun %as-exact-jet (x k)
  "Coerce X to an exact-jet of order K (lifting rationals; reusing jets as-is)."
  (if (exact-jet-p x) x (exact-jet-from-rational x k)))

;;; --- arithmetic ----------------------------------------------------------

(defun exact-jet-add (x y)
  "Component-wise addition (operands lifted to a common order)."
  (let* ((k (%exact-jet-order2 x y))
         (a (exact-jet-coeffs (%as-exact-jet x k)))
         (b (exact-jet-coeffs (%as-exact-jet y k)))
         (c (make-array (1+ k) :initial-element 0)))
    (dotimes (i (1+ k)) (setf (aref c i) (+ (aref a i) (aref b i))))
    (%make-exact-jet c)))

(defun exact-jet-sub (x y)
  "Component-wise subtraction."
  (let* ((k (%exact-jet-order2 x y))
         (a (exact-jet-coeffs (%as-exact-jet x k)))
         (b (exact-jet-coeffs (%as-exact-jet y k)))
         (c (make-array (1+ k) :initial-element 0)))
    (dotimes (i (1+ k)) (setf (aref c i) (- (aref a i) (aref b i))))
    (%make-exact-jet c)))

(defun exact-jet-neg (x)
  "Component-wise negation."
  (if (rationalp x)
      (exact-jet-neg (exact-jet-from-rational x 0))
      (let* ((k (exact-jet-order x)) (a (exact-jet-coeffs x))
             (c (make-array (1+ k) :initial-element 0)))
        (dotimes (i (1+ k)) (setf (aref c i) (- (aref a i))))
        (%make-exact-jet c))))

(defun exact-jet-mul (x y)
  "Cauchy product: c_k = Σ_{i=0}^k a_i b_{k-i}, truncated at the common order."
  (let* ((k (%exact-jet-order2 x y))
         (a (exact-jet-coeffs (%as-exact-jet x k)))
         (b (exact-jet-coeffs (%as-exact-jet y k)))
         (c (make-array (1+ k) :initial-element 0)))
    (dotimes (m (1+ k))
      (let ((s 0))
        (dotimes (i (1+ m)) (setf s (+ s (* (aref a i) (aref b (- m i))))))
        (setf (aref c m) s)))
    (%make-exact-jet c)))

(defun exact-jet-recip (x)
  "1/x via c·x = 1: c_0 = 1/a_0, c_k = -(1/a_0) Σ_{i=1}^k a_i c_{k-i}."
  (if (rationalp x)
      (exact-jet-recip (exact-jet-from-rational x 0))
      (let* ((k (exact-jet-order x)) (a (exact-jet-coeffs x))
             (c (make-array (1+ k) :initial-element 0)))
        (when (zerop (aref a 0))
          (error "exact-jet-recip: division by zero (a_0 is 0)"))
        (setf (aref c 0) (/ 1 (aref a 0)))
        (loop for m from 1 to k do
          (let ((s 0))
            (loop for i from 1 to m do (setf s (+ s (* (aref a i) (aref c (- m i))))))
            (setf (aref c m) (- (/ s (aref a 0))))))
        (%make-exact-jet c))))

(defun exact-jet-div (x y)
  "x / y = x · (1/y), exact."
  (exact-jet-mul x (exact-jet-recip y)))

(defun exact-jet-expt (x p)
  "x^P for a non-negative integer P, by binary exponentiation over exact mul.
Exact for any rational base — no exp(P·log x) detour, so a_0 may be any
rational (including 0 and negatives)."
  (unless (and (integerp p) (>= p 0))
    (error "exact-jet-expt: exponent ~S must be a non-negative integer" p))
  (let ((k (if (exact-jet-p x) (exact-jet-order x) 0)))
    (let ((base (%as-exact-jet x k))
          (acc (exact-jet-from-rational 1 k))
          (e p))
      (loop while (plusp e) do
        (when (oddp e) (setf acc (exact-jet-mul acc base)))
        (setf e (ash e -1))
        (when (plusp e) (setf base (exact-jet-mul base base))))
      acc)))

;;; --- driver API ----------------------------------------------------------

(defun taylor-exact (fn x k)
  "Evaluate FN (an exact-jet → exact-jet closure) at exact point X to order K.
Returns the exact Taylor-coefficient vector #(a_0 a_1 … a_k),
a_j = f^(j)(X)/j!, every element an exact CL rational."
  (let* ((j (funcall fn (exact-jet-deriv-seed x k)))
         (out (make-array (1+ k) :initial-element 0)))
    (dotimes (i (1+ k) out)
      (setf (aref out i) (exact-jet-coeff j i)))))

(defun derivative-n-exact (fn x k)
  "The exact k-th derivative f^(k)(X) of FN at exact point X.
FN is a closure over the exact-jet ops; the result is k!·a_k as an exact
CL rational (integer, ratio, or bignum) — never a float."
  (let ((j (funcall fn (exact-jet-deriv-seed x k))))
    (exact-jet-derivative j k)))
