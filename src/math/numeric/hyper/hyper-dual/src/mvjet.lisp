(in-package #:rosette-hyper-dual)

;;;; Multivariate truncated Taylor jets: f and all its mixed partials up to a
;;;; chosen total degree d over m variables, in a single forward pass.
;;;;
;;;; A multivariate jet over m variables truncated at total degree d is an
;;;; element of the ring  R[e1 ... e_m] / (monomials of total degree > d).
;;;; It is stored as a dense coefficient vector indexed by the multi-indices
;;;; alpha = (e_1 ... e_m) with |alpha| = Sum e_i <= d, laid out by a shared, memoised
;;;; MVJET-LAYOUT.  After evaluating f on the seed jets (variable k = x_k + e_k)
;;;; the coefficient at alpha equals (d^|alpha| f / dx^alpha)(x) / alpha!, so the raw partial
;;;; is alpha!*coeff (MVJET-PARTIAL).
;;;;
;;;; Ring ops are the truncated multivariate sum/product.  Smooth functions g
;;;; use the nilpotent-part identity: writing a jet as a0 + p where p collects
;;;; every term of total degree >= 1, p is nilpotent (p^{d+1} truncates to 0),
;;;; so g(a0 + p) = Sum_{k=0}^{d} (g^{(k)}(a0)/k!)*p^k exactly -- a Horner
;;;; evaluation over p using the univariate Taylor coefficients of g at a0.
;;;;
;;;; jet-n is the single-variable specialisation; this file is the m-variable
;;;; generalisation, and (at d = 2) delivers the full gradient AND Hessian in
;;;; one pass -- see MVJET-GRAD-HESSIAN.

;;; --- layout: multi-index enumeration, ordinals, factorials ---------------

(defstruct (mvjet-layout (:constructor %make-mvjet-layout))
  (m 0 :type fixnum)
  (d 0 :type fixnum)
  (size 0 :type fixnum)
  (indices #() :type simple-vector)                 ; ordinal -> multi-index
  (degrees #() :type (simple-array fixnum (*)))      ; ordinal -> |alpha|
  (factorials #() :type (simple-array double-float (*))) ; ordinal -> alpha!
  (ordinals (make-hash-table) :type hash-table))     ; packed key -> ordinal

(defvar *mvjet-layouts* (make-hash-table :test 'equal)
  "Memo of MVJET-LAYOUT keyed by (M . D).")

(defun %degree (mi)
  "Total degree |alpha| of a multi-index."
  (let ((s 0)) (dotimes (i (length mi) s) (incf s (svref mi i)))))

(defun %pack-index (mi d)
  "Bijective fixnum key for a multi-index whose entries are all <= D."
  (let ((base (1+ d)) (key 0))
    (loop for i from (1- (length mi)) downto 0
          do (setf key (+ (* key base) (svref mi i))))
    key))

(defun %multi-factorial (mi)
  "alpha! = Product e_i!"
  (let ((f 1d0))
    (loop for e across mi
          do (loop for i from 2 to e do (setf f (* f (coerce i 'double-float)))))
    f))

(defun %add-mi (a b)
  "Component-wise sum of two multi-indices (a fresh SIMPLE-VECTOR)."
  (let ((v (make-array (length a))))
    (dotimes (i (length a) v) (setf (svref v i) (+ (svref a i) (svref b i))))))

(defun %enumerate-multi-indices (m d)
  "All length-M multi-indices with total degree <= D, canonical order
\(all-zero index first)."
  (let ((out '()))
    (labels ((rec (pos remaining acc)
               (if (= pos m)
                   (push (coerce (reverse acc) 'simple-vector) out)
                   (loop for e from 0 to remaining
                         do (rec (1+ pos) (- remaining e) (cons e acc))))))
      (rec 0 d '()))
    (nreverse out)))

(defun %build-mvjet-layout (m d)
  (when (< m 1) (error "mvjet: need at least one variable"))
  (when (< d 0) (error "mvjet: total order must be non-negative"))
  (let* ((mis (%enumerate-multi-indices m d))
         (size (length mis))
         (indices (make-array size))
         (degrees (make-array size :element-type 'fixnum))
         (facts (make-array size :element-type 'double-float))
         (ords (make-hash-table)))
    (loop for mi in mis for k from 0 do
      (setf (svref indices k) mi
            (aref degrees k) (%degree mi)
            (aref facts k) (%multi-factorial mi)
            (gethash (%pack-index mi d) ords) k))
    (%make-mvjet-layout :m m :d d :size size :indices indices
                        :degrees degrees :factorials facts :ordinals ords)))

(defun ensure-mvjet-layout (m d)
  "Memoised MVJET-LAYOUT for M variables at total order D."
  (let ((key (cons m d)))
    (or (gethash key *mvjet-layouts*)
        (setf (gethash key *mvjet-layouts*) (%build-mvjet-layout m d)))))

(defun %constant-ordinal (layout)
  "Ordinal of the all-zero multi-index (the constant/value coefficient)."
  (gethash 0 (mvjet-layout-ordinals layout)))

(defun %ordinal (layout mi)
  "Ordinal of multi-index MI in LAYOUT, or NIL if its degree exceeds D."
  (gethash (%pack-index mi (mvjet-layout-d layout))
           (mvjet-layout-ordinals layout)))

;;; --- the jet carrier -----------------------------------------------------

(defstruct (mvjet (:constructor %make-mvjet (layout coeffs))
                  (:print-function
                   (lambda (j s dp)
                     (declare (ignore dp))
                     (format s "#<mvjet ~Dvar order ~D: value ~,4F>"
                             (mvjet-layout-m (mvjet-layout j))
                             (mvjet-layout-d (mvjet-layout j))
                             (mvjet-value j)))))
  (layout nil :type mvjet-layout)
  (coeffs #() :type (simple-array double-float (*))))

(defun %mvjet-zero (layout)
  (%make-mvjet layout (make-array (mvjet-layout-size layout)
                                  :element-type 'double-float :initial-element 0d0)))

(defun %mvjet-const (layout a)
  "Constant jet with value A in LAYOUT."
  (let ((jc (%mvjet-zero layout)))
    (setf (aref (mvjet-coeffs jc) (%constant-ordinal layout)) (as-f64 a))
    jc))

(defun mvjet-from-real (a m d)
  "Lift real A to an M-variable, order-D jet with no perturbation."
  (%mvjet-const (ensure-mvjet-layout m d) a))

(defun mvjet-num-vars (j) (mvjet-layout-m (mvjet-layout j)))
(defun mvjet-order (j) (mvjet-layout-d (mvjet-layout j)))

;;; --- accessors: value, coefficients, partials, gradient, Hessian ---------

(defun mvjet-value (j)
  "f(x) = coefficient at the all-zero multi-index."
  (aref (mvjet-coeffs j) (%constant-ordinal (mvjet-layout j))))

(defun mvjet-coeff (j mi)
  "Taylor coefficient at multi-index MI (0 if |MI| exceeds the order)."
  (let ((ord (%ordinal (mvjet-layout j) mi)))
    (if ord (aref (mvjet-coeffs j) ord) 0d0)))

(defun mvjet-partial (j mi)
  "Mixed partial derivative d^|alpha| f/dx^alpha at the seed point, alpha = MI.
Equals alpha!*coeff(MI)."
  (* (%multi-factorial mi) (mvjet-coeff j mi)))

(defun %unit-index (m i)
  (let ((v (make-array m :initial-element 0))) (setf (svref v i) 1) v))

(defun %pair-index (m i j)
  (let ((v (make-array m :initial-element 0)))
    (incf (svref v i)) (incf (svref v j)) v))     ; i = j yields a 2

(defun mvjet-gradient (j)
  "Gradient grad f as a (SIMPLE-ARRAY DOUBLE-FLOAT (M)).  Requires order >= 1."
  (let* ((m (mvjet-num-vars j))
         (g (make-array m :element-type 'double-float)))
    (dotimes (i m g)
      (setf (aref g i) (mvjet-partial j (%unit-index m i))))))

(defun mvjet-hessian (j)
  "Hessian d2f as a symmetric (SIMPLE-ARRAY DOUBLE-FLOAT (M M)).
Requires order >= 2."
  (let* ((m (mvjet-num-vars j))
         (h (make-array (list m m) :element-type 'double-float)))
    (dotimes (i m h)
      (loop for k from i below m do
        (let ((v (mvjet-partial j (%pair-index m i k))))
          (setf (aref h i k) v (aref h k i) v))))))

;;; --- seeds and evaluation ------------------------------------------------

(defun mvjet-seeds (x d)
  "SIMPLE-VECTOR of M = (LENGTH X) seed jets over M variables at total order D:
seed k carries value X_k and a unit first-order perturbation e_k."
  (let* ((m (length x))
         (layout (ensure-mvjet-layout m d))
         (seeds (make-array m)))
    (when (< d 1) (error "mvjet-seeds: order must be >= 1 to carry derivatives"))
    (dotimes (i m seeds)
      (let ((jc (%mvjet-const layout (elt x i))))
        (setf (aref (mvjet-coeffs jc) (%ordinal layout (%unit-index m i))) 1d0)
        (setf (aref seeds i) jc)))))

(defun mvjet-derivatives (f x d)
  "Evaluate F (a seeds-vector -> mvjet function) at the real point X to total
order D and return the resulting mvjet.  Read partials off it with
MVJET-PARTIAL / MVJET-GRADIENT / MVJET-HESSIAN."
  (funcall f (mvjet-seeds x d)))

(defun mvjet-grad-hessian (f x)
  "Exact value, gradient and full Hessian of a scalar F : R^m -> R at X in a
SINGLE forward pass (an order-2 multivariate jet).  F receives a
SIMPLE-VECTOR of M mvjet numbers and returns an mvjet.  Returns
\(values GRADIENT HESSIAN F-VALUE).  Unlike HD-GRAD-HESSIAN's n(n+1)/2
hyper-dual passes, this evaluates F once -- at the cost of carrying the
order-2 coefficient table."
  (let ((j (mvjet-derivatives f x 2)))
    (values (mvjet-gradient j) (mvjet-hessian j) (mvjet-value j))))

;;; --- ring operations -----------------------------------------------------

(defun %binary-layout (x y)
  (cond ((and (mvjet-p x) (mvjet-p y))
         (let ((lx (mvjet-layout x)) (ly (mvjet-layout y)))
           (unless (and (= (mvjet-layout-m lx) (mvjet-layout-m ly))
                        (= (mvjet-layout-d lx) (mvjet-layout-d ly)))
             (error "mvjet: operands have mismatched (m,d) layouts"))
           lx))
        ((mvjet-p x) (mvjet-layout x))
        ((mvjet-p y) (mvjet-layout y))
        (t (error "mvjet: at least one operand must be an mvjet"))))

(defun %as-mvjet (x layout)
  (if (mvjet-p x) x (%mvjet-const layout (as-f64 x))))

(defun mvjet-add (x y)
  "Truncated multivariate sum (operands lifted to a common layout)."
  (let* ((layout (%binary-layout x y))
         (a (mvjet-coeffs (%as-mvjet x layout)))
         (b (mvjet-coeffs (%as-mvjet y layout)))
         (c (make-array (mvjet-layout-size layout) :element-type 'double-float)))
    (dotimes (k (length c)) (setf (aref c k) (+ (aref a k) (aref b k))))
    (%make-mvjet layout c)))

(defun mvjet-sub (x y)
  "Truncated multivariate difference."
  (let* ((layout (%binary-layout x y))
         (a (mvjet-coeffs (%as-mvjet x layout)))
         (b (mvjet-coeffs (%as-mvjet y layout)))
         (c (make-array (mvjet-layout-size layout) :element-type 'double-float)))
    (dotimes (k (length c)) (setf (aref c k) (- (aref a k) (aref b k))))
    (%make-mvjet layout c)))

(defun mvjet-scale (x s)
  "Multiply every coefficient by the real scalar S."
  (let* ((s (as-f64 s)) (a (mvjet-coeffs x))
         (c (make-array (length a) :element-type 'double-float)))
    (dotimes (k (length a)) (setf (aref c k) (* s (aref a k))))
    (%make-mvjet (mvjet-layout x) c)))

(defun mvjet-neg (x)
  (if (mvjet-p x) (mvjet-scale x -1d0) (- (as-f64 x))))

(defun mvjet-mul (x y)
  "Truncated multivariate product: c[alpha+beta] += a[alpha]*b[beta] whenever
|alpha|+|beta| <= d."
  (let* ((layout (%binary-layout x y))
         (d (mvjet-layout-d layout))
         (idx (mvjet-layout-indices layout))
         (deg (mvjet-layout-degrees layout))
         (ords (mvjet-layout-ordinals layout))
         (a (mvjet-coeffs (%as-mvjet x layout)))
         (b (mvjet-coeffs (%as-mvjet y layout)))
         (size (mvjet-layout-size layout))
         (c (make-array size :element-type 'double-float :initial-element 0d0)))
    (dotimes (ia size)
      (let ((av (aref a ia)))
        (unless (zerop av)
          (let ((dega (aref deg ia)) (mia (svref idx ia)))
            (dotimes (ib size)
              (let ((bv (aref b ib)))
                (unless (zerop bv)
                  (when (<= (+ dega (aref deg ib)) d)
                    (let ((ord (gethash (%pack-index (%add-mi mia (svref idx ib)) d)
                                        ords)))
                      (incf (aref c ord) (* av bv)))))))))))
    (%make-mvjet layout c)))

;;; --- smooth functions via the nilpotent-part Horner identity -------------

(defun %mvjet-nilpotent (j)
  "Copy of J with its constant term zeroed (the nilpotent part p)."
  (let* ((layout (mvjet-layout j))
         (c (copy-seq (mvjet-coeffs j))))
    (setf (aref c (%constant-ordinal layout)) 0d0)
    (%make-mvjet layout c)))

(defun %mvjet-compose (j taylor)
  "g(J) = Sum_{k=0}^{d} TAYLOR[k]*p^k, p the nilpotent part of J, evaluated by
Horner.  TAYLOR is the length-(d+1) univariate Taylor-coefficient vector of g
about (MVJET-VALUE J)."
  (let* ((layout (mvjet-layout j))
         (d (mvjet-layout-d layout))
         (p (%mvjet-nilpotent j))
         (r (%mvjet-const layout (aref taylor d))))
    (loop for k from (1- d) downto 0 do
      (setf r (mvjet-add (mvjet-mul r p) (%mvjet-const layout (aref taylor k)))))
    r))

(defun %exp-series (a0 d)
  (let ((tv (make-array (1+ d) :element-type 'double-float))
        (e (exp a0)) (fact 1d0))
    (dotimes (k (1+ d) tv)
      (when (> k 0) (setf fact (* fact (coerce k 'double-float))))
      (setf (aref tv k) (/ e fact)))))

(defun %log-series (a0 d)
  (when (<= a0 0d0) (error "mvjet-log: non-positive constant term"))
  (let ((tv (make-array (1+ d) :element-type 'double-float)))
    (setf (aref tv 0) (log a0))
    (loop for k from 1 to d do
      ;; g^{(k)}(a0)/k! = (-1)^{k-1} / (k*a0^k)
      (setf (aref tv k) (/ (if (oddp k) 1d0 -1d0) (* k (expt a0 k)))))
    tv))

(defun %binom-series (a0 s d)
  "Taylor coefficients of t^s about A0>0: t_k = C(s,k)*a0^{s-k}, C real."
  (when (<= a0 0d0) (error "mvjet: non-positive constant term for real power"))
  (let ((tv (make-array (1+ d) :element-type 'double-float))
        (binom 1d0))
    (dotimes (k (1+ d) tv)
      (when (> k 0)
        (setf binom (* binom (/ (- s (coerce (1- k) 'double-float))
                                (coerce k 'double-float)))))
      (setf (aref tv k) (* binom (expt a0 (- s (coerce k 'double-float))))))))

(defun %recip-series (a0 d)
  "Taylor coefficients of 1/t about A0!=0: t_k = (-1)^k / a0^{k+1} (integer
exponents, so A0 may be negative)."
  (when (zerop a0) (error "mvjet-recip: zero constant term"))
  (let ((tv (make-array (1+ d) :element-type 'double-float)))
    (dotimes (k (1+ d) tv)
      (setf (aref tv k) (/ (if (evenp k) 1d0 -1d0) (expt a0 (1+ k)))))))

(defun mvjet-exp (x)
  (if (mvjet-p x)
      (%mvjet-compose x (%exp-series (mvjet-value x) (mvjet-order x)))
      (exp (as-f64 x))))

(defun mvjet-log (x)
  (if (mvjet-p x)
      (%mvjet-compose x (%log-series (mvjet-value x) (mvjet-order x)))
      (log (as-f64 x))))

(defun mvjet-sqrt (x)
  (if (mvjet-p x)
      (%mvjet-compose x (%binom-series (mvjet-value x) 0.5d0 (mvjet-order x)))
      (sqrt (as-f64 x))))

(defun mvjet-power (x s)
  "x^S for real exponent S; requires a positive constant term."
  (if (mvjet-p x)
      (%mvjet-compose x (%binom-series (mvjet-value x) (as-f64 s) (mvjet-order x)))
      (expt (as-f64 x) (as-f64 s))))

(defun mvjet-recip (x)
  (if (mvjet-p x)
      (%mvjet-compose x (%recip-series (mvjet-value x) (mvjet-order x)))
      (/ 1d0 (as-f64 x))))

(defun mvjet-div (x y)
  "x / y = x * (1/y)."
  (mvjet-mul x (mvjet-recip y)))
