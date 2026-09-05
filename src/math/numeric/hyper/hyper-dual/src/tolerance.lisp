;;;; tolerance.lisp --- scalar tolerance as a dimensionally scaled jet budget.

(in-package #:rosette-hyper-dual)

(defstruct (jet-tolerance-certificate
            (:constructor %make-jet-tolerance-certificate))
  "Executable receipt comparing every derivative component against the scalar
tolerance transported into that derivative's units."
  (budgets #() :type vector)
  (residuals #() :type vector)
  (normalized-residuals #() :type vector)
  (max-normalized-residual 0d0 :type double-float)
  (passed-p nil :type boolean))

(defun jet-derivative-vector (jet)
  "Return #(f,f',...,f^(n)) for any public floating jet carrier."
  (typecase jet
    (hyper-dual
     (vector (hd-value jet) (hd-first-derivative jet)
             (hd-second-derivative jet)))
    (jet3 (vector (jet3-value jet) (jet3-d1 jet) (jet3-d2 jet)
                  (jet3-d3 jet)))
    (jet4 (vector (jet4-value jet) (jet4-d1 jet) (jet4-d2 jet)
                  (jet4-d3 jet) (jet4-d4 jet)))
    (jetn
     (let ((out (make-array (1+ (jetn-order jet))
                            :element-type 'double-float)))
       (dotimes (k (length out) out)
         (setf (aref out k) (jetn-derivative jet k)))))
    (vector
     (map 'vector (lambda (x) (coerce x 'double-float)) jet))
    (t (error "Unsupported jet carrier ~S" (type-of jet)))))

(defun jet-tolerance-profile (reference &key
                                          (absolute-tolerance
                                           rosette-scalar-core:+default-tolerance+)
                                          (relative-tolerance 0d0)
                                          (input-scale 1d0))
  "Transport a scalar tolerance to all derivative orders of REFERENCE.

Order k has units output/input^k, so its absolute budget is ABS-TOL/SCALE^k.
REL-TOL additionally contributes REL-TOL*|reference_k|. INPUT-SCALE must be a
positive characteristic input magnitude; making it explicit prevents unlike
derivative units from being compared with one magic epsilon."
  (when (minusp absolute-tolerance)
    (error "ABSOLUTE-TOLERANCE must be non-negative"))
  (when (minusp relative-tolerance)
    (error "RELATIVE-TOLERANCE must be non-negative"))
  (unless (plusp input-scale)
    (error "INPUT-SCALE must be positive"))
  (let* ((derivatives (jet-derivative-vector reference))
         (out (make-array (length derivatives) :element-type 'double-float))
         (abs-tol (coerce absolute-tolerance 'double-float))
         (rel-tol (coerce relative-tolerance 'double-float))
         (scale (coerce input-scale 'double-float)))
    (dotimes (k (length out) out)
      (setf (aref out k)
            (+ (/ abs-tol (expt scale k))
               (* rel-tol (abs (aref derivatives k))))))))

(defun certify-jet-tolerance (reference observed &key
                                                   (absolute-tolerance
                                                    rosette-scalar-core:+default-tolerance+)
                                                   (relative-tolerance 0d0)
                                                   (input-scale 1d0))
  "Compare two jet gauges componentwise and return a machine-checkable receipt.
The certificate passes exactly when every |observed_k-reference_k| is within
the dimensionally transported scalar budget for derivative order k."
  (let* ((ref (jet-derivative-vector reference))
         (got (jet-derivative-vector observed)))
    (unless (= (length ref) (length got))
      (error "Jet orders differ: ~D versus ~D" (1- (length ref)) (1- (length got))))
    (let* ((budgets (jet-tolerance-profile
                     ref :absolute-tolerance absolute-tolerance
                         :relative-tolerance relative-tolerance
                         :input-scale input-scale))
           (residuals (make-array (length ref) :element-type 'double-float))
           (normalized (make-array (length ref) :element-type 'double-float))
           (passed t)
           (maximum 0d0))
      (dotimes (k (length ref))
        (let* ((residual (abs (- (aref got k) (aref ref k))))
               (budget (aref budgets k))
               (ratio (cond ((plusp budget) (/ residual budget))
                            ((zerop residual) 0d0)
                            (t most-positive-double-float))))
          (setf (aref residuals k) residual
                (aref normalized k) ratio
                maximum (max maximum ratio))
          (unless (<= residual budget) (setf passed nil))))
      (%make-jet-tolerance-certificate
       :budgets budgets :residuals residuals
       :normalized-residuals normalized
       :max-normalized-residual maximum :passed-p passed))))
