;;;; scalars.lisp --- f64/hyper-dual coefficient abstraction and obstructions.

(in-package #:rosette-quantum-vm)

(defparameter +obstruction-projection-max-depth+ 256)
(defparameter +obstruction-projection-max-conses+ 4096)

(defun %obstruction-data-projection (object)
  "Copy diagnostic cons data without retaining cycles or unbounded trees."
  (let ((active (make-hash-table :test #'eq))
        (remaining +obstruction-projection-max-conses+))
    (labels ((project (value depth)
               (cond
                 ((atom value) value)
                 ((gethash value active) :circular-reference)
                 ((>= depth +obstruction-projection-max-depth+)
                  :projection-depth-limit)
                 ((zerop remaining) :projection-size-limit)
                 (t
                  (decf remaining)
                  (setf (gethash value active) t)
                  (unwind-protect
                       (cons (project (car value) (1+ depth))
                             (project (cdr value) (1+ depth)))
                    (remhash value active))))))
      (project object 0))))

(define-condition quantum-vm-obstruction (error)
  ((kind :initarg :kind :reader %obstruction-kind)
   (path :initarg :path :initform nil :reader %obstruction-path)
   (details :initarg :details :initform nil :reader %obstruction-details))
  (:report
   (lambda (condition stream)
     ;; Conditions are public and can be constructed without %OBSTRUCT, so
     ;; project again here and bound the printer rather than trusting slots.
     (let ((*print-circle* t)
           (*print-length* 128)
           (*print-level* 32)
           (*print-pretty* nil)
           (*print-readably* nil))
       (format stream "Quantum VM refused (~S)~@[ at ~S~]~@[ — ~S~]"
               (%obstruction-kind condition)
               (%obstruction-data-projection (%obstruction-path condition))
               (%obstruction-data-projection
                (%obstruction-details condition)))))))

(defun quantum-vm-obstruction-kind (condition)
  (%obstruction-kind condition))

(defun quantum-vm-obstruction-path (condition)
  (%obstruction-data-projection (%obstruction-path condition)))

(defun quantum-vm-obstruction-details (condition)
  (%obstruction-data-projection (%obstruction-details condition)))

(defun %obstruct (kind &key path details)
  (error 'quantum-vm-obstruction
         :kind kind
         :path (%obstruction-data-projection path)
         :details (%obstruction-data-projection details)))

(declaim (inline %hd-p %s+ %s- %s-neg %s* %s/ %s-sin %s-cos %s-value))

(defun %hd-p (x)
  (rosette-hyper-dual:hyper-dual-p x))

(defun %coerce-real (x)
  (if (typep x 'double-float) x (coerce x 'double-float)))

(defun %s+ (x y)
  (if (or (%hd-p x) (%hd-p y))
      (rosette-hyper-dual:hd-add x y)
      (+ (%coerce-real x) (%coerce-real y))))

(defun %s- (x y)
  (if (or (%hd-p x) (%hd-p y))
      (rosette-hyper-dual:hd-sub x y)
      (- (%coerce-real x) (%coerce-real y))))

(defun %s-neg (x)
  (if (%hd-p x) (rosette-hyper-dual:hd-neg x) (- (%coerce-real x))))

(defun %s* (x y)
  (if (or (%hd-p x) (%hd-p y))
      (rosette-hyper-dual:hd-mul x y)
      (* (%coerce-real x) (%coerce-real y))))

(defun %s/ (x y)
  (if (or (%hd-p x) (%hd-p y))
      (rosette-hyper-dual:hd-div x y)
      (/ (%coerce-real x) (%coerce-real y))))

(defun %s-sin (x)
  (if (%hd-p x) (rosette-hyper-dual:hd-sin x) (sin (%coerce-real x))))

(defun %s-cos (x)
  (if (%hd-p x) (rosette-hyper-dual:hd-cos x) (cos (%coerce-real x))))

(defun %s-value (x)
  (if (%hd-p x) (rosette-hyper-dual:hd-value x) (%coerce-real x)))

(defun %s-component (x component)
  (if (%hd-p x)
      (ecase component
        (:value (rosette-hyper-dual:hd-value x))
        (:d1 (rosette-hyper-dual:hd-d1 x))
        (:d2 (rosette-hyper-dual:hd-d2 x))
        (:d12 (rosette-hyper-dual:hd-d12 x)))
      (if (eq component :value) (%coerce-real x) 0d0)))

(defun %zero-like (prototype)
  (if (%hd-p prototype) (rosette-hyper-dual:hd-from-real 0d0) 0d0))

(defun %one-like (prototype)
  (if (%hd-p prototype) (rosette-hyper-dual:hd-from-real 1d0) 1d0))

(defstruct (%complex-scalar
            (:constructor %c (real imag))
            (:conc-name %c-)
            (:copier nil))
  real imag)

(defun %c+ (a b)
  (%c (%s+ (%c-real a) (%c-real b))
      (%s+ (%c-imag a) (%c-imag b))))

(defun %c* (a b)
  (%c (%s- (%s* (%c-real a) (%c-real b))
           (%s* (%c-imag a) (%c-imag b)))
      (%s+ (%s* (%c-real a) (%c-imag b))
           (%s* (%c-imag a) (%c-real b)))))

(defun %c-conjugate (a)
  (%c (%c-real a) (%s-neg (%c-imag a))))

(defun %scalar-content (x)
  (if (%hd-p x)
      (list :hd (rosette-hyper-dual:hd-value x)
                (rosette-hyper-dual:hd-d1 x)
                (rosette-hyper-dual:hd-d2 x)
                (rosette-hyper-dual:hd-d12 x))
      (%coerce-real x)))

(defun %stable-fingerprint (content)
  "Process-independent FNV-1a digest for already-canonical CONTENT."
  (let* ((text (with-standard-io-syntax
                 (let ((*package* (find-package '#:rosette-quantum-vm))
                       (*print-circle* nil)
                       (*print-pretty* nil)
                       (*print-readably* t))
                   (write-to-string content))))
         (hash #xcbf29ce484222325))
    (loop for character across text do
      (setf hash
            (logand #xffffffffffffffff
                    (* (logxor hash (char-code character))
                       #x100000001b3))))
    (format nil "fnv1a64-~16,'0X" hash)))
