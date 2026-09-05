;;;; state.lisp --- coefficient-generic split-complex dense state vectors.

(in-package #:rosette-quantum-vm)

(defstruct (quantum-state
            (:constructor %make-state (n-qubits real-parts imag-parts))
            (:conc-name %state-)
            (:copier nil))
  (n-qubits 1 :type (integer 1 *) :read-only t)
  (real-parts #() :type simple-vector :read-only t)
  (imag-parts #() :type simple-vector :read-only t))

(defun quantum-state-n-qubits (state)
  (%state-n-qubits state))

(defun quantum-state-real-parts (state)
  (let* ((source (%state-real-parts state))
         (copy (make-array (length source))))
    (dotimes (i (length source) copy)
      (setf (aref copy i) (aref source i)))))

(defun quantum-state-imag-parts (state)
  (let* ((source (%state-imag-parts state))
         (copy (make-array (length source))))
    (dotimes (i (length source) copy)
      (setf (aref copy i) (aref source i)))))

(defun quantum-state-values (state)
  "Project STATE coefficients to a fresh complex-double vector."
  (let* ((real (%state-real-parts state))
         (imag (%state-imag-parts state))
         (out (make-array (length real)
                          :element-type '(complex double-float))))
    (dotimes (i (length real) out)
      (setf (aref out i)
            (complex (%s-value (aref real i)) (%s-value (aref imag i)))))))

(defun %finite-double-p (value)
  (and (= value value) (<= (abs value) most-positive-double-float)))

(defun %power-of-two-p (integer)
  (and (plusp integer) (zerop (logand integer (1- integer)))))

(defun %complex-vector-state (vector context)
  "Checked private conversion shared by observable and VJP vector seams."
  (unless (and (vectorp vector)
               (>= (length vector) 2)
               (%power-of-two-p (length vector)))
    (%obstruct :dimension-mismatch
               :details (list :context context :amplitudes
                              (and (vectorp vector) (length vector))
                              :expected :positive-power-of-two-at-least-two)))
  (let* ((dimension (length vector))
         (n-qubits (1- (integer-length dimension)))
         (real (make-array dimension))
         (imag (make-array dimension)))
    (dotimes (index dimension)
      (let ((amplitude (aref vector index)))
        (unless (numberp amplitude)
          (%obstruct :malformed-state
                     :details (list :context context :index index
                                    :amplitude amplitude)))
        (let ((re (%coerce-real (realpart amplitude)))
              (im (%coerce-real (imagpart amplitude))))
          (unless (and (%finite-double-p re) (%finite-double-p im))
            (%obstruct :malformed-state
                       :details (list :context context :index index
                                      :amplitude amplitude)))
          (setf (aref real index) re
                (aref imag index) im))))
    (%make-state n-qubits real imag)))

(defun %coerce-state-input (object context &optional expected-n-qubits)
  (let ((state
          (cond
            ((quantum-state-p object) object)
            ((vectorp object) (%complex-vector-state object context))
            (t (%obstruct :malformed-state
                          :details (list :context context :state object))))))
    (when (and expected-n-qubits
               (/= expected-n-qubits (%state-n-qubits state)))
      (%obstruct :dimension-mismatch
                 :details (list :context context
                                :expected-qubits expected-n-qubits
                                :actual-qubits (%state-n-qubits state))))
    state))

(defun %basis-state (n-qubits basis prototype)
  (unless (and (integerp n-qubits) (plusp n-qubits))
    (%obstruct :malformed-tape :details (list :n-qubits n-qubits)))
  (let ((dimension (ash 1 n-qubits)))
    (unless (and (integerp basis) (<= 0 basis) (< basis dimension))
      (%obstruct :malformed-tape
                 :details (list :basis basis :dimension dimension)))
    (let ((real (make-array dimension))
          (imag (make-array dimension)))
      (dotimes (i dimension)
        (setf (aref real i) (%zero-like prototype)
              (aref imag i) (%zero-like prototype)))
      (setf (aref real basis) (%one-like prototype))
      (%make-state n-qubits real imag))))

(defun %state-from-components (state component)
  (let* ((n (%state-n-qubits state))
         (source-real (%state-real-parts state))
         (source-imag (%state-imag-parts state))
         (real (make-array (length source-real)))
         (imag (make-array (length source-imag))))
    (dotimes (i (length real))
      (setf (aref real i) (%s-component (aref source-real i) component)
            (aref imag i) (%s-component (aref source-imag i) component)))
    (%make-state n real imag)))

(defun quantum-state-norm-squared (state)
  (let* ((real (%state-real-parts state))
         (imag (%state-imag-parts state))
         (sum (%zero-like (aref real 0))))
    (dotimes (i (length real) sum)
      (setf sum (%s+ sum
                      (%s+ (%s* (aref real i) (aref real i))
                           (%s* (aref imag i) (aref imag i))))))))

(defun %state-inner-real (left right)
  "Real part of <LEFT|RIGHT>, over the active coefficient ring."
  (unless (and (= (%state-n-qubits left) (%state-n-qubits right))
               (= (length (%state-real-parts left))
                  (length (%state-real-parts right))))
    (%obstruct :dimension-mismatch))
  (let* ((lr (%state-real-parts left)) (li (%state-imag-parts left))
         (rr (%state-real-parts right)) (ri (%state-imag-parts right))
         (sum (%zero-like (aref lr 0))))
    (dotimes (i (length lr) sum)
      ;; Re(conj(l) r) = lr*rr + li*ri.
      (setf sum (%s+ sum
                      (%s+ (%s* (aref lr i) (aref rr i))
                           (%s* (aref li i) (aref ri i))))))))

(defun %state-content (state)
  (list :n-qubits (%state-n-qubits state)
        :real (loop for x across (%state-real-parts state)
                    collect (%scalar-content x))
        :imag (loop for x across (%state-imag-parts state)
                    collect (%scalar-content x))))

(defun %validate-target (state target &optional other)
  (unless (and (integerp target)
               (<= 0 target)
               (< target (%state-n-qubits state))
               (or (null other) (/= target other)))
    (%obstruct :malformed-tape
               :details (list :target target :other other
                              :n-qubits (%state-n-qubits state)))))

(defun %gate-matrix (opcode &optional angle)
  "Return a row-major 2x2 matrix of split-complex coefficients."
  (let* ((zero 0d0) (one 1d0)
         (half (and angle (%s/ angle 2d0)))
         (c (and angle (%s-cos half)))
         (s (and angle (%s-sin half)))
         (a (/ (sqrt 2d0))))
    (flet ((m (a00 a01 a10 a11) (vector a00 a01 a10 a11))
           (z () (%c zero zero)))
      (case opcode
        (:h (m (%c a zero) (%c a zero) (%c a zero) (%c (- a) zero)))
        (:x (m (z) (%c one zero) (%c one zero) (z)))
        (:y (m (z) (%c zero -1d0) (%c zero one) (z)))
        (:z (m (%c one zero) (z) (z) (%c -1d0 zero)))
        (:s (m (%c one zero) (z) (z) (%c zero one)))
        (:t (let ((p (cos (/ pi 4d0))))
              (m (%c one zero) (z) (z) (%c p p))))
        (:rx (m (%c c zero) (%c zero (%s-neg s))
                (%c zero (%s-neg s)) (%c c zero)))
        (:ry (m (%c c zero) (%c (%s-neg s) zero)
                (%c s zero) (%c c zero)))
        (:rz (m (%c c (%s-neg s)) (z) (z) (%c c s)))
        (otherwise
         (%obstruct :unsupported-opcode :details (list :opcode opcode)))))))

(defun %dagger-matrix (matrix)
  (vector (%c-conjugate (aref matrix 0))
          (%c-conjugate (aref matrix 2))
          (%c-conjugate (aref matrix 1))
          (%c-conjugate (aref matrix 3))))

(defun %rotation-derivative-matrix (opcode angle)
  "Exact dU/dANGLE for RX, RY, or RZ at a double-float ANGLE."
  (let* ((half (/ (%coerce-real angle) 2d0))
         (c (cos half)) (s (sin half))
         (dc (* -0.5d0 s)) (ds (* 0.5d0 c))
         (z (%c 0d0 0d0)))
    (case opcode
      (:rx (vector (%c dc 0d0) (%c 0d0 (- ds))
                   (%c 0d0 (- ds)) (%c dc 0d0)))
      (:ry (vector (%c dc 0d0) (%c (- ds) 0d0)
                   (%c ds 0d0) (%c dc 0d0)))
      (:rz (vector (%c dc (- ds)) z z (%c dc ds)))
      (otherwise (%obstruct :internal-differentiation
                            :details (list :opcode opcode))))))

(defun %apply-one-matrix (state matrix target)
  (%validate-target state target)
  (let* ((n (%state-n-qubits state))
         (source-r (%state-real-parts state))
         (source-i (%state-imag-parts state))
         (out-r (copy-seq source-r))
         (out-i (copy-seq source-i))
         (mask (ash 1 (- n 1 target))))
    (dotimes (i (length source-r))
      (when (zerop (logand i mask))
        (let* ((j (logior i mask))
               (a (%c (aref source-r i) (aref source-i i)))
               (b (%c (aref source-r j) (aref source-i j)))
               (r0 (%c+ (%c* (aref matrix 0) a)
                         (%c* (aref matrix 1) b)))
               (r1 (%c+ (%c* (aref matrix 2) a)
                         (%c* (aref matrix 3) b))))
          (setf (aref out-r i) (%c-real r0)
                (aref out-i i) (%c-imag r0)
                (aref out-r j) (%c-real r1)
                (aref out-i j) (%c-imag r1)))))
    (%make-state n out-r out-i)))

(defun %apply-two-gate (state opcode first second)
  (%validate-target state first second)
  (%validate-target state second first)
  (let* ((n (%state-n-qubits state))
         (source-r (%state-real-parts state))
         (source-i (%state-imag-parts state))
         (dimension (length source-r))
         (out-r (make-array dimension))
         (out-i (make-array dimension))
         (first-mask (ash 1 (- n 1 first)))
         (second-mask (ash 1 (- n 1 second))))
    (dotimes (i dimension)
      (let ((destination
              (case opcode
                (:cnot (if (logtest first-mask i)
                           (logxor i second-mask) i))
                (:swap (if (eq (logtest first-mask i)
                               (logtest second-mask i))
                           i (logxor i first-mask second-mask)))
                (:cz i)
                (otherwise
                 (%obstruct :unsupported-opcode
                            :details (list :opcode opcode))))))
        (setf (aref out-r destination)
              (if (and (eq opcode :cz)
                       (logtest first-mask i) (logtest second-mask i))
                  (%s-neg (aref source-r i))
                  (aref source-r i))
              (aref out-i destination)
              (if (and (eq opcode :cz)
                       (logtest first-mask i) (logtest second-mask i))
                  (%s-neg (aref source-i i))
                  (aref source-i i)))))
    (%make-state n out-r out-i)))

(defun %state-close-p (left right &optional (tolerance 1d-11))
  (and (= (%state-n-qubits left) (%state-n-qubits right))
       (let ((a (quantum-state-values left))
             (b (quantum-state-values right)))
         (loop for i below (length a)
               always (<= (abs (- (aref a i) (aref b i))) tolerance)))))
