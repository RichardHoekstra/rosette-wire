;;;; observable.lisp --- Pauli sums over the dense split-complex carrier.

(in-package #:rosette-quantum-vm)

(defstruct (pauli-sum
            (:constructor %make-pauli-sum (terms n-qubits fingerprint))
            (:conc-name %pauli-)
            (:copier nil))
  (terms nil :read-only t)
  (n-qubits 1 :type (integer 1 *) :read-only t)
  (fingerprint "" :type string :read-only t))

(defun %term-parts (term)
  (cond
    ((and (consp term) (stringp (cdr term)))
     (values (car term) (cdr term)))
    ((and (%proper-list-p term) (= (length term) 2))
     (values (first term) (second term)))
    (t (%obstruct :malformed-observable :details (list :term term)))))

(defun make-pauli-sum (terms)
  "Construct an immutable real Pauli sum from (COEFFICIENT . \"IXYZ\") terms."
  (unless (and (%proper-list-p terms) terms)
    (%obstruct :malformed-observable :details (list :terms terms)))
  (let ((normalized nil) (width nil))
    (dolist (term terms)
      (multiple-value-bind (coefficient word) (%term-parts term)
        (unless (realp coefficient)
          (%obstruct :malformed-observable
                     :details (list :coefficient coefficient)))
        (unless (and (stringp word) (plusp (length word))
                     (every (lambda (character)
                              (find character "IXYZ" :test #'char=))
                            word))
          (%obstruct :malformed-observable :details (list :pauli word)))
        (if width
            (unless (= width (length word))
              (%obstruct :malformed-observable
                         :details (list :widths (list width (length word)))))
            (setf width (length word)))
        (push (cons (%coerce-real coefficient) (copy-seq word)) normalized)))
    (setf normalized (nreverse normalized))
    (%make-pauli-sum normalized width
                     (%stable-fingerprint
                      (loop for (coefficient . word) in normalized
                            collect (list coefficient word))))))

(defun pauli-sum-terms (observable)
  (loop for (coefficient . word) in (%pauli-terms observable)
        collect (cons coefficient (copy-seq word))))

(defun pauli-sum-n-qubits (observable) (%pauli-n-qubits observable))
(defun pauli-sum-fingerprint (observable) (copy-seq (%pauli-fingerprint observable)))

(defun %phase-for-pauli (word source n-qubits)
  (let ((destination source)
        (phase (%c 1d0 0d0)))
    (dotimes (qubit n-qubits)
      (let* ((mask (ash 1 (- n-qubits 1 qubit)))
             (one-p (logtest mask source)))
        (case (char word qubit)
          (#\I nil)
          (#\X (setf destination (logxor destination mask)))
          (#\Z (when one-p
                 (setf phase (%c* phase (%c -1d0 0d0)))))
          (#\Y
           (setf destination (logxor destination mask)
                 phase (%c* phase (if one-p
                                      (%c 0d0 -1d0)
                                      (%c 0d0 1d0))))))))
    (values destination phase)))

(defun %apply-pauli-sum (observable state)
  (unless (= (%pauli-n-qubits observable) (%state-n-qubits state))
    (%obstruct :dimension-mismatch
               :details (list :observable (%pauli-n-qubits observable)
                              :state (%state-n-qubits state))))
  (let* ((source-r (%state-real-parts state))
         (source-i (%state-imag-parts state))
         (dimension (length source-r))
         (prototype (aref source-r 0))
         (out-r (make-array dimension))
         (out-i (make-array dimension)))
    (dotimes (i dimension)
      (setf (aref out-r i) (%zero-like prototype)
            (aref out-i i) (%zero-like prototype)))
    (dolist (term (%pauli-terms observable))
      (let ((coefficient (car term)) (word (cdr term)))
        (dotimes (source dimension)
          (multiple-value-bind (destination phase)
              (%phase-for-pauli word source (%state-n-qubits state))
            (let* ((amplitude (%c (aref source-r source)
                                  (aref source-i source)))
                   (contribution (%c* (%c coefficient 0d0)
                                      (%c* phase amplitude))))
              (setf (aref out-r destination)
                    (%s+ (aref out-r destination) (%c-real contribution))
                    (aref out-i destination)
                    (%s+ (aref out-i destination) (%c-imag contribution))))))))
    (%make-state (%state-n-qubits state) out-r out-i)))

(defun observable-expectation (observable state)
  "Expectation for a VM state or checked complex amplitude vector."
  (unless (pauli-sum-p observable)
    (%obstruct :malformed-observable))
  (let ((coerced
          (%coerce-state-input state :observable-expectation
                               (%pauli-n-qubits observable))))
    (%state-inner-real coerced (%apply-pauli-sum observable coerced))))
