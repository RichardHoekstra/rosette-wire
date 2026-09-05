;;;; gradients.lisp --- hyper-dual, adjoint, parameter-shift, JVP, and VJP.

(in-package #:rosette-quantum-vm)

(defstruct (gradient-result
            (:constructor %make-gradient-result
                (method value parameters gradient hessian tape observable
                        bindings receipt fingerprint))
            (:conc-name %gradient-)
            (:copier nil))
  (method nil :read-only t)
  (value 0d0 :type double-float :read-only t)
  (parameters nil :read-only t)
  (gradient nil :read-only t)
  (hessian nil :read-only t)
  (tape nil :read-only t)
  (observable nil :read-only t)
  (bindings nil :read-only t)
  (receipt nil :read-only t)
  (fingerprint "" :type string :read-only t))

(defun gradient-result-method (result) (%gradient-method result))
(defun gradient-result-value (result) (%gradient-value result))
(defun gradient-result-parameters (result) (copy-list (%gradient-parameters result)))
(defun gradient-result-gradient (result) (copy-tree (%gradient-gradient result)))

(defun %copy-matrix (matrix)
  (when matrix
    (let* ((dimensions (array-dimensions matrix))
           (copy (make-array dimensions :element-type 'double-float)))
      (dotimes (i (array-total-size matrix) copy)
        (setf (row-major-aref copy i) (row-major-aref matrix i))))))

(defun gradient-result-hessian (result) (%copy-matrix (%gradient-hessian result)))
(defun gradient-result-receipt (result) (copy-tree (%gradient-receipt result)))
(defun gradient-result-fingerprint (result) (copy-seq (%gradient-fingerprint result)))

(defun %parameter-names (tape)
  (loop for (name type) in (rosette-quantum-ir:quantum-tape-parameters tape)
        when (eq type :real) collect name))

(defun %base-bindings (tape bindings)
  (loop for (name . value) in (%normalize-bindings tape bindings)
        collect (cons name (%s-value value))))

(defun %parameter-vector (parameters bindings)
  (let ((vector (make-array (length parameters)
                            :element-type 'double-float)))
    (loop for name in parameters for i from 0 do
      (setf (aref vector i) (cdr (assoc name bindings :test #'eq))))
    vector))

(defun %bindings-from-vector (parameters vector)
  (loop for name in parameters for i from 0
        collect (cons name (aref vector i))))

(defun %energy (tape observable bindings &key shift-index shift-delta)
  (let ((execution (%execute-quantum-tape tape bindings
                                            :shift-index shift-index
                                            :shift-delta shift-delta)))
    (observable-expectation observable (%execution-state execution))))

(defun %gradient-alist (parameters vector)
  (loop for name in parameters for i from 0
        collect (cons name (aref vector i))))

(defun %hyperdual-route (tape observable bindings parameters)
  (if (null parameters)
      (values (%s-value (%energy tape observable bindings)) nil)
      (multiple-value-bind (gradient value)
          (rosette-hyper-dual:hd-gradient
           (lambda (vector)
             (%energy tape observable
                      (%bindings-from-vector parameters vector)))
           (%parameter-vector parameters bindings))
        (values value (%gradient-alist parameters gradient)))))

(defun %parameter-shift-route (tape observable bindings parameters)
  (let* ((base (%execute-quantum-tape tape bindings))
         (value (%s-value (observable-expectation observable
                                                  (%execution-state base))))
         (accumulator (make-hash-table :test #'eq)))
    (dolist (name parameters) (setf (gethash name accumulator) 0d0))
    ;; One occurrence at a time is essential when a root parameter is shared.
    (dolist (step (%execution-trace base))
      (let ((root (%gate-step-parameter-root step)))
        (when root
          (let* ((index (%gate-step-index step))
                 (plus (%s-value
                        (%energy tape observable bindings
                                 :shift-index index :shift-delta (/ pi 2d0))))
                 (minus (%s-value
                         (%energy tape observable bindings
                                  :shift-index index :shift-delta (- (/ pi 2d0)))))
                 (contribution (* 0.5d0 (- plus minus))))
            (incf (gethash root accumulator 0d0) contribution)))))
    (values value
            (loop for name in parameters
                  collect (cons name (gethash name accumulator 0d0))))))

(defun %adjoint-route (tape observable bindings parameters)
  (%adjoint-value-gradient (%execute-quantum-tape tape bindings)
                           observable parameters))

(defun %matrix-content (matrix)
  (and matrix
       (loop for i below (array-dimension matrix 0)
             collect (loop for j below (array-dimension matrix 1)
                           collect (aref matrix i j)))))

(defun %build-gradient-result (method value parameters gradient hessian
                               tape observable bindings)
  (let* ((receipt
           (list :version 1
                 :method method
                 :tape (rosette-quantum-ir:quantum-tape-fingerprint tape)
                 :observable (pauli-sum-fingerprint observable)
                 :bindings (%canonical-bindings bindings)
                 :value value
                 :parameters (copy-list parameters)
                 :gradient (copy-tree gradient)
                 :hessian (%matrix-content hessian)))
         (fingerprint (%stable-fingerprint receipt)))
    (%make-gradient-result method value (copy-list parameters)
                           (copy-tree gradient) (%copy-matrix hessian)
                           tape observable (copy-list bindings)
                           receipt fingerprint)))

(defun value-and-gradient (tape observable bindings &key (method :adjoint))
  "Return an immutable value/gradient result using METHOD.

METHOD is :HYPER-DUAL, :ADJOINT, or :PARAMETER-SHIFT.  All routes consume the
same immutable tape and return gradients in tape-parameter order."
  (unless (pauli-sum-p observable)
    (%obstruct :malformed-observable :details (list :observable observable)))
  (let* ((base (%base-bindings tape bindings))
         (parameters (%parameter-names tape)))
    (multiple-value-bind (value gradient)
        (case method
          (:hyper-dual (%hyperdual-route tape observable base parameters))
          (:adjoint (%adjoint-route tape observable base parameters))
          (:parameter-shift
           (%parameter-shift-route tape observable base parameters))
          (otherwise
           (%obstruct :unsupported-method :details (list :method method))))
      (%build-gradient-result method (%coerce-real value) parameters gradient nil
                              tape observable base))))

(defun hyperdual-gradient-hessian (tape observable bindings)
  "Return exact gradient and symmetric Hessian from hyper-dual seed batching."
  (let* ((base (%base-bindings tape bindings))
         (parameters (%parameter-names tape))
         (point (%parameter-vector parameters base)))
    (if (null parameters)
        (%build-gradient-result
         :hyper-dual (%s-value (%energy tape observable base)) nil nil
         (make-array '(0 0) :element-type 'double-float)
         tape observable base)
        (multiple-value-bind (gradient hessian value)
            (rosette-hyper-dual:hd-grad-hessian
             (lambda (vector)
               (%energy tape observable
                        (%bindings-from-vector parameters vector)))
             point)
          (%build-gradient-result :hyper-dual value parameters
                                  (%gradient-alist parameters gradient) hessian
                                  tape observable base)))))

(defun hessian-vector-product (result vector)
  "Multiply RESULT's stored exact Hessian by VECTOR."
  (let ((hessian (%gradient-hessian result)))
    (unless hessian
      (%obstruct :missing-hessian
                 :details (list :method (%gradient-method result))))
    (unless (= (length vector) (array-dimension hessian 1))
      (%obstruct :dimension-mismatch
                 :details (list :vector (length vector)
                                :hessian (array-dimensions hessian))))
    (let* ((n (array-dimension hessian 0))
           (out (make-array n :element-type 'double-float
                              :initial-element 0d0)))
      (dotimes (i n out)
        (dotimes (j n)
          (incf (aref out i) (* (aref hessian i j)
                                (%coerce-real (elt vector j)))))))))

(defun %tangent-vector (parameters tangent)
  (unless (%proper-list-p tangent)
    (%obstruct :malformed-binding :details (list :tangent tangent)))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (entry tangent)
      (unless (and (consp entry) (symbolp (car entry)))
        (%obstruct :malformed-binding :details (list :tangent-entry entry)))
      (unless (member (car entry) parameters :test #'eq)
        (%obstruct :malformed-binding :details (list :unknown-tangent (car entry))))
      (when (gethash (car entry) seen)
        (%obstruct :malformed-binding :details (list :duplicate-tangent (car entry))))
      (setf (gethash (car entry) seen) t)))
  (loop for name in parameters collect
    (let ((entry (assoc name tangent :test #'eq)))
      (if entry
          (let ((value (%binding-entry-value entry)))
            (unless (realp value)
              (%obstruct :malformed-binding
                         :details (list :tangent name :value value)))
            (%coerce-real value))
          0d0))))

(defun state-jvp (tape bindings tangent)
  "Return (values PRIMAL TANGENT) for the state map at parameter TANGENT."
  (let* ((base (%base-bindings tape bindings))
         (parameters (%parameter-names tape))
         (directions (%tangent-vector parameters tangent))
         (seeded
           (loop for (name . value) in base
                 for direction in directions
                 collect (cons name
                               (rosette-hyper-dual:make-hd value direction 0d0 0d0))))
         (state (%execution-state (%execute-quantum-tape tape seeded))))
    (values (%state-from-components state :value)
            (%state-from-components state :d1))))

(defun state-vjp (tape bindings cotangent)
  "Return the state-map VJP for a quantum state or complex amplitude vector."
  (let* ((base (%base-bindings tape bindings))
         (parameters (%parameter-names tape))
         (execution (%execute-quantum-tape tape base))
         (state (%execution-state execution))
         (cotangent-state
           (%coerce-state-input cotangent :state-vjp (%state-n-qubits state))))
    (%reverse-gradient execution cotangent-state parameters 1d0)))
