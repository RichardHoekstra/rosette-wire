;;;; adjoint.lisp --- reverse observable and state costates over gate traces.

(in-package #:rosette-quantum-vm)

(defun %backpropagate-gate (costate step)
  (let ((opcode (%gate-step-opcode step))
        (targets (%gate-step-targets step)))
    (cond
      ((member opcode '(:cnot :cz :swap))
       ;; These permutation/phase gates are self-adjoint.
       (%apply-two-gate costate opcode (first targets) (second targets)))
      (t
       (let* ((angle (and (%gate-step-angle step)
                          (%s-value (%gate-step-angle step))))
              (matrix (%gate-matrix opcode angle)))
         (%apply-one-matrix costate (%dagger-matrix matrix)
                            (first targets)))))))

(defun %reverse-gradient (execution initial-costate parameters factor)
  "Reverse one real costate; FACTOR is 2 for <psi|O|psi>, 1 for a state VJP."
  (let ((costate initial-costate)
        (accumulator (make-hash-table :test #'eq)))
    (dolist (name parameters) (setf (gethash name accumulator) 0d0))
    (dolist (step (reverse (%execution-trace execution)))
      (let ((root (%gate-step-parameter-root step)))
        (when root
          (let* ((derivative
                   (%rotation-derivative-matrix
                    (%gate-step-opcode step)
                    (%s-value (%gate-step-angle step))))
                 (direction
                   (%apply-one-matrix (%gate-step-before step) derivative
                                      (first (%gate-step-targets step))))
                 (contribution
                   (* factor (%s-value (%state-inner-real costate direction)))))
            (incf (gethash root accumulator 0d0) contribution))))
      (setf costate (%backpropagate-gate costate step)))
    (loop for name in parameters
          collect (cons name (gethash name accumulator 0d0)))))

(defun %adjoint-value-gradient (execution observable parameters)
  "Exact observable adjoint: forward state cache, backward O|psi> costate."
  (let* ((state (%execution-state execution))
         (value (%s-value (observable-expectation observable state)))
         (costate (%apply-pauli-sum observable state)))
    (values value (%reverse-gradient execution costate parameters 2d0))))
