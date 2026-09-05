;;;; rosette-proof-witness/src/lean-witness.lisp --- CL handles for formal proofs.

(in-package #:rosette-proof-witness)

(defstruct (lean-witness
            (:conc-name lean-witness-)
            (:predicate lean-witness-p)
            (:constructor %make-lean-witness)
            (:copier nil)
            (:print-function
             (lambda (obj stream depth)
               (declare (ignore depth))
               (format stream "#<LEAN-WITNESS ~A>"
                       (lean-witness-theorem-name obj)))))
  "A handle bridging a Common Lisp claim to a Lean-side formal proof.

THEOREM-NAME is a symbol naming the theorem. LEAN-PATH is NIL or a
string naming the Lean file that states and proves it. TEST-FN is a
zero-argument function returning true iff the theorem's content holds
against the current executable model."
  (theorem-name nil :type symbol :read-only t)
  (lean-path    nil :type (or null string) :read-only t)
  (test-fn      nil :type function :read-only t))

(defun make-lean-witness (theorem-name test-fn &key lean-path)
  "Construct a LEAN-WITNESS."
  (check-type theorem-name symbol)
  (check-type test-fn function)
  (check-type lean-path (or null string))
  (%make-lean-witness :theorem-name theorem-name
                      :lean-path lean-path
                      :test-fn test-fn))

(defparameter *lean-witness-registry* (make-hash-table :test #'eq)
  "Global registry of Lean witnesses indexed by theorem-name symbols.")

(defun register-lean-witness (witness)
  "Register WITNESS in *LEAN-WITNESS-REGISTRY*. Returns WITNESS."
  (check-type witness lean-witness)
  (setf (gethash (lean-witness-theorem-name witness) *lean-witness-registry*)
        witness)
  witness)

(defun find-lean-witness (theorem-name)
  "Look up THEOREM-NAME in *LEAN-WITNESS-REGISTRY*, or return NIL."
  (gethash theorem-name *lean-witness-registry*))
