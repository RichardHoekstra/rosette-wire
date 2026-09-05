;;;; ir.lisp --- ProgramIR carrier and report data.

(in-package #:rosette-quantum-ir)

(defstruct (diagnostic
            (:constructor %make-diagnostic (&key kind path message)))
  "One deterministic admission failure at PATH in the projected source."
  kind
  (path nil :type list)
  (message "" :type string))

(defstruct (program-report
            (:constructor %make-program-report
                (&key ok-p kind definitions programs resources errors)))
  "Admission result for one quantum compilation unit.

DEFINITIONS and PROGRAMS are compact property-list summaries.  ERRORS is a
list of DIAGNOSTIC values in source order."
  (ok-p nil :type boolean)
  kind
  (definitions nil :type list)
  (programs nil :type list)
  (resources nil :type list)
  (errors nil :type list))

(defstruct (resource-certificate
            (:constructor %make-resource-certificate
                (&key name type profile structural-rules defined-at transition
                      derivations)))
  "Proof-carrying structural assignment for one SSA value."
  name
  type
  profile
  (structural-rules nil :type list)
  (defined-at nil :type list)
  transition
  (derivations nil :type list))

(defun %proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %resource-name-p (object)
  "SSA value names are ordinary non-keyword symbols."
  (and (symbolp object)
       object
       (not (keywordp object))
       (not (member object '(t nil)))))

(defun %value-type-p (type)
  (or (member type '(:bit :real))
      (and (%proper-list-p type)
           (= (length type) 2)
           (eq (first type) :qstate)
           (integerp (second type))
           (plusp (second type)))))

(defun %quantum-type-p (type)
  (and (consp type) (eq (first type) :qstate)))

(defun make-quantum-program (source)
  "Lift quantum SOURCE onto the repository's single ProgramIR carrier.

SOURCE must be a :QUANTUM-UNIT or legacy :QPROGRAM s-expression.  Passing an
existing ProgramIR value is idempotent; admission remains CHECK-PROGRAM's job."
  (if (rosette-program-ir:programp source)
      source
      (rosette-program-ir:lisp->program (copy-tree source))))

(defun quantum-program-p (object)
  "True when OBJECT is ProgramIR carrying :QUANTUM-UNIT or legacy :QPROGRAM."
  (and (rosette-program-ir:programp object)
       (handler-case
           (let ((form (rosette-program-ir:program->lisp object)))
             (and (%proper-list-p form)
                  (member (first form) '(:quantum-unit :qprogram))))
         (error () nil))))

(defun program-form (program)
  "Project PROGRAM to a fresh dialect s-expression."
  (unless (quantum-program-p program)
    (error "Expected a quantum ProgramIR carrier, got ~S" program))
  (copy-tree (rosette-program-ir:program->lisp program)))
