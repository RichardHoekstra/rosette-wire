#.(progn (require :asdf) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((self-dir (make-pathname :defaults *load-pathname* :name nil :type nil))
         (root (loop for dir = self-dir
                     then (make-pathname :directory (butlast (pathname-directory dir)) :defaults dir)
                     while (cdr (pathname-directory dir))
                     when (probe-file (merge-pathnames ".rosette-wire-root" dir)) return dir)))
    (if root
        (asdf:initialize-source-registry `(:source-registry (:tree ,root) :ignore-inherited-configuration))
        (pushnew self-dir asdf:*central-registry* :test #'equal))))

(in-package :asdf-user)


(defsystem #:front-door
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A named, deliberately small front door over the existing core-term spine: one surface program is parsed, represented in ProgramIR, evaluated, lowered to the shared kernel VM, oracle-gated, and sealed in a re-runnable certificate. It also exposes an honest common closed STLC fragment with independently checked core/FinSet-CCC/BLC meanings, an exact integral expression-core bridge, and an optional ProgramIR-to-Eshkol JIT/AOT execution floor."
  :version
  "0.1.0"
  :depends-on
  (#:core-term #:program-ir #:ccc #:blc #:blc-reduce #:expression-core
   #:proof-witness)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core") (:file "terms"))
  :in-order-to
  ((test-op (test-op #:front-door/tests) (test-op #:front-door/eshkol/tests))))


(defsystem #:front-door/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Executable gates for surface/ProgramIR round-trip, eval/lower/oracle agreement, re-runnable certificates, the shared core/CCC/BLC fragment, and exact CF-expression admission/refusal."
  :depends-on
  (#:front-door #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op component) (declare (ignore op component))
   (symbol-call :rosette-front-door/tests :run-all-tests)))


(defsystem #:front-door/eshkol
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Optional ProgramIR-to-Eshkol backend for the validated front-door integer dialect. It emits deterministic Scheme, executes true cache-disabled LLVM JIT and explicit AOT under rosette-isolated-worker ceilings, and compares both results with the direct core evaluator and canonical kernel-VM oracle."
  :version
  "0.1.0"
  :depends-on
  (#:front-door #:program-ir #:isolated-worker #:tool-envelope
   #:content-identity)
  :pathname
  "src/"
  :components
  ((:file "eshkol"))
  :in-order-to
  ((test-op (test-op #:front-door/eshkol/tests))))


(defsystem #:front-door/eshkol/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Pure emission/admission gates plus optional real Eshkol JIT/AOT differential checks for the front-door ProgramIR dialect."
  :depends-on
  (#:front-door/eshkol #:program-ir #:isolated-worker)
  :pathname
  "tests/"
  :components
  ((:file "eshkol-tests") (:static-file "fixtures/fake-eshkol.sh"))
  :perform
  (test-op (op component) (declare (ignore op component))
   (symbol-call :rosette-front-door/eshkol/tests :run-all-tests)))
