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


(defsystem #:core-term
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "ONE typed first-order core term language (integers + let + if + arithmetic + bounded recursion) carrying THREE coincidences on the SAME term: (1) EVAL -- a direct interpreter; (2) PROVE -- a normalizer whose reduction EMITS a proof-carrier-core node per step, sealed into a re-runnable proof-witness certificate, so NORMALIZING PRODUCES THE CERTIFICATE (definitional equality decided by normal-form identity); (3) COMPILE -- the same term lowered (let-substituted) to rosette-lisp-codegen integer Lisp, assembled to bytecode, run on the rosette-gpu-kernel-dsl kernel-spec VM through the CPU oracle (interpret-kernel-spec), BIT-EXACT against the evaluator.  eval and prove are DERIVED here; the silicon lowering is BRIDGED through the proven rosette-lisp-codegen -> kernel-IR path.  A corrupted lowering is caught by the bit-check; two beta-equal terms share a normal form and two in-equal terms do not; a forged certificate fails its re-run."
  :version
  "0.1.0"
  :depends-on
  (#:proof-carrier-core #:proof-witness #:lisp-codegen #:gpu-kernel-dsl
   #:gpu-kernel-dsl/cpu)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "term") (:file "normalize") (:file "lower")
   (:file "reduce") (:file "selfeval") (:file "selfeval-f"))
  :in-order-to
  ((test-op (test-op #:core-term/tests))))


(defsystem #:core-term/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Acceptance gate for core-term: one term (Horner poly / bounded-rec sum) where eval = normalized value = CPU-oracle-of-lowered-kernel bit-exactly; the certificate re-runs; beta-equal terms share a normal form; negative gates (corrupted lowering, forged certificate, in-equal terms, ill-typed term) all have teeth."
  :depends-on
  (#:core-term)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-core-term/tests :run-all-tests)))
