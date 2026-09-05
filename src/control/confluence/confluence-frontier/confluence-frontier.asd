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


(defsystem #:confluence-frontier
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The boundary between order-free (symbolic / verifiable) and measure-requiring (neuro / measurement) computation. A local rewrite system is symbolic exactly where it is confluent: concurrent redexes commute, so the normal form is order-independent -- no consensus and no tiebreak, a verifier decides it for free. It becomes neuro exactly at a non-confluent redex whose outcome depends on order: no local fact selects the branch, so an external MEASURE (Born rule / learned amplitude / sampler) must supply it. measurement-needed-p is that frontier. The neurosymbolic answer read off the confluence structure: confluent => symbolic (unique-result, verify-only); non-confluent => neuro (resolve-with-measure). Honest subtlety: order-freedom has TWO sources -- commuting (clean) and forgetting (a :set reset that erases the branch distinction, a projective measurement) -- so confluence is SUFFICIENT but not NECESSARY for tiebreak-freedom (converse witnessed). Reuses rosette-causal-frontier's op algebra (does not re-implement semantics). Deps: rosette-causal-frontier. Composes with rosette-code-holography (curvature=confluence) and rosette-cognitive-tape (trace as confluence object)."
  :version
  "0.1.0"
  :depends-on
  (#:causal-frontier)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "confluence-frontier"))
  :in-order-to
  ((test-op (test-op #:confluence-frontier/tests))))


(defsystem #:confluence-frontier/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for confluence-frontier: confluent (add-only) => order-independent and tiebreak-free; non-confluent (add+set) => order-dependent and the tiebreak is load-bearing; a non-confluent-but-order-independent case (trailing :set forgets) witnesses that confluence is sufficient not necessary; unique-result is defined only on the symbolic side; resolve-with-measure needs a measure and different measures diverge on the neuro side; verify-resolution gates a chosen branch even where it could not be chosen."
  :depends-on
  (#:confluence-frontier #:causal-frontier #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-confluence-frontier/tests :run-all-tests)))
