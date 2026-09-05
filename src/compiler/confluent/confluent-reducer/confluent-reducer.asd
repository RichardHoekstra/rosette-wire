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


(defsystem #:confluent-reducer
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A certified-parallel functional reducer over rosette-core-term where PARALLELISM IS A GAUGE, not a hope: every reduction emits its own proof.  It reduces the accumulator/op fragment of a core-term (a let-chain of `(+ cell lit)` and `lit` updates on named cells), extracts the per-cell op-stream, and uses rosette-confluence-frontier to partition the redexes into the CONFLUENT set (order-independent runs of commuting ops -> fire as a parallel batch, any schedule reaches the same partial result) and the NON-CONFLUENT frontier (a forgetting `:set` reset that does not commute -> the irreducibly-serial residue).  The parallel schedule is sealed as a gauge by rosette-causal-frontier PARTITION-INVARIANCE (moving cells across arbiters is invariant IFF no cell's op-stream is split), and rosette-causal-repartition's min-cut cost model quantifies the cross-cell parallel width (the located split/no-split phase transition at critical-bridge).  It inherits rosette-core-term's coincidence: the reduced value == core-term's own EVAL == its normalizer's normal form, and the whole run is sealed into a re-runnable rosette-proof-witness certificate.  HONEST SCOPE: `parallel` means PROVABLY ORDER-INDEPENDENT (a gauge), not OS-threaded execution; the classification is exact over free permutations of the one-cell :add/:set algebra (a causally-final forgetting :set reads as a frontier -- the rosette-forgetting-frontier subtlety); general pure core-term redexes are already fully confluent (empty non-confluent frontier)."
  :version
  "0.1.0"
  :depends-on
  (#:core-term #:confluence-frontier #:causal-frontier #:causal-repartition
   #:proof-witness)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:confluent-reducer/tests))))


(defsystem #:confluent-reducer/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Mechanism-proof gate for confluent-reducer: one core-term with BOTH confluent runs and non-confluent `:set` frontiers where (a) the confluent redexes fire in ANY order to the SAME normal form, (b) the non-confluent frontier is located as the serial residue, (c) the parallel-schedule value == the serial normal form sealed by a partition-invariance gauge certificate, (d) the reduced value == rosette-core-term's own EVAL; the run re-runs to T and tampering is caught."
  :depends-on
  (#:confluent-reducer)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-confluent-reducer/tests :run-all-tests)))
