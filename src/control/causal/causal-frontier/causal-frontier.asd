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


(defsystem #:causal-frontier
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The causal partial order whose totally-ordered quotient is rosette-game-protocol. A global tick is a consensus artifact (TiDi -- pause the world so everyone agrees who was first); the honest object is a causal partial order: events carry hybrid logical clocks (causality + a near-physical-time bound in bounded size) and explicit causal dependencies (a reactive frontier), genuinely-concurrent events have no world-fact about their order and are settled only by a deterministic HLC-then-id tiebreak, and each state cell is adjudicated by exactly one arbiter so contention stays local. Two laws are gate-proved: QUOTIENT -- adjudicating the causal order equals rosette-game-protocol:run-game over any deterministic linearization; PARTITION-INVARIANCE -- the per-cell final state is independent of the arbiter partition provided no cell's op-stream is split across arbiters, so physical repartition along cell boundaries is a gauge (the switch is sound; a boundary that sweeps through one arbiter is the falsifier). Deps: rosette-game-protocol."
  :version
  "0.1.0"
  :depends-on
  (#:game-protocol)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "causal-frontier"))
  :in-order-to
  ((test-op (test-op #:causal-frontier/tests))))


(defsystem #:causal-frontier/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for causal-frontier: HLC order + happens-before + concurrency; deterministic linearization; QUOTIENT (adjudicate == run-game over the linearization) and PARTITION-INVARIANCE (coarse one-arbiter == fine per-cell) over non-commutative ops; a boundary that splits one cell across arbiters is rejected."
  :depends-on
  (#:causal-frontier #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-causal-frontier/tests :run-all-tests)))
