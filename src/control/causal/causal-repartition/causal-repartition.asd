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


(defsystem #:causal-repartition
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The physical-layer rebalancer that flows under the logical gravity-well tree: which machine runs which piece, rebalanced on load, invisible to the player and (because the logical outcome is partition-invariant) a gauge on the world. Load is measured as CAUSAL-EVENT-RATE (adjudications/time = the Delta_der residue two objects force), not occupancy. The min-cut cost model: net gain of a cut S|T at latency-ratio lambda = min(internal(S),internal(T)) - (lambda-1)*cut(S,T) -- parallelism won minus the excess price of severed edges. This is a LOCATED PHASE TRANSITION: a barbell (dense clusters, thin bridge) splits, a dense clique does not, the boundary is bridge = internal/(lambda-1), and lambda is its sharpness. Measurement is local per seam. A two-threshold HYSTERESIS controller turns the sharp transition into a discrete, HLC-stamped, replayable SWITCH (split above theta-hi, merge below theta-lo, hold between) that provably collapses the flapping a single threshold suffers. Deps: rosette-causal-frontier."
  :version
  "0.1.0"
  :depends-on
  (#:causal-frontier)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "causal-repartition"))
  :in-order-to
  ((test-op (test-op #:causal-repartition/tests))))


(defsystem #:causal-repartition/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for causal-repartition: min-cut finds the thin bridge of a barbell and splits it; a dense clique is not split; the phase boundary sits at internal/(lambda-1) and sharpens with lambda; seam gain is local (a disconnected cluster does not change it); the hysteresis controller collapses flapping that a naive single threshold suffers, and its event stream is deterministic/replayable."
  :depends-on
  (#:causal-repartition #:causal-frontier #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-causal-repartition/tests :run-all-tests)))
