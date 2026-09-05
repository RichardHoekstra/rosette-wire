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


(defsystem #:game-protocol
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The deterministic-replay contract every Rosette game backend is an instance of. Five generic functions -- game-advance (one tick under one opaque input), game-hash (deterministic state fingerprint), game-tick (integer tick), and the game-capture/game-restore snapshot pair that abstracts over the functional-vs-mutating gauge -- plus one portable verifier: run-game records a captured initial world, per-tick inputs, and a fingerprint trace; replay-p restores and re-runs, proving identical fingerprints. A backend conforms by supplying methods; capture/restore default to identity so a purely functional stepper is conformant with no extra code. Zero dependencies; this is the gauge-invariant that rosette-minecraft-clone and rosette-game-engine each stopped re-implementing."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "protocol"))
  :in-order-to
  ((test-op (test-op #:game-protocol/tests))))


(defsystem #:game-protocol/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for game-protocol: a functional toy backend and a mutating toy backend both satisfy replay-p; capture/restore isolates a run from later mutation; replay-p is falsified by a nondeterministic advance; capture/restore default to identity."
  :depends-on
  (#:game-protocol #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-game-protocol/tests :run-all-tests)))
