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


(defsystem #:wire-diagnostics
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Replayable diagnostic bundles for Eshkol execution-floor and Moonlab public-ABI campaigns."
  :version
  "0.1.0"
  :depends-on
  (#:front-door/eshkol #:content-identity #:json #:quantum-ir #:quantum-vm
   #:wire-graph)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "wire-diagnostics") (:file "moonlab-diagnostics")
   (:file "moonlab-surfaces") (:file "generated-campaigns")
   (:file "wire-adapters") (:file "cross-boundary")
   (:static-file "../tools/moonlab-abi-probe.c")
   (:static-file "../corpus/manifest.json")
   (:static-file "../corpus/eshkol-no-stdlib-aot-link.json")
   (:static-file "../corpus/moonlab-qgt-nband-sign.json"))
  :in-order-to
  ((test-op (test-op #:wire-diagnostics/tests))))


(defsystem #:wire-diagnostics/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Identity, replay, disagreement, refusal, and privacy gates for Wire diagnostics."
  :depends-on
  (#:wire-diagnostics #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-wire-diagnostics/tests :run-all-tests)))
