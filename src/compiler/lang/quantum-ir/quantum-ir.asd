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


(defsystem #:quantum-ir
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Proof-carrying substructural quantum ProgramIR: every SSA value has a replayable canonical usage profile, measurement emits unrestricted classical data plus linear quantum state, explicit COPY/DISCARD boundaries are type-licensed, and deterministic semantic tapes preserve the effects."
  :version
  "0.1.0"
  :depends-on
  (#:program-ir #:substructural)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "ir") (:file "typecheck") (:file "elaborate")
   (:file "certification") (:file "tape")))
