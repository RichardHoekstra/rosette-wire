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


(defsystem #:sexpr-dag
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Hash-consed s-expression DAGs with CSE metrics and source provenance."
  :version
  "0.1.0"
  :depends-on
  (#:scalar-core #:string-escape #:graph-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "dag") (:file "reader") (:file "analysis")
   (:file "analysis-shape-predicates")
   (:file "analysis-shape-binding-predicates")
   (:file "analysis-shape-domain-predicates") (:file "analysis-shape")
   (:file "analysis-shape-roles") (:file "analysis-summary")
   (:file "analysis-isomorphism") (:file "analysis-concepts")
   (:file "analysis-hotspots") (:file "analysis-extraction")
   (:file "analysis-profiles") (:file "analysis-printers"))
  :in-order-to
  ((test-op (test-op #:sexpr-dag/tests))))


(defsystem #:sexpr-dag/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for sexpr-dag."
  :depends-on
  (#:sexpr-dag)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-sexpr-dag/tests :run-all-tests)))
