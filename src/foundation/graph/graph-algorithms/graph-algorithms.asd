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


(defsystem #:graph-algorithms
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Classic graph algorithms (topological sort, connected components, MST, max-flow/min-cut, centrality) over rosette-graph-core CSR adjacency."
  :version
  "0.1.0"
  :depends-on
  (#:graph-core #:union-find-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "topological-sort") (:file "components")
   (:file "mst") (:file "max-flow") (:file "centrality"))
  :in-order-to
  ((test-op (test-op #:graph-algorithms/tests))))


(defsystem #:graph-algorithms/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for graph-algorithms."
  :depends-on
  (#:graph-algorithms #:graph-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-graph-algorithms/tests :run-all-tests)))
