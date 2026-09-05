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


(defsystem #:graph-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "CSR adjacency primitive with BFS, DFS, SCC, and edge-attribute storage."
  :version
  "0.1.0"
  :depends-on
  (#:array-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "csr") (:file "traversal") (:file "scc"))
  :in-order-to
  ((test-op (test-op #:graph-core/tests))))


(defsystem #:graph-core/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for graph-core."
  :depends-on
  (#:graph-core #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-graph-core/tests :run-all-tests)))
