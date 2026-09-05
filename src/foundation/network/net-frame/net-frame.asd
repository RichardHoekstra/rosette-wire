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


(defsystem #:net-frame
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Network data-path kernel: Ethernet II + IPv4 header build/parse, the IPv4 one's-complement (internet) checksum, and CRC32 (Ethernet FCS), with frame<->deframe round-trip."
  :version
  "0.1.0"
  :depends-on
  (#:byte-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:net-frame/tests))))


(defsystem #:net-frame/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for net-frame."
  :depends-on
  (#:net-frame)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-net-frame/tests :run-all-tests)))
