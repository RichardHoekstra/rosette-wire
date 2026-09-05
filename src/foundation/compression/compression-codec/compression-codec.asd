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


(defsystem #:compression-codec
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Lossless compression encoders: canonical Huffman coding, LZ77, a DEFLATE (RFC 1951) encoder producing rosette-inflate-decodable output, and a byte-oriented range/arithmetic coder."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "bitstream") (:file "huffman") (:file "lz77")
   (:file "deflate") (:file "range-coder")))
