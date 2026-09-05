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


(defsystem #:vector-font
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Single-stroke (Hershey roman-simplex) vector font as pure 2-D polyline geometry: char/string -> stroke lists in a normalized baseline frame, with advance/cap-height metrics, scaling, rotation and bounds. Covers printable ASCII 32-126 plus the Greek alphabet keyed by Unicode code point (a literal Greek char renders directly). ZERO dependencies and NO rasterization -- every backend (AA raster stroker, SVG <path>, GPU line-list, pen-plotter) consumes the same gauge-invariant strokes. Glyph data generated from the public-domain Hershey font set (rowmans + greekc)."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "glyphs") (:file "font"))
  :in-order-to
  ((test-op (test-op #:vector-font/tests))))


(defsystem #:vector-font/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for vector-font."
  :depends-on
  (#:vector-font #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-vector-font/tests :run-all-tests)))
