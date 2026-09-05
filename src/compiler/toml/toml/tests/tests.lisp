;;;; tests.lisp --- rosette-toml laws.

(defpackage #:rosette-toml/tests
  (:use #:cl #:rosette-toml)
  (:import-from #:rosette-assert-core #:assert-true #:assert-close)
  (:export #:run-all-tests))

(in-package #:rosette-toml/tests)

(defun run-all-tests ()
  (let* ((text "title = \"flow case\"
enabled = true
[grid]
nx = 48
dx = 0.5
[solver]
aspects = [0.8, 1.0, 1.6]
")
         (document (parse-toml text)))
    (assert-true (string= "flow case" (toml-get document "title"))
                 "root string")
    (assert-true (eq t (toml-get document "enabled")) "boolean")
    (assert-true (= 48 (toml-get document "grid.nx")) "nested integer")
    (assert-close 0.5d0 (toml-get document "grid.dx") "float" 1d-14)
    (assert-true (= 3 (length (toml-get document "solver.aspects")))
                 "homogeneous array")
    (let ((roundtrip
            (with-output-to-string (stream) (write-toml document stream))))
      (assert-true
       (= 48 (toml-get (parse-toml roundtrip) "grid.nx"))
       "writer round trip"))
    (assert-true
     (handler-case (progn (parse-toml "x = [1, \"two\"]") nil)
       (toml-error () t))
     "mixed arrays rejected")
    (assert-true
     (equal '("a" "different-length")
            (toml-get (parse-toml "x = [\"a\", \"different-length\"]") "x"))
     "string arrays are homogeneous independent of runtime length type")
    (assert-true
     (string= "gmail"
              (toml-get
               (parse-toml "[source.private-mail]
service = \"gmail\"")
               "source.private-mail.service"))
     "nested dotted tables traverse their contained alists")
    (assert-true
     (handler-case (progn (parse-toml "[grid\nx=2") nil)
       (toml-error (condition) (= 1 (toml-error-line condition))))
     "located malformed table")
    t))
