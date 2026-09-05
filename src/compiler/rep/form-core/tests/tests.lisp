;;;; tests.lisp --- tests for rosette-form-core.

(defpackage #:rosette-form-core/tests
  (:use #:cl #:rosette-form-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-form-core/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-form-core")
    (check run
           (equal
            (mapcar #'form-address-of
                    (list nil :x "abc" (list :a 1 (vector 2 3))))
            (list #xAF63BD4C8601B7DF
                  #x329E5A8BFF95BC64
                  #x3D24CFE8EEF19352
                  #x9CB4D8DD6794E4AA))
           "optimized hashing preserves frozen structural address vectors")
    (let* ((value '(+ (* x x) x))
           (a (intern-form value))
           (b (intern-form (copy-tree value))))
      (check run (form-p a) "intern-form returns a Form")
      (check run (eq a b) "equal structure interns to one Form")
      (check run (eql (form-address a) (form-address-of value)) "address matches structural hash")
      (check run (eql (form-address-of value)
                      (form-address-of (copy-tree value)))
             "address is deterministic for copied structure")
      (check run (not (eql (form-address-of '(+ x 1))
                           (form-address-of '(+ x 2))))
             "different structure receives a different address")
      (check run (not (eq a (make-form value)))
             "make-form creates a non-interned Form handle")
      (check run (eql (form-address (make-form value)) (form-address a))
             "make-form computes the same structural address")
      (check run (equal (form-value a) value) "value is preserved")
      (check run (eq (intern-form (form-value a)) a)
             "interning a Form value returns the canonical handle"))
    (check run (eql (form-address-of "abc") (form-address-of (copy-seq "abc")))
           "string addresses are deterministic across fresh strings")
    (check run (not (eql (form-address-of 'cl-user::x)
                         (form-address-of 'keyword::x)))
           "symbol addresses include the package")
    (check run (eql (form-address-of #(1 2 3))
                    (form-address-of (make-array 3 :initial-contents '(1 2 3))))
           "array addresses are structural over contents")
    (check run (not (eql (form-address-of #(1 2 3))
                         (form-address-of #(1 2 4))))
           "array addresses distinguish changed elements")
    (let* ((value '(:rows (("a" 1.25d0) ("b" #(2 3))) :done t))
           (domains '((:rosette-content-id/v1 0)
                      (:rosette-content-id/v1 1)
                      (:rosette-content-id/v1 2)
                      (:rosette-content-id/v1 3)))
           (expected
             (mapcar (lambda (domain)
                       (form-address-of (append domain (list value))))
                     domains)))
      (check run
             (equal expected
                    (form-addresses-of-domain-separated value domains))
             "multi-lane hashing is bit-exact with independent framed hashes"))
    (let ((seen nil))
      (check run (equal (walk-form (lambda (node) (push node seen)) '(+ x 1))
                        '(+ x 1))
             "walk-form returns raw input values")
      (check run (member '+ seen) "walk sees operator")
      (check run (member 'x seen) "walk sees variable")
      (check run (member 1 seen) "walk sees constant")
      (check run (member '(x 1) seen :test #'equal)
             "walk sees cons tails in preorder"))
    (let* ((form (intern-form '(* y 2)))
           (seen nil))
      (check run (eq (walk-form (lambda (node) (push node seen)) form) form)
             "walk-form returns Form handles unchanged")
      (check run (equal (first (last seen)) '(* y 2))
             "walk-form starts at the Form value"))
    (check run (string= (with-output-to-string (out)
                          (pprint-form (intern-form '(+ x 1)) out))
                        (with-output-to-string (out)
                          (pprint '(+ x 1) out)))
           "pprint-form prints the structural value of a Form")
    (check run (string= (with-output-to-string (out)
                          (pprint-form '(+ x 1) out))
                        (with-output-to-string (out)
                          (pprint '(+ x 1) out)))
           "pprint-form accepts raw structural values")

    (let* ((arena (make-form-arena))
           (source (list :plan (list (copy-seq "alpha")
                                     (vector 1 2))))
           (frozen (intern-frozen-form arena source))
           (expected '(:plan ("alpha" #(1 2)))))
      (setf (char (first (second source)) 0) #\Z
            (aref (second (second source)) 0) 9)
      (check run (equalp expected (thaw-frozen-form frozen))
             "freezing isolates list, string, and vector caller mutation")
      (let ((projection (thaw-frozen-form frozen)))
        (setf (char (first (second projection)) 1) #\Z
              (aref (second (second projection)) 1) 9)
        (check run (equalp expected (thaw-frozen-form frozen))
               "thawing never exposes arena-owned mutable storage"))
      (check run
             (eq frozen (intern-frozen-form arena
                                            '(:plan ("alpha" #(1 2)))))
             "equal structure has pointer identity within one arena")
      (let* ((other-arena (make-form-arena))
             (other (intern-frozen-form other-arena frozen)))
        (check run (not (eq frozen other))
               "pointer identity does not cross explicit arena lifetimes")
        (check run (equalp (thaw-frozen-form frozen)
                           (thaw-frozen-form other))
               "cross-arena import preserves structural value"))
      (let ((before (form-arena-node-count arena)))
        (dotimes (i 20)
          (declare (ignore i))
          (intern-frozen-form arena '(:plan ("alpha" #(1 2)))))
        (check run (= before (form-arena-node-count arena))
               "re-interning equal values does not grow the arena"))
      (check run
             (equalp
              expected
              (fold-frozen-form
               (lambda (kind payload children)
                 (case kind
                   (:null nil)
                   (:cons (cons (first children) (second children)))
                   (:array
                    (let* ((array (make-array
                                   (getf payload :dimensions)
                                   :element-type (getf payload :element-type))))
                      (loop for child in children for index from 0
                            do (setf (row-major-aref array index) child))
                      array))
                   (otherwise payload)))
               frozen))
             "fold-frozen-form lowers the DAG without exposing its storage"))

    (let* ((arena (make-form-arena))
           (source (make-array '(2 2) :element-type '(unsigned-byte 8)
                                      :initial-contents '((1 2) (3 4))))
           (frozen (intern-frozen-form arena source))
           (copy (thaw-frozen-form frozen)))
      (check run (and (equal '(2 2) (array-dimensions copy))
                      (equal (array-element-type source)
                             (array-element-type copy))
                      (equalp source copy))
             "frozen arrays preserve dimensions, upgraded element type, and data"))

    (let ((arena (make-form-arena)))
      (check run
             (not (eq (intern-frozen-form arena 0.0d0)
                      (intern-frozen-form arena -0.0d0)))
             "exact float keys retain the sign of zero"))

    (let ((cycle (list :cycle)))
      (setf (cdr cycle) cycle)
      (check run
             (handler-case
                 (progn (intern-frozen-form (make-form-arena) cycle) nil)
               (error () t))
             "cyclic input fails closed instead of entering the arena"))))
