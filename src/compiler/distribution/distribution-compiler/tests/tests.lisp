;;;; distribution-compiler/tests/tests.lisp --- executable compiler laws.

(defpackage #:distribution-compiler/tests
  (:use #:cl #:rosette-distribution-compiler)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:distribution-compiler/tests)

(defparameter *assertions* 0)
(defparameter *graph*
  '(("rosette" "cell" "composition")
    ("composition" "component") ("component" "cell") ("cell")
    ("rose" "rosette")))

(defun check (value control &rest arguments)
  (incf *assertions*)
  (assert-true value (apply #'format nil control arguments)))

(defun deps (name) (copy-list (cdr (assoc name *graph* :test #'string=))))
(defun resolvable (name) (and (assoc name *graph* :test #'string=) t))

(defun definition (&key (roots '("rosette"))
                        (expected '("cell" "component" "composition" "rosette"))
                        (forbidden '("rose")) (max 4)
                        (license-grants-id "sha256:grants-a"))
  (make-distribution-definition
   :name "Rosette" :version "1" :roots roots
   :expected-systems expected :forbidden-systems forbidden
   :forbidden-prefixes '("private-") :max-systems max
   :public-license "Apache-2.0" :source-prefix "rosette-"
   :public-prefix "rosette-" :source-environment-prefix "ROSETTE_"
   :public-environment-prefix "ROSETTE_" :forbidden-tokens '("/private-root/")
   :cut-policy-id "sha256:cut" :export-policy-id "sha256:export"
   :license-grants-id license-grants-id))

(defun error-code (thunk)
  (handler-case (progn (funcall thunk) nil)
    (distribution-error (condition) (distribution-error-code condition))))

(defun compile-fixture (definition)
  (compile-distribution-plan definition :deps-fn #'deps
                            :resolvable-fn #'resolvable))

(defun test-plan ()
  (let* ((plan (compile-fixture (definition)))
         (again (compile-fixture (definition))))
    (check (equal '("cell" "component" "composition" "rosette")
                  (distribution-plan-systems plan))
           "closure plan is not the exact public set")
    (check (string= (distribution-plan-id plan) (distribution-plan-id again))
           "identical definitions did not compile to one identity")
    (check (equal '("cell" "component" "composition" "rosette")
                  (cdr (assoc "rosette"
                              (distribution-plan-root-closures plan)
                              :test #'string=)))
           "root closure lost dependency order")))

(defun test-policy-is-identity-bearing ()
  (let ((base (compile-fixture (definition)))
        (changed
          (compile-fixture
           (make-distribution-definition
            :name "Rosette" :version "1" :roots '("rosette")
            :expected-systems '("cell" "component" "composition" "rosette")
            :forbidden-systems '("rose") :forbidden-prefixes '("private-")
            :max-systems 5 :public-license "Apache-2.0"
            :source-prefix "rosette-" :public-prefix "rosette-"
            :source-environment-prefix "ROSETTE_"
            :public-environment-prefix "ROSETTE_"
            :forbidden-tokens '("/private-root/")))))
    (check (not (string= (distribution-plan-id base)
                         (distribution-plan-id changed)))
           "a changed distribution boundary retained the old plan identity")
    (let ((grant-change
            (compile-fixture
             (definition :license-grants-id "sha256:grants-b"))))
      (check (not (string= (distribution-plan-id base)
                           (distribution-plan-id grant-change)))
             "changed license grants retained the old plan identity"))))

(defun test-negative-controls ()
  (check (eq :closure-drift
             (error-code
              (lambda ()
                (compile-fixture (definition :expected '("rosette"))))))
         "expected-set drift was accepted")
  (check (eq :system-ceiling
             (error-code
              (lambda () (compile-fixture (definition :max 3)))))
         "over-budget closure was accepted")
  (check (eq :forbidden-system
             (error-code
              (lambda ()
                (compile-fixture
                 (definition :roots '("rose")
                             :expected
                             '("cell" "component" "composition" "rose"
                               "rosette")
                             :max 5)))))
         "Rose crossed the Rosette membrane"))

(defun test-lowering ()
  (let ((definition (definition)))
    (check (string= "rosette-cell"
                    (lower-distribution-name definition "rosette-cell"))
           "system namespace did not lower")
    (check (string= "ROSETTE_CELL rosette-cell"
                    (lower-distribution-text definition "ROSETTE_CELL rosette-cell"))
           "source and environment namespaces did not lower")))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-plan)
  (test-policy-is-identity-bearing)
  (test-negative-controls)
  (test-lowering)
  (format t "distribution-compiler: ~D assertions passed~%" *assertions*)
  t)
