;;;; tests.lisp --- tests for rosette-egraph.

(defpackage #:rosette-egraph/tests
  (:use #:cl #:rosette-egraph #:rosette-form-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-egraph/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-egraph")
    (let ((graph (make-egraph)))
      (let ((a (egraph-add graph '(+ x 0)))
            (b (egraph-add graph 'x)))
        (check run (not (egraph-equivalent-p graph a b)) "distinct forms start apart")
        (egraph-merge graph a b)
        (check run (egraph-equivalent-p graph '(+ x 0) 'x) "merge records equivalence")
        (check run (= (length (egraph-class-members graph a)) 2) "class contains both forms")))
    (let ((graph (make-egraph)))
      (let ((a (unify-forms graph '(* x 1) (copy-tree '(* x 1)))))
        (check run (form-p a) "unify returns a Form")
        (check run (= (length (egraph-class-members graph '(* x 1))) 1)
               "structural duplicates collapse through form interning")))))
