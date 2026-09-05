;;;; rosette-program-ir/tests/tests.lisp --- Test suite.

(defpackage #:rosette-program-ir/tests
  (:use #:cl #:rosette-program-ir)
  (:export #:run-all-tests))

(in-package #:rosette-program-ir/tests)

(defvar *passes* 0)
(defvar *fails* 0)

(defmacro check (name expression)
  `(if ,expression
       (progn (incf *passes*) (format t "~&  PASS  ~A~%" ',name))
       (progn (incf *fails*)
              (format t "~&  FAIL  ~A~%    expression: ~S~%" ',name ',expression))))

(defun run-all-tests ()
  "Run program-IR tests."
  (setf *passes* 0 *fails* 0)
  (format t "~&;; rosette-program-ir test run --------------------------------~%")
  (let* ((sexp '(+ 1 (* 2 3)))
         (prog (lisp->program sexp)))
    (check roundtrip (equal sexp (program->lisp prog)))
    (check count-nodes (= 5 (count-nodes prog)))
    (check tree-depth (= 3 (tree-depth prog)))
    (check arity (= 2 (program-arity prog)))
    (check programp-root (programp prog))
    (check root-parent-is-nil (null (program-parent prog)))
    (check root-children-are-programs
           (every #'programp (program-children prog)))
    (check child-parent-pointers-threaded
           (every (lambda (child) (eq (program-parent child) prog))
                  (program-children prog)))
    (check leaf-has-no-children
           (null (program-children (first (program-children prog)))))
    (check leaf-arity-zero
           (zerop (program-arity (first (program-children prog)))))
    (check walk-ast-returns-input
           (eq prog (walk-ast prog (lambda (node) (declare (ignore node)) nil))))
    (let ((visited '()))
      (walk-ast prog
                (lambda (node)
                  (push (if (programp node) (program->lisp node) node)
                        visited)))
      (check walk-ast-preorder
             (equal (nreverse visited) '((+ 1 (* 2 3)) 1 (* 2 3) 2 3))))
    (let ((visited '()))
      (for-each-node prog
                     (lambda (node)
                       (push (if (programp node) (program->lisp node) node)
                             visited)))
      (check for-each-node-aliases-walk
             (equal (nreverse visited) '((+ 1 (* 2 3)) 1 (* 2 3) 2 3))))
    (check fold-ast-preorder-accumulates
           (equal (fold-ast prog '()
                            (lambda (acc node)
                              (cons (if (programp node) (program->lisp node) node)
                                    acc)))
                  '(3 2 (* 2 3) 1 (+ 1 (* 2 3)))))
    (check structural-equality
           (program-equal? prog (lisp->program '(+ 1 (* 2 3)))))
    (check structural-inequality-operator
           (not (program-equal? prog (lisp->program '(- 1 (* 2 3))))))
    (check structural-inequality-arity
           (not (program-equal? prog (lisp->program '(+ 1 (* 2 3) 4)))))
    (check equality-ignores-metadata
           (program-equal? (make-program 'x :metadata '(:source :a))
                           (make-program 'x :metadata '(:source :b))))
    (check canonical-preserves-equality
           (program-equal? prog (canonical-form prog)))
    (check canonical-strips-metadata
           (null (program-metadata
                  (canonical-form (make-program 'x :metadata '(:source :a))))))
    (check canonical-strips-root-parent
           (null (program-parent
                  (canonical-form (make-program 'x :parent prog)))))
    (check map-ast-recurse-preserves-shape
           (program-equal? prog (map-ast prog (lambda (node)
                                                (declare (ignore node))
                                                :recurse))))
    (check map-ast-can-replace-root
           (equal 'z (program->lisp
                      (map-ast prog
                               (lambda (node)
                                 (if (and (programp node)
                                          (equal (program->lisp node) sexp))
                                     (make-program 'z)
                                     :recurse))))))
    (check replace-node
           (equal '(+ 9 (* 2 3))
                  (program->lisp
                   (replace-node prog
                                 (lambda (node)
                                   (and (programp node)
                                        (equal (program-ast node) 1)))
                                 (make-program 9)))))
    (check replace-node-function
           (equal '(+ 2 (* 4 6))
                  (program->lisp
                   (replace-node prog
                                 (lambda (node)
                                   (and (programp node)
                                        (numberp (program-ast node))))
                                 (lambda (node)
                                   (make-program (* 2 (program-ast node)))))))))
  (let* ((metadata (list :source :caller))
         (prog (make-program 'x :metadata metadata)))
    (setf (getf metadata :source) :mutated)
    (check metadata-isolated
           (equal (program-metadata prog) '(:source :caller))))
  (let ((parent (make-program 'parent)))
    (check make-program-preserves-parent
           (eq parent (program-parent (make-program 'child :parent parent)))))
  (let ((rejected nil))
    (handler-case
        (make-program 'bad :metadata '(:source))
      (error ()
        (setf rejected t)))
    (check malformed-metadata-rejected rejected))
  (let ((rejected nil))
    (handler-case
        (make-program 'bad :metadata '("source" :caller))
      (error ()
        (setf rejected t)))
    (check non-symbol-metadata-key-rejected rejected))
  (format t "~&;; ----------------------------------------------------------~%")
  (format t "~&;; ~D pass, ~D fail~%" *passes* *fails*)
  (unless (zerop *fails*)
    (error "rosette-program-ir tests failed: ~D failures." *fails*))
  (values *passes* *fails*))
