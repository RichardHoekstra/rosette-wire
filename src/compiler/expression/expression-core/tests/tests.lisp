;;;; tests.lisp --- Unit tests for rosette-expression-core.

(defpackage #:rosette-expression-core/tests
  (:use #:cl #:rosette-expression-core)
  (:export #:run-all-tests))

(in-package #:rosette-expression-core/tests)

(defvar *passes* 0)
(defvar *fails* 0)

(defmacro check (name expression)
  `(if ,expression
       (progn (incf *passes*) (format t "~&  PASS  ~A~%" ',name))
       (progn (incf *fails*)
              (format t "~&  FAIL  ~A~%    expression: ~S~%" ',name ',expression))))

(defun expr-fixture ()
  (cf-add (cf-mul (cf-var 'x) (cf-var 'x))
          (cf-log (cf-add (cf-var 'y) (cf-const 2d0)))))

(defun property-builders ()
  (let ((expr (expr-fixture)))
    (and (typep expr 'cf-node)
         (eq (cf-node-op expr) :add)
         (= (cf-node-count expr) 8)
         (= (cf-depth expr) 4))))

(defun property-variables ()
  (equal (cf-variables (expr-fixture)) '(x y)))

(defun property-substitute-compose ()
  (let* ((expr (expr-fixture))
         (sub (cf-substitute expr `((x . ,(cf-const 3d0)))))
         (composed (cf-compose expr 'y (cf-var 'z))))
    (and (equal (cf-variables sub) '(y))
         (equal (cf-variables composed) '(x z)))))

(defun property-specials ()
  (and (cf-uses-special-p (cf-normal-cdf (cf-var 'x)))
       (not (cf-uses-special-p (expr-fixture)))))

(defun property-cse ()
  (let* ((x (cf-var 'x))
         (expr (cf-add (cf-mul x x) (cf-mul x x)))
         (table (cf-cse-keys expr)))
    (and (hash-table-p table)
         (= (hash-table-count table) 3)
         (< (hash-table-count table) (cf-node-count expr)))))

(defun run-all-tests ()
  (setf *passes* 0 *fails* 0)
  (format t "~&;; rosette-expression-core test run ----------------------------~%")
  (let ((c (cf-const 2))
        (x (cf-var 'x)))
    (check const-builder
           (and (typep c 'cf-node)
                (eq (cf-node-op c) :const)
                (equal (cf-node-args c) '(2d0))))
    (check var-builder
           (and (typep x 'cf-node)
                (eq (cf-node-op x) :var)
                (equal (cf-node-args x) '(x)))))
  (let ((x (cf-var 'x))
        (y (cf-var 'y)))
    (check arithmetic-builder-ops
           (and (eq (cf-node-op (cf-add x y)) :add)
                (eq (cf-node-op (cf-sub x y)) :sub)
                (eq (cf-node-op (cf-mul x y)) :mul)
                (eq (cf-node-op (cf-div x y)) :div)))
    (check arithmetic-builder-args
           (equal (cf-node-args (cf-add x y)) (list x y)))
    (check unary-builder-ops
           (and (eq (cf-node-op (cf-exp x)) :exp)
                (eq (cf-node-op (cf-log x)) :log)
                (eq (cf-node-op (cf-sqrt x)) :sqrt)))
    (check unary-builder-args
           (equal (cf-node-args (cf-log x)) (list x)))
    (let ((pow (cf-power x (cf-const 3))))
      (check power-expands-to-exp-mul-log
             (and (eq (cf-node-op pow) :exp)
                  (eq (cf-node-op (first (cf-node-args pow))) :mul)
                  (eq (cf-node-op (second (cf-node-args
                                           (first (cf-node-args pow)))))
                      :log)))))
  (let ((x (cf-var 'x))
        (p (cf-var 'p))
        (mu (cf-var 'mu))
        (lam (cf-var 'lambda)))
    (check special-builder-ops
           (and (eq (cf-node-op (cf-normal-cdf x)) :normal-cdf)
                (eq (cf-node-op (cf-inverse-normal-cdf p)) :inverse-normal-cdf)
                (eq (cf-node-op (cf-inverse-gaussian-quantile p mu lam))
                    :inverse-gaussian-quantile)))
    (check inverse-gaussian-arg-order
           (equal (cf-node-args (cf-inverse-gaussian-quantile p mu lam))
                  (list p mu lam)))
    (let ((piece (cf-piecewise p x mu)))
      (check piecewise-builder
             (and (eq (cf-node-op piece) :piecewise)
                  (equal (cf-node-args piece) (list p x mu))))))
  (check builders (property-builders))
  (check variables (property-variables))
  (let ((vars (cf-variables
               (cf-add (cf-var 'x)
                       (cf-add (cf-var 'y) (cf-var 'x))))))
    (check variables-are-deduplicated
           (and (= (length vars) 2)
                (member 'x vars)
                (member 'y vars))))
  (check closed-constant
         (cf-closed-p (cf-add (cf-const 1) (cf-const 2))))
  (check open-variable
         (not (cf-closed-p (cf-add (cf-const 1) (cf-var 'x)))))
  (check node-count-leaves
         (and (= (cf-node-count (cf-const 1)) 1)
              (= (cf-node-count (cf-var 'x)) 1)))
  (check depth-leaves
         (and (= (cf-depth (cf-const 1)) 1)
              (= (cf-depth (cf-var 'x)) 1)))
  (check depth-compound
         (= (cf-depth (cf-exp (cf-add (cf-var 'x) (cf-const 1)))) 3))
  (check substitute-compose (property-substitute-compose))
  (let* ((x (cf-var 'x))
         (y (cf-var 'y))
         (expr (cf-add x y))
         (sub (cf-substitute expr `((x . ,(cf-const 4))
                                    (z . ,(cf-const 9))))))
    (check substitute-preserves-unmentioned-vars
           (equal (cf-variables sub) '(y)))
    (check substitute-replaces-mentioned-var
           (and (eq (cf-node-op (first (cf-node-args sub))) :const)
                (= (first (cf-node-args (first (cf-node-args sub)))) 4d0)))
    (check compose-equivalent-to-single-substitution
           (equalp (cf-compose expr 'y (cf-const 5))
                   (cf-substitute expr `((y . ,(cf-const 5)))))))
  (check specials (property-specials))
  (check inverse-normal-is-special
         (cf-uses-special-p (cf-inverse-normal-cdf (cf-var 'p))))
  (check inverse-gaussian-is-special
         (cf-uses-special-p
          (cf-inverse-gaussian-quantile (cf-var 'p) (cf-var 'mu) (cf-var 'lambda))))
  (check piecewise-propagates-special
         (cf-uses-special-p
          (cf-piecewise (cf-var 'c)
                        (cf-normal-cdf (cf-var 'x))
                        (cf-const 0))))
  (check cse (property-cse))
  (let* ((x (cf-var 'x))
         (expr (cf-add (cf-mul x x) (cf-mul x x)))
         (table (cf-cse-keys expr))
         (var-id (gethash '(:var x) table))
         (mul-key (cons :mul (list var-id var-id)))
         (mul-id (gethash mul-key table))
         (root-key (cons :add (list mul-id mul-id))))
    (check cse-reuses-var-key
           (and var-id (= var-id (gethash '(:var x) table))))
    (check cse-has-root-key
           (and mul-id (plusp (gethash root-key table)))))
  (format t "~&;; ~D pass, ~D fail~%" *passes* *fails*)
  (unless (zerop *fails*)
    (error "rosette-expression-core tests failed: ~D failures." *fails*))
  (values *passes* *fails*))
