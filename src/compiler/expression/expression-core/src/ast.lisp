;;;; ast.lisp --- AST nodes for the closed-form expression class C
;;;;
;;;; A cf-node is the union-of-cases AST representing an element of
;;;; Definition 8's closed-form class C: constants, variables, the four
;;;; field operations, exp/log/sqrt, the standard normal CDF and its
;;;; inverse, the inverse-Gaussian quantile, and finite piecewise
;;;; selection.  All builders are zero-cons after warm-up — each node
;;;; is a 16-byte struct holding op-keyword + args list.

(in-package #:rosette-expression-core)

(defstruct (cf-node (:constructor make-cf-node (op args)))
  "Closed-form cf-node: an operator keyword OP and a list ARGS
of either children (cf-node) or literal data (number for :const, symbol
for :var)."
  op
  args)

;;; --- leaves --------------------------------------------------------------

(defun cf-const (value)
  "AST node for the literal constant VALUE (coerced to double-float)."
  (make-cf-node :const (list (coerce value 'double-float))))

(defun cf-var (name)
  "AST node for the free variable NAME (a symbol)."
  (make-cf-node :var (list name)))

;;; --- field operations ----------------------------------------------------

(defun cf-add (a b) (make-cf-node :add (list a b)))
(defun cf-sub (a b) (make-cf-node :sub (list a b)))
(defun cf-mul (a b) (make-cf-node :mul (list a b)))
(defun cf-div (a b) (make-cf-node :div (list a b)))

;;; --- elementary unary ---------------------------------------------------

(defun cf-exp  (a) (make-cf-node :exp  (list a)))
(defun cf-log  (a) (make-cf-node :log  (list a)))
(defun cf-sqrt (a) (make-cf-node :sqrt (list a)))

(defun cf-power (base exponent)
  "Integer or real power of an AST: x^k = exp(k * log x).
For positive bases this stays in C.  For non-positive bases or
non-integer exponents the caller must guarantee domain; see THEORY.md
Definition 8 condition 1."
  (cf-exp (cf-mul exponent (cf-log base))))

;;; --- special-function unary ---------------------------------------------

(defun cf-normal-cdf (a)
  "AST node for Phi(a), the standard-normal CDF."
  (make-cf-node :normal-cdf (list a)))

(defun cf-inverse-normal-cdf (a)
  "AST node for Phi^{-1}(a), the standard-normal quantile."
  (make-cf-node :inverse-normal-cdf (list a)))

;;; --- ternary special-function -------------------------------------------

(defun cf-inverse-gaussian-quantile (p mu lambda)
  "AST node for F_IG^{-1}(p; mu, lambda), the inverse-Gaussian quantile."
  (make-cf-node :inverse-gaussian-quantile (list p mu lambda)))

;;; --- piecewise selection ------------------------------------------------

(defun cf-piecewise (condition then-expr else-expr)
  "AST node for a finite piecewise selection: if (CONDITION > 0) then
THEN-EXPR else ELSE-EXPR.  Closure of C under finite semialgebraic
selection (Definition 8 condition 3)."
  (make-cf-node :piecewise (list condition then-expr else-expr)))
