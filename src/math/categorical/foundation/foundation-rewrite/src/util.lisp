;;;; util.lisp --- small numerical and structural helpers.
;;;;
;;;; This file is intentionally tiny: rosette-foundation-rewrite has no
;;;; rosette-* dependencies, so anything genuinely numerical that downstream
;;;; libraries need should live in those libraries, not here.

(in-package #:rosette-foundation-rewrite)

(declaim (inline approx=))

(defun approx= (a b &optional (tol 1d-9))
  "Return T iff |A - B| ≤ TOL.  Both operands are coerced to double-float."
  (<= (abs (- (coerce a 'double-float) (coerce b 'double-float)))
      (coerce tol 'double-float)))

(defun rationalize-prob (bps)
  "Return the (numerator . denominator) pair for the probability whose
basispunten count is BPS.  Reduced to lowest terms.  E.g. 5000 → (1 . 2),
1234 → (617 . 5000)."
  (let* ((g (gcd bps 10000))
         (n (/ bps g))
         (d (/ 10000 g)))
    (cons n d)))

;;; A tiny multiset / hash-set helper used by domain-colimit on the σ
;;; component, where the Lean source spells out "unie op σ" — set union,
;;; not list concatenation.

(defun ordered-list-union (xs ys test)
  "Set-union of XS and YS under TEST, preserving first occurrence order."
  (let ((acc (copy-list xs))
        (tail nil))
    (when acc
      (setf tail (last acc)))
    (dolist (y ys)
      (unless (member y acc :test test)
        (let ((cell (list y)))
          (if tail
              (setf (cdr tail) cell
                    tail cell)
              (setf acc cell
                    tail cell)))))
    acc))

(defun set-union-eql (xs ys)
  "Set-union of XS and YS under EQL.  Order: XS first, then the elements
of YS not already present."
  (ordered-list-union xs ys #'eql))

(defun list-disjoint-union (xs ys)
  "Disjunctive union of XS and YS — i.e. plain list concatenation.
Named explicitly so the call site at DOMAIN-COLIMIT reads as the Lean
source intends."
  (append xs ys))

(defun list-conjunction-union (xs ys)
  "List union with EQUAL-equality, used for the λ-component of the
domain colimit (Lean: \"conjunctie op λ\")."
  (ordered-list-union xs ys #'equal))

(defun safe-coerce-double (x)
  "Coerce X to double-float, accepting integers, ratios, single-floats."
  (coerce x 'double-float))
