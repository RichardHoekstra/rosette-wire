;;;; rewrite-general.lisp --- OPTIMIZE (stage 3c): general rewriting, the seed.
;;;;
;;;; certified-simplify (rewrite.lisp) rides rosette-sound-self-rewrite:improve,
;;;; whose equality-saturation loop is scoped to ARITHMETIC op-trees over
;;;; {+ - * sq}. This file seeds rewriting BEYOND arithmetic by driving the
;;;; equality-graph kernel (rosette-egraph, transitively present via
;;;; rosette-sound-self-rewrite -> rosette-egraph-saturate -> rosette-egraph) directly: two
;;;; forms are equivalent iff, after asserting a set of equalities, they land in
;;;; the same e-class.
;;;;
;;;; HONEST WALL. Rules here apply only as LITERAL equivalences: a rule
;;;; (LHS RHS) asserts those two specific ground forms equal -- there is no
;;;; pattern-variable substitution and no saturation loop, so `(+ ?x 0) === ?x'
;;;; as a schema does NOT fire on `(+ y 0)'. A real congruence-closure /
;;;; unification-driven saturation (rosette-egraph:unify-forms + rewrite schemas) is
;;;; the frontier this seeds. rosette-sound-self-rewrite already does the arithmetic
;;;; sub-case with certificates; this exposes the raw e-class machinery so
;;;; non-arithmetic equivalences can be proven and, later, saturated.

(in-package #:rosette-ship)

(defun forms-equivalent-p (a b &key (rules '()))
  "True iff forms A and B are provably equal under RULES. RULES is a list of
literal equalities (LHS RHS); each asserts that those two ground forms are
equal. Build an e-graph, add A and B, then for every rule add both sides and
merge their e-classes; A and B are equivalent iff they end in the same class.
With no RULES this is structural equality (A and B intern to one form).

WALL: rules are LITERAL, not schematic -- no pattern variables, no saturation.
See the file header."
  (let ((g (rosette-egraph:make-egraph)))
    (rosette-egraph:egraph-add g a)
    (rosette-egraph:egraph-add g b)
    (dolist (rule rules)
      (destructuring-bind (lhs rhs) rule
        (rosette-egraph:egraph-add g lhs)
        (rosette-egraph:egraph-add g rhs)
        (rosette-egraph:egraph-merge g lhs rhs)))
    (rosette-egraph:egraph-equivalent-p g a b)))

(defun prove-simplification (a b &key rules)
  "A plist adjudicating a proposed rewrite A -> B under RULES: :EQUIVALENT is
the FORMS-EQUIVALENT-P verdict (is B a sound replacement for A?), :SMALLER is
whether B's printed representation is strictly shorter than A's (did the rewrite
actually shrink anything?). A trustworthy simplification is both."
  (list :from a
        :to b
        :equivalent (forms-equivalent-p a b :rules rules)
        :smaller (< (length (format nil "~S" b))
                    (length (format nil "~S" a)))))
