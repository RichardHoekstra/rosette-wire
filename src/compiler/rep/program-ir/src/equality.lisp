;;;; equality.lisp --- Structural equality and canonical form.
;;;;
;;;; PROGRAM-EQUAL? compares the underlying s-expression structure and
;;;; ignores metadata, parent pointers, and any object identity.  This
;;;; is the right notion of equality for a homoiconic store: two
;;;; programs are equal iff they would PRINC to the same source code.
;;;;
;;;; CANONICAL-FORM is currently identity composed with PROGRAM->LISP;
;;;; once rosette-mdl-parsimony lands it will fold in alpha-renaming and
;;;; commutative-operator sorting.  Until then the function exists so
;;;; callers can write to the eventual API surface without their code
;;;; breaking when the canonicaliser becomes non-trivial.

(in-package #:rosette-program-ir)

(defun program-equal? (a b)
  "Return T iff PROGRAMs A and B have structurally equal ASTs.

Compares atoms with CL:EQUAL.  Compound forms are equal iff they have
the same operator symbol (compared with EQ) and pairwise PROGRAM-EQUAL?
children of the same arity.  Metadata, parent pointers, and node
identity are ignored.

Either argument may also be a raw atom; this is treated as a leaf."
  (cond
    ((and (programp a) (programp b))
     (program-equal? (program-ast a) (program-ast b)))
    ((and (atom a) (atom b))
     (equal a b))
    ((and (consp a) (consp b))
     (and (eq (car a) (car b))
          (let ((ax (cdr a)) (bx (cdr b)))
            (and (= (length ax) (length bx))
                 (every #'program-equal? ax bx)))))
    (t nil)))

(defun canonical-form (prog)
  "Return a canonical-form PROGRAM that is PROGRAM-EQUAL? to PROG.

In 0.1 this strips metadata and parent pointers but preserves AST
shape.  The canonicaliser will grow alpha-renaming, commutative-op
sorting, and rewrite-rule normalisation in a future point release; the
contract that  (program-equal? P (canonical-form P))  is and will
remain T."
  (lisp->program (program->lisp prog)))
