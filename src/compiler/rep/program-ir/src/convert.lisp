;;;; convert.lisp --- LISP->PROGRAM and PROGRAM->LISP.
;;;;
;;;; The canonical lifted form is:
;;;;
;;;;   (LISP->PROGRAM '(+ 1 (* 2 3)))
;;;;   => #S(PROGRAM :AST (+ #<PROGRAM 1> #<PROGRAM (* #<PROGRAM 2>
;;;;                                                  #<PROGRAM 3>)>)
;;;;                 ...)
;;;;
;;;; i.e. the operator is left as a raw symbol in the head, and every
;;;; argument (including atoms) is wrapped in its own PROGRAM.  This
;;;; gives every node a stable identity for thermodynamic annotation.

(in-package #:rosette-program-ir)

(defun lisp->program (sexp &key parent)
  "Lift the s-expression SEXP into a PROGRAM tree.

Every sub-expression — including atoms — is wrapped in its own PROGRAM
so it can carry thermodynamic metadata.  PARENT is threaded through so
that PROGRAM-PARENT is correctly populated for non-root nodes.

The operator symbol of a compound form is *not* wrapped; it remains the
car of the AST list.  This preserves the homoiconic invariant that
PROGRAM->LISP is a left-inverse of LISP->PROGRAM.

Implementation note: PROGRAM-PARENT pointers are filled in after the
node is constructed, since the parent must exist before its children
can refer to it.  We walk the constructed AST once and patch the
PARENT slot of every immediate child."
  (cond
    ((atom sexp)
     (%make-program :ast sexp :metadata nil :parent parent))
    (t
     (let* ((children (mapcar (lambda (c) (lisp->program c :parent nil))
                              (cdr sexp)))
            (node     (%make-program :ast (cons (car sexp) children)
                                     :metadata nil
                                     :parent parent)))
       (dolist (child children)
         (setf (program-parent child) node))
       node))))

(defun program->lisp (prog)
  "Project a PROGRAM tree back to its underlying s-expression.

This is the left-inverse of LISP->PROGRAM:
  (program->lisp (lisp->program X)) == X (under EQUAL).

Metadata, parent pointers, and node identity are discarded."
  (let ((ast (program-ast prog)))
    (cond
      ((atom ast) ast)
      (t (cons (car ast)
               (mapcar (lambda (child)
                         (if (programp child)
                             (program->lisp child)
                             child))
                       (cdr ast)))))))
