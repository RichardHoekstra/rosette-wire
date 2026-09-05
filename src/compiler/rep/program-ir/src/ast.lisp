;;;; ast.lisp --- The PROGRAM struct: typed homoiconic program data.
;;;;
;;;; A PROGRAM is a thin envelope around a Lisp s-expression (the AST)
;;;; with two extra slots:
;;;;
;;;;   - METADATA: a property list holding thermodynamic annotations
;;;;     (energy, entropy, temperature, ...).  Numerical content is
;;;;     produced by sister libraries; this layer only owns the slot.
;;;;   - PARENT: optional back-pointer used by fold/walk visitors that
;;;;     want to ascend the tree without re-walking from the root.
;;;;
;;;; The AST itself can be:
;;;;
;;;;   - an atom (symbol, number, string, character, keyword) — a leaf
;;;;   - a proper list whose first element is the operator and whose
;;;;     remaining elements are child PROGRAMs (or atoms — see
;;;;     LISP->PROGRAM for the lifted form).
;;;;
;;;; The struct itself is intentionally agnostic about which form
;;;; appears; CONVERT.LISP defines the canonical lifted form used by
;;;; LISP->PROGRAM and PROGRAM->LISP.

(in-package #:rosette-program-ir)

(defstruct (program
            (:constructor %make-program)
            (:copier nil)
            (:predicate programp))
  "A homoiconic program node.

AST is either an atom or a list whose car is the operator symbol and
whose cdr are child PROGRAM nodes.

METADATA is a property list.  Reserved keys (claimed by the
thermodynamic kapsel):

  :ENERGY       — real, scalar associated with this node
  :ENTROPY      — real, log-multiplicity over equivalent rewrites
  :TEMPERATURE  — real, scaling parameter for the local Boltzmann factor

Other keys are free for downstream-library use; see the kapsel
sister libraries for concrete meanings.

PARENT, if non-NIL, is the immediately enclosing PROGRAM."
  (ast       nil :read-only t)
  (metadata  nil :type list)
  (parent    nil))

(declaim (inline programp))

(defun make-program (ast &key metadata parent)
  "Construct a PROGRAM with the given AST.

AST is *not* copied; the caller is responsible for ensuring it is not
shared with another mutable owner.  METADATA defaults to NIL (an empty
plist).  PARENT defaults to NIL."
  (%make-program :ast ast
                 :metadata (rosette-metadata-core:copy-plist
                            metadata
                            :label "Program METADATA")
                 :parent parent))

(defun program-children (prog)
  "Return the child sub-programs of PROG, or NIL for a leaf.

For a lifted PROGRAM whose AST is a list (OP . CHILDREN*), this returns
the CHILDREN* unchanged.  For a leaf PROGRAM whose AST is an atom, this
returns NIL.  Raw atoms appearing inside a list AST (as produced by
LISP->PROGRAM with :LIFT-LEAVES NIL) are returned as-is and are *not*
PROGRAM instances; consumers must check with PROGRAMP."
  (let ((ast (program-ast prog)))
    (cond
      ((atom ast) nil)
      ((null (cdr ast)) nil)
      (t (cdr ast)))))

(defun program-arity (prog)
  "Return the number of children of PROG.

A leaf has arity 0.  A list-form (OP . CHILDREN*) has arity (length
CHILDREN*)."
  (length (program-children prog)))
