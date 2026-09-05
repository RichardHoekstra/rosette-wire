;;;; type-formers.lisp --- the cube, increment 7: Kan comp for Sigma / Pi / Path.
;;;;
;;;; This begins the architectural lift.  In cubical type theory comp is defined
;;;; BY RECURSION ON THE TYPE, with one structural clause per type-former.  Each
;;;; clause says how to transport a value of that former along a line of such
;;;; types, in terms of comp on the COMPONENT lines:
;;;;
;;;;   Sigma:  transp (Sigma A B) (a0 . b0)
;;;;             = (transp A a0 . transp (B over the a-path) b0)     -- componentwise
;;;;   Pi:     transp (Pi A B) f
;;;;             = lambda a1. transp (B ...) (f (transp^back A a1))   -- contravariant domain
;;;;   Path:   transp (Path A u v) p
;;;;             = the path  i |-> transp A (p i),  endpoints transported    -- pointwise
;;;;
;;;; We reify each clause as the type-line's transp-fn (exactly as make-glue-line
;;;; reified ua-beta), so the EXISTING transp/comp fire the structural rule with
;;;; no new dispatch.  These are the genuine type-former comps that Omega(S^2)
;;;; and ultimately pi_4(S^3)=Z/2 are built from.
;;;;
;;;; STILL AHEAD (the rest of the lift): the Glue comp (not just transp), the
;;;; higher hcomp filling S^2, and connectivity bookkeeping.  Here: the three
;;;; structural recursion clauses, computing.

(in-package #:rosette-cubical-core)

;;; ---- Sigma ------------------------------------------------------------

(defun sigma-type (a-type b-type-fn)
  "The dependent pair type  Sigma (a : A), B a.  A value is (a . b) with
a : A-TYPE and b : (B-TYPE-FN a)."
  (make-hott-type
   :sigma
   :predicate (lambda (v)
                (and (consp v)
                     (in-type-p a-type (car v))
                     (in-type-p (funcall b-type-fn (car v)) (cdr v))))
   :truncation 0))

(defun make-sigma-line (a-line b-type-at b-transp)
  "A line of Sigma types.  A-LINE : I -> U is the first component.  B-TYPE-AT
is (endpoint a-value -> hott-type), the second-component fibre.  B-TRANSP is
(a0 b0 -> b1), the second-component transport along the line over the a-path.
transp on this line is COMPONENTWISE comp:  (a0 . b0) |-> (transp A a0 . b1)."
  (make-type-line
   :sigma
   (lambda (e)
     (sigma-type (type-line-at a-line e)
                 (lambda (a) (funcall b-type-at e a))))
   (lambda (pair)
     (cons (transp a-line (car pair))
           (funcall b-transp (car pair) (cdr pair))))))

;;; ---- Pi ---------------------------------------------------------------

(defun pi-type (&optional (name :pi))
  "A (non-dependent-checked) function type: its inhabitants are functions.
Predicate accepts any function, since extensional fibre checking is undecidable."
  (make-hott-type name :predicate #'functionp :truncation 0))

(defun make-pi-line (a-transp-back b-transp)
  "A line of Pi types.  A-TRANSP-BACK : a1 -> a0 transports a domain argument
BACKWARD along the line (contravariance).  B-TRANSP : (a0 (f a0)) -> b1 moves
the codomain value forward.  transp on this line:
  f  |->  lambda a1. B-TRANSP (A-TRANSP-BACK a1) (f (A-TRANSP-BACK a1))."
  (make-type-line
   :pi
   (lambda (e) (declare (ignore e)) (pi-type))
   (lambda (f)
     (lambda (a1)
       (let ((a0 (funcall a-transp-back a1)))
         (funcall b-transp a0 (funcall f a0)))))))

;;; ---- Path -------------------------------------------------------------

(defun path-type (inner-type u v)
  "The path type  Path INNER-TYPE u v: inhabitants are cpaths in INNER-TYPE
from u to v."
  (make-hott-type
   :path
   :predicate (lambda (p) (and (cpath-p p)
                               (eq (hott-type-name (cpath-type p))
                                   (hott-type-name inner-type))))
   :truncation 1))

(defun make-path-line (inner-line)
  "A line of Path types over INNER-LINE : I -> U.  transp on this line moves a
path POINTWISE:  p  |->  the path  i |-> transp INNER-LINE (p i).  (Endpoints
move with the line; this is the Path-former clause of comp.)"
  (make-type-line
   :path
   (lambda (e)
     (let ((it (type-line-at inner-line e)))
       (path-type it
                  ;; endpoint descriptors are nominal here; the predicate only
                  ;; checks the inner type, which is what transp needs.
                  nil nil)))
   (lambda (p)
     (make-cpath (type-line-at inner-line :1)
                 (lambda (i) (transp inner-line (path-app p i)))))))
