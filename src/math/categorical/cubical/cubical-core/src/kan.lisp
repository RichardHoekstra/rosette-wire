;;;; kan.lisp --- Kan operations: transp, hcomp, and computational univalence.
;;;;
;;;; This is where the wall is crossed.  A LINE OF TYPES is a function
;;;; A : I -> U.  The Kan operation  transp A x  transports x : A i0 to
;;;; A i1.  In cubical type theory transp is defined BY RECURSION ON THE
;;;; STRUCTURE OF THE TYPE-LINE, and that structural definition is what
;;;; makes univalence compute:
;;;;
;;;;   * constant line   -> transp = identity            (regularity beta)
;;;;   * Glue line ua(e)  -> transp = e.fun              (univalence beta)
;;;;
;;;; Contrast: rosette-hott-core (book-HoTT) has  univalence-transport, which
;;;; only works by digging the equivalence back out of an OPAQUE path
;;;; witness that was stuffed in by hand -- transport itself never sees a
;;;; "line of types" and is STUCK on a genuine type family (families.lisp
;;;; refuses to move a value when the source/target fibers differ).  Here
;;;; transp is a real structural recursion, and ua-beta fires by it.

(in-package #:rosette-cubical-core)

;;; A type-line carries: a TAG (its head structure, used by transp), an
;;; AT function  endpoint -> hott-type, and a TRANSP-FN closure encoding
;;; the cubical reduction rule for THIS line's head.  Building a line
;;; means committing to how transp computes on it -- the structural
;;; recursion is reified as the closure, so there is no opaque axiom.

(defstruct (type-line (:constructor %make-type-line (tag at-fn transp-fn)))
  "A line of types  A : I -> U.
TAG       -- :const or :glue, the head the Kan operation dispatches on.
AT-FN     -- (endpoint -> hott-type).
TRANSP-FN -- (value -> value): the cubical transp reduction for this head."
  (tag nil :read-only t)
  (at-fn nil :type function :read-only t)
  (transp-fn nil :type function :read-only t))

(defun type-line-at (line r)
  "The type A r along LINE at interval term R (endpoints reduce)."
  (let ((v (eval-interval r nil)))
    (funcall (type-line-at-fn line) v)))

(defun make-const-line (type)
  "The constant line  lambda i. TYPE.  transp is the identity (regularity)."
  (%make-type-line
   :const
   (lambda (endpoint) (declare (ignore endpoint)) type)
   #'identity))

(defun make-glue-line (equivalence)
  "The Glue line for an equivalence  e : A ~= B, i.e. the type-line
realising  ua e : A = B.

THE univalence beta-rule is baked in structurally: transp along this
line is exactly the equivalence's forward map.  (Cubical Glue is more
general -- a partial type glued to a base via an equivalence on a
cofibration; this is the total, single-equivalence case, which is the
case ua needs.)"
  (let ((a (hott-equivalence-source equivalence))
        (b (hott-equivalence-target equivalence)))
    (%make-type-line
     :glue
     (lambda (endpoint)
       (ecase endpoint (:0 a) (:1 b)))
     (lambda (x) (equiv-forward equivalence x)))))

(defun make-type-line (tag at transp-fn)
  "General constructor (escape hatch for custom heads)."
  (%make-type-line tag at transp-fn))

;;; ---- The Kan transport ------------------------------------------------

(defun transp (line value)
  "Transport VALUE : (LINE i0) along LINE to (LINE i1).

Defined by recursion on LINE's head (its TAG).  For :const this is the
identity; for :glue (= ua e) this REDUCES to e.fun applied to VALUE.
The result is checked to inhabit (LINE i1)."
  (let ((src (funcall (type-line-at-fn line) :0))
        (dst (funcall (type-line-at-fn line) :1)))
    (unless (in-type-p src value)
      (error "transp source ~S is not in (LINE i0) = ~S."
             value (hott-type-name src)))
    (let ((out (funcall (type-line-transp-fn line) value)))
      (unless (in-type-p dst out)
        (error "transp produced ~S outside (LINE i1) = ~S."
               out (hott-type-name dst)))
      out)))

(defun transp-const-id-p (type value &key (test #'eql))
  "Regularity/beta check: transp over the constant line is the identity."
  (funcall test (transp (make-const-line type) value) value))

;;; ---- Homogeneous composition -----------------------------------------
;;;
;;; hcomp fills an open box: given a CAP value u0 : A at the i=0 face and
;;; a family of side faces (the tube) it returns the i=1 face.  We model
;;; the computational core: the tube is a list of functions of the
;;; composition dimension; the missing lid is filled by applying them in
;;; sequence from the cap.  This captures the operational content (paths
;;; compose by hcomp) without a full face-lattice solver.

(defun hcomp (type cap tube)
  "Homogeneous composition in TYPE.  CAP is the bottom (i=0) value; TUBE
is a list of side closures (value -> value), each a path-segment applied
at the composition's 1-end.  Returns the lid (i=1) value."
  (let ((acc cap))
    (dolist (face tube)
      (setf acc (funcall face acc))
      (unless (in-type-p type acc)
        (error "hcomp face produced ~S outside ~S."
               acc (hott-type-name type))))
    acc))

(defun hcomp-cap (cap) "The cap (degenerate tube): hcomp with no sides." cap)

;;; ---- Computational univalence ----------------------------------------

(defun ua-line (equivalence)
  "The type-line  ua e : A = B  as a Glue line (the cubical realisation)."
  (make-glue-line equivalence))

(defun ua (equivalence)
  "ua e : A = B as a cubical PATH in the universe, carrying the Glue line
as its computational content (so transp along it fires the beta-rule).
The path's endpoints reduce definitionally to the source and target type
descriptors."
  (let ((line (ua-line equivalence)))
    ;; A path in the universe: I -> U, endpoints A and B by computation.
    (list :ua-path line)))

(defun ua-beta (equivalence value)
  "THE headline.  transp (ua e) value  ==  e.fun value, BY COMPUTATION on
the Glue line -- not by reading an opaque witness.  Returns the
transported value."
  (transp (ua-line equivalence) value))
