;;;; pathp.lisp --- the cube, increment 2: PathP and the unified Kan comp.
;;;;
;;;; A cpath (paths.lisp) is a path within ONE type.  A PathP ("path over") is
;;;; the genuinely HETEROGENEOUS path: given a line of types  A : I -> U, a
;;;; PathP A x y is a function  p : (i : I) -> A i  whose endpoints land in
;;;; DIFFERENT fibers,  p i0 : A i0  and  p i1 : A i1.  This is the dependent
;;;; analogue of a path and the shape every higher cubical operation needs.
;;;;
;;;; comp is the UNIFIED Kan operation: it transports a cap along a type-line
;;;; WHILE composing with a tube of side faces.  It strictly generalises both
;;;; primitives of increment 0/1:
;;;;   comp line cap []         ==  transp line cap        (empty tube)
;;;;   comp (const T) cap tube  ==  hcomp T cap tube        (constant line)
;;;; and it fuses them: comp along  ua e  WITH a tube applies the equivalence
;;;; to the composed cap -- univalence and composition in one reduction.
;;;;
;;;; THE WALL (still located, the rungs after this): the tube here is folded in
;;;; value space rather than solved over a face lattice; connections/higher
;;;; hcomp coherence, dependent circle-INDUCTION, and n-truncation / pi_n are
;;;; the remaining climb to pi_4(S^3) = Z/2.

(in-package #:rosette-cubical-core)

;;; ---- PathP: a path over a line of types ------------------------------

(defstruct (cpathp (:constructor %make-cpathp (line function)))
  "A heterogeneous path over a type-line.  LINE : I -> U; FUNCTION maps an
interval endpoint/term to a value, with FUNCTION(:0) : (LINE i0) and
FUNCTION(:1) : (LINE i1).  The endpoints may inhabit DIFFERENT fibers --
that is what distinguishes a PathP from an ordinary (homogeneous) cpath."
  (line nil :type type-line :read-only t)
  (function nil :type function :read-only t))

(defun pathp-app (p r)
  "Apply PathP P at interval term R (endpoints reduce definitionally)."
  (funcall (cpathp-function p) (eval-interval r nil)))

(defun cpathp-i0 (p) "The 0-endpoint, in fiber (LINE i0)." (pathp-app p :0))
(defun cpathp-i1 (p) "The 1-endpoint, in fiber (LINE i1)." (pathp-app p :1))

(defun make-cpathp (line function)
  "Construct a PathP over LINE.  Each endpoint is checked against ITS OWN
fiber: FUNCTION(:0) in (LINE i0), FUNCTION(:1) in (LINE i1)."
  (let ((v0 (funcall function :0))
        (v1 (funcall function :1))
        (a0 (type-line-at line :0))
        (a1 (type-line-at line :1)))
    (unless (in-type-p a0 v0)
      (error "PathP 0-endpoint ~S not in fiber (LINE i0) = ~S." v0 (hott-type-name a0)))
    (unless (in-type-p a1 v1)
      (error "PathP 1-endpoint ~S not in fiber (LINE i1) = ~S." v1 (hott-type-name a1))))
  (%make-cpathp line function))

(defun cpath->pathp (path)
  "A homogeneous cpath is the special case of a PathP over a CONSTANT line.
This exhibits cpath <-> PathP-over-const, the coherence between the two."
  (make-cpathp (make-const-line (cpath-type path))
               (lambda (r) (path-app path r))))

(defun pathp-heterogeneous-p (p)
  "T when P genuinely lives over a line whose two endpoint fibres are
DIFFERENT types -- the strong sense of heterogeneity.  A path whose fibres
share a type descriptor (a constant line, or an auto-equivalence line like
ua succ : Z = Z) is NOT heterogeneous in this sense, even if its endpoint
VALUES differ -- it is an ordinary path in that one type."
  (let ((a0 (type-line-at (cpathp-line p) :0))
        (a1 (type-line-at (cpathp-line p) :1)))
    (not (eq (hott-type-name a0) (hott-type-name a1)))))

;;; ---- transp as a PathP: transport IS a path over the line ------------

(defun transp-fill (line cap)
  "The PathP over LINE from CAP to (transp LINE CAP): transport realised as a
path over the line.  This is the canonical heterogeneous path -- its
endpoints live in the two (possibly distinct) fibers and are joined by the
line's own Kan transport.  A plain cpath cannot express this."
  (let ((endpoint (transp line cap)))
    (make-cpathp line
                 (lambda (r) (case (eval-interval r nil)
                               (:0 cap)
                               (:1 endpoint)
                               (t endpoint))))))

;;; ---- comp: the unified Kan composition -------------------------------

(defun comp (line cap tube)
  "Unified Kan composition along type-line LINE: fold the TUBE of side faces
(value -> value) onto CAP in the i0 fibre, then transport the result along
LINE to the i1 fibre.  Returns the lid in (LINE i1).

Degeneracies (the defining beta-laws, checked in tests):
  (comp line cap '())        = (transp line cap)
  (comp (make-const-line T) cap tube) = (hcomp T cap tube)."
  (let ((src (type-line-at line :0))
        (lid cap))
    (unless (in-type-p src cap)
      (error "comp cap ~S not in (LINE i0) = ~S." cap (hott-type-name src)))
    (dolist (face tube)
      (setf lid (funcall face lid))
      (unless (in-type-p src lid)
        (error "comp tube face left the i0 fibre with ~S." lid)))
    (transp line lid)))

(defun comp-fill (line cap tube)
  "The PathP over LINE from CAP to (comp LINE CAP TUBE) -- the filler of the
open box, as a heterogeneous path."
  (let ((endpoint (comp line cap tube)))
    (make-cpathp line
                 (lambda (r) (case (eval-interval r nil)
                               (:0 cap)
                               (:1 endpoint)
                               (t endpoint))))))

;;; ---- a genuinely distinct-fibre equivalence (for real heterogeneity) -

(defun bit-type ()
  "The two-element type {:zero, :one} -- distinct DESCRIPTOR from Bool."
  (make-hott-type :bit
                  :predicate (lambda (x) (member x '(:zero :one)))
                  :truncation 0))

(defun bool-bit-equivalence ()
  "An equivalence Bool ~= Bit between DISTINCT types: T<->:one, NIL<->:zero.
A Glue line for it has genuinely different fibres at the two endpoints, so a
PathP over  ua (bool-bit)  is heterogeneous in the strongest sense."
  (let ((b (bool-type)) (bit (bit-type)))
    (make-hott-equivalence
     b bit
     (lambda (x) (if x :one :zero))
     (lambda (y) (eq y :one))
     :left-homotopy (lambda (x) (crefl-hott b x))
     :right-homotopy (lambda (y) (crefl-hott bit y)))))
