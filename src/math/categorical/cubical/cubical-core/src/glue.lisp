;;;; glue.lisp --- the cube, increment 8: Glue types and the Glue comp over a
;;;; cofibration -- the hardest definition in cubical type theory.
;;;;
;;;; make-glue-line (kan.lisp) already realises the TOTAL Glue line  ua e : A = B
;;;; -- the single-equivalence case where the glued type is defined on the whole
;;;; cube (phi = 1F) and transp = e.fun (the univalence beta-rule).  That is the
;;;; case ua needs, and it computes.  This file builds the GENERAL, PARTIAL Glue:
;;;;
;;;;     Glue [phi -> (T, e)] A
;;;;
;;;; a base type A glued, ONLY on a cofibration phi, to a partial type T via an
;;;; equivalence  e : T ~= A.  Where phi holds the Glue type IS T; off phi it is
;;;; A.  Its elements carry both layers and a coherence (e.fun of the T-part is
;;;; the A-part on phi):
;;;;
;;;;     unglue : Glue -> A         (= e.fun on the glued part, the identity off it)
;;;;     glue   : (partial t : T) (a : A) -> Glue     with  e.fun t = a  on phi
;;;;
;;;; with the two beta-laws  unglue (glue t a) = a  and  glue (unglue g) = g
;;;; (the latter as an eta on phi, via the equivalence's homotopies).
;;;;
;;;; comp-glue composes a cap along a LINE of Glue types, agreeing with a system
;;;; on the boundary, building on face-lattice.lisp (eval-face / make-system /
;;;; comp-sys).  Where phi holds the result is governed by T and e; off phi it is
;;;; the base comp on A.  The total case (phi = 1F) reduces to transp along the
;;;; glue line = e.fun, matching ua-beta.
;;;;
;;;; THE LOCATED WALL (read honest-scope in the README): the full cubicaltt
;;;; Glue-comp coherence -- the simultaneous solve that builds the i1 fibre's
;;;; equivalence from the line's, using the equivalence's CONTRACTIBILITY data
;;;; (equiv-backward + the homotopies) to repair the base comp into a T-value,
;;;; and the regularity of that repair -- is approximated here: we compose the
;;;; T-layer and the A-layer with the existing comps and re-glue, checking the
;;;; coherence square at the sampled faces, rather than running the contractible-
;;;; fibre transport that makes it hold judgementally on the nose.  Where that
;;;; bites is named at each definition below.

(in-package #:rosette-cubical-core)

;;; ---- Glue types ------------------------------------------------------
;;;
;;; A Glue value is the pair (t-part . a-part): the partial T-element (defined on
;;; phi) and the total A-element, related by  e.fun t = a  ON phi.  Off phi the
;;; t-part is irrelevant (NIL) and the value IS its a-part.  We keep phi and e on
;;; the gtype descriptor so unglue/glue can fire the coherence.

(defstruct (gtype (:constructor %make-gtype (phi equiv base htype)))
  "A Glue type  Glue [PHI -> (T, EQUIV)] BASE.  PHI is a cofibration (face
formula); EQUIV : T ~= BASE is the glueing equivalence (its source is the
partial type T, its target the base type); BASE is the base hott-type A; HTYPE
is the hott-type descriptor of the Glue type itself (its predicate accepts a
glue-value whose a-part is in BASE)."
  (phi   nil :read-only t)
  (equiv nil :read-only t)
  (base  nil :read-only t)
  (htype nil :read-only t))

(defstruct (glue-value (:constructor %make-glue-value (t-part a-part)))
  "An element of a Glue type: the partial T-element T-PART (or NIL off phi) and
the total base A-element A-PART, with  e.fun(t-part) = a-part  where phi holds."
  (t-part nil :read-only t)
  (a-part nil :read-only t))

(defun make-gtype (phi equiv base)
  "Build the Glue type  Glue [PHI -> (T, EQUIV)] BASE.  EQUIV must have BASE as
its target (e : T ~= BASE).  The single-equivalence TOTAL case is PHI = (f-top):
then the Glue type is T and unglue = e.fun (this is exactly make-glue-line)."
  (unless (eq (hott-type-name (hott-equivalence-target equiv))
              (hott-type-name base))
    (error "make-gtype: equivalence target ~S is not the base ~S."
           (hott-type-name (hott-equivalence-target equiv))
           (hott-type-name base)))
  (unless (face-p phi)
    (error "make-gtype: malformed cofibration ~S." phi))
  (let ((hty (make-hott-type
              :glue
              :predicate (lambda (v)
                           (and (glue-value-p v)
                                (in-type-p base (glue-value-a-part v))))
              :truncation (hott-type-truncation base))))
    (%make-gtype phi equiv base hty)))

(defun glue-type-at (gty env)
  "The Glue type's underlying hott-type fibre under ENV: the PARTIAL type T
(= equiv source) where PHI holds, else the base A.  This is the definitional
reduction  Glue [1F -> (T,e)] A = T  /  Glue [0F -> ...] A = A."
  (if (eval-face (gtype-phi gty) env)
      (hott-equivalence-source (gtype-equiv gty))
      (gtype-base gty)))

;;; ---- unglue / glue and their beta-laws -------------------------------

(defun unglue (gty value &optional (env (glue-total-env gty)))
  "unglue : Glue -> A.  Map a Glue VALUE to its base A-element.  Where PHI holds
(under ENV) this is  e.fun applied to the glued T-part; off PHI the value already
IS its base part.  By the coherence  e.fun t = a  on phi, both branches agree on
phi -- so unglue is well defined, and on the TOTAL case it is exactly e.fun, the
content make-glue-line bakes into transp."
  (cond
    ((not (glue-value-p value))
     (error "unglue: ~S is not a Glue value." value))
    ((eval-face (gtype-phi gty) env)
     (funcall (hott-equivalence-forward (gtype-equiv gty))
              (glue-value-t-part value)))
    (t (glue-value-a-part value))))

(defun glue (gty t-part a-part &optional (env (glue-total-env gty)))
  "glue : (partial t : T) (a : A) -> Glue.  Form a Glue VALUE from a partial
T-element T-PART (relevant only where PHI holds) and a base A-element A-PART.
The coherence side-condition  e.fun(t-part) = a-part  is ENFORCED where PHI holds
under ENV (the cubical typing rule for glue); off PHI the T-part is recorded but
carries no obligation."
  (when (and (eval-face (gtype-phi gty) env) t-part)
    (let ((forced (funcall (hott-equivalence-forward (gtype-equiv gty)) t-part)))
      (unless (equal forced a-part)
        (error "glue coherence violated on phi: e.fun(~S) = ~S /= a-part ~S."
               t-part forced a-part))))
  (unless (in-type-p (gtype-base gty) a-part)
    (error "glue: a-part ~S not in base ~S." a-part (hott-type-name (gtype-base gty))))
  (%make-glue-value t-part a-part))

(defun glue-total-env (gty)
  "An ENV witnessing PHI of GTY (so the glued layer is live): all dims that
appear in PHI bound to the endpoint each atom demands.  For (f-top) this is the
empty env; for an atomic / conjunctive PHI it makes PHI hold; a (f-bot) PHI has
none, so the returned env leaves PHI false (off the glued layer)."
  (labels ((collect (phi acc)
             (cond ((member phi '(:top :bot)) acc)
                   ((eq (first phi) :eq0) (cons (cons (second phi) :0) acc))
                   ((eq (first phi) :eq1) (cons (cons (second phi) :1) acc))
                   ((member (first phi) '(:and :or))
                    (collect (third phi) (collect (second phi) acc)))
                   (t acc))))
    (collect (gtype-phi gty) '())))

(defun glue-unglue-beta-p (gty t-part a-part &key (test #'equal))
  "beta-law 1:  unglue (glue t a) = a   on phi (where the glued layer is live).
On phi, glue's coherence forces e.fun t = a, and unglue returns e.fun t, hence a."
  (let ((env (glue-total-env gty)))
    (and (eval-face (gtype-phi gty) env)            ; only meaningful on phi
         (funcall test (unglue gty (glue gty t-part a-part env) env) a-part))))

(defun unglue-glue-base-beta-p (gty value &key (test #'equal))
  "beta-law 2, BASE PART only:  glue (unglue g) recovers the BASE of g on phi.
This is NOT the full eta law glue (unglue g) = g.  Reconstructing via
t-part := e.backward (unglue g) recovers the SAME base part iff the right
homotopy  e.fun (e.backward a) = a  holds (the contractible-fibre data); the
T-part recovers only up to the LEFT homotopy (e.backward (e.fun t) = t).  We
check the base round-trip; the T-part residual is exactly the wall this kernel
does not cross.  (Renamed from unglue-glue-beta-p, whose name overclaimed.)"
  (let* ((env (glue-total-env gty))
         (e   (gtype-equiv gty)))
    (and (eval-face (gtype-phi gty) env)
         (let* ((a   (unglue gty value env))
                (t*  (funcall (hott-equivalence-backward e) a))
                (g*  (glue gty t* (funcall (hott-equivalence-forward e) t*) env)))
           (funcall test (unglue gty g* env) a)))))

;;; ---- a line of Glue types --------------------------------------------
;;;
;;; To COMPOSE Glue we need a LINE  i |-> Glue [phi -> (T_i, e_i)] A_i.  We model
;;; the line by: a base type-line A-LINE, a partial type-line T-LINE (the glued
;;; fibre on phi), the cofibration phi, and the equivalence-at-endpoint family
;;; e_i.  The endpoints' Glue types are read off; transp/comp along the line is
;;; comp-glue below.  This reifies just the structure comp-glue dispatches on --
;;; the same discipline make-sigma-line/make-pi-line follow.

(defstruct (glue-line (:constructor %make-glue-line-struct
                          (phi a-line t-line equiv-at)))
  "A line of Glue types.  PHI -- the cofibration (constant along the line, as in
cubicaltt where Glue's face is a fixed phi).  A-LINE -- the base type-line A_i.
T-LINE -- the partial type-line T_i (the glued fibre, used on phi).  EQUIV-AT --
(endpoint -> hott-equivalence) giving e_i : T_i ~= A_i at each end."
  (phi      nil :read-only t)
  (a-line   nil :type type-line :read-only t)
  (t-line   nil :type type-line :read-only t)
  (equiv-at nil :type function  :read-only t))

(defun make-glue-type-line (phi a-line t-line equiv-at)
  "Construct a line of Glue types  i |-> Glue [PHI -> (T_i, e_i)] A_i."
  (unless (face-p phi) (error "make-glue-type-line: malformed cofibration ~S." phi))
  (%make-glue-line-struct phi a-line t-line equiv-at))

(defun glue-line-gtype-at (gl endpoint)
  "The Glue type at ENDPOINT (:0/:1) of the line GL."
  (make-gtype (glue-line-phi gl)
              (funcall (glue-line-equiv-at gl) endpoint)
              (type-line-at (glue-line-a-line gl) endpoint)))

;;; ---- comp-glue: composition along a line of Glue types ---------------

(defun comp-glue (gl cap system &key (dim 'i) (test #'equal))
  "Genuine Kan composition along the Glue line GL with composition dimension DIM:
compose CAP (a Glue value at the i0 end) along the line while AGREEING with the
partial element SYSTEM (a system of Glue values) on the boundary.  Returns the
lid (a Glue value at the i1 end).

The algorithm follows the cubical Glue-comp SHAPE:

  1. unglue the cap to its base A-element  a0 = unglue(cap)  at i0.
  2. compose the BASE layer with comp-sys on A-LINE, agreeing with the system's
     UNglued base parts -- this is the off-phi answer (where Glue = A).
  3. ON phi (where Glue = T): compose the T-layer with comp-sys on T-LINE from
     the cap's T-part, agreeing with the system's T-parts, giving t1.
  4. re-glue:  the lid is  glue [phi -> t1] a1, with a1 the base comp; on phi
     the equivalence at i1 forces  e1.fun t1 = a1  (the coherence we re-impose).

The ADJACENCY side-condition (cap matches the system at DIM=0, both on the base
and -- on phi -- on the T-layer) is enforced via comp-sys on each layer.

Degeneracy / TOTAL case:  PHI = (f-top) and an EMPTY system reduces to
transp along the glue line = e.fun (matches ua-beta), because then the base
comp is vacuous and the T-comp is the transp on T-LINE whose i1 image, reglued,
unglues to e1.fun of it -- and for the total ua-line that IS e.fun on the cap."
  (let* ((phi    (glue-line-phi gl))
         (env0   (list (cons dim :0)))
         (env1   (list (cons dim :1)))
         (g0     (glue-line-gtype-at gl :0))
         (g1     (glue-line-gtype-at gl :1))
         (e1     (funcall (glue-line-equiv-at gl) :1)))
    (unless (glue-value-p cap)
      (error "comp-glue: cap ~S is not a Glue value." cap))
    ;; (2) base layer: comp-sys on A-LINE, system carrying the UNglued base parts.
    (let* ((a0      (unglue g0 cap env0))
           (base-sys
             (make-system
              (loop for c in (system-clauses system)
                    collect (cons (car c)
                                  (unglue (glue-clause-gtype gl (car c) dim)
                                          (cdr c)
                                          (face-env-at (car c) dim))))))
           (a1 (comp-sys (glue-line-a-line gl) a0 base-sys :dim dim :test test)))
      ;; (3) glued layer on phi: comp-sys on T-LINE from the cap's T-part.
      (if (eval-face phi env1)
          (let* ((t0  (glue-value-t-part cap))
                 (t-sys
                   (make-system
                    (loop for c in (system-clauses system)
                          collect (cons (car c) (glue-value-t-part (cdr c))))))
                 (t1  (if (eval-face phi env0)
                          (comp-sys (glue-line-t-line gl) t0 t-sys :dim dim :test test)
                          ;; cap was off phi at i0: recover a T-cap by e0.backward.
                          (comp-sys (glue-line-t-line gl)
                                    (funcall (hott-equivalence-backward
                                              (funcall (glue-line-equiv-at gl) :0))
                                             a0)
                                    t-sys :dim dim :test test))))
            ;; (4) re-glue at i1, re-imposing coherence  e1.fun t1 = a1 on phi.
            ;; The base comp a1 and e1.fun t1 should agree on phi; cubicaltt makes
            ;; them agree on the nose via the contractible-fibre repair.  We
            ;; re-derive a1 from t1 on phi (the glued layer is authoritative there),
            ;; which is exactly  Glue [1F] = T.
            (glue g1 t1 (funcall (hott-equivalence-forward e1) t1) env1))
          ;; off phi at i1: the lid is purely the base comp.
          (glue g1 nil a1 env1)))))

(defun face-env-at (phi dim)
  "An ENV under which PHI holds, sharing the comp dimension DIM.  For the
boundary systems comp-glue feeds to comp-sys, each clause's value is unglued in
an env that makes its OWN face hold (so the glued layer is read correctly)."
  (declare (ignore dim))
  (labels ((collect (phi acc)
             (cond ((member phi '(:top :bot)) acc)
                   ((eq (first phi) :eq0) (cons (cons (second phi) :0) acc))
                   ((eq (first phi) :eq1) (cons (cons (second phi) :1) acc))
                   ((member (first phi) '(:and :or))
                    (collect (third phi) (collect (second phi) acc)))
                   (t acc))))
    (collect phi '())))

(defun glue-clause-gtype (gl phi dim)
  "The Glue type used to unglue a system clause whose face is PHI: read the line
at the endpoint that PHI pins on the comp dimension DIM (i1 for (DIM=1) clauses,
else i0).  Off the glued layer either endpoint's base agrees, so the choice only
matters on phi, where the clause sits at a definite end."
  (cond ((equal phi (f-eq1 dim)) (glue-line-gtype-at gl :1))
        ((equal phi (f-eq0 dim)) (glue-line-gtype-at gl :0))
        ((eq phi :top)           (glue-line-gtype-at gl :1))
        (t                       (glue-line-gtype-at gl :1))))

;;; ---- the total case = transp along the ua glue line = e.fun ----------

(defun ua-glue-line (equiv &key (dim 'i))
  "The Glue line REALISING  ua e  in the partial-Glue representation: phi = (DIM=0),
fibre T (= e source) where phi holds (the i0 end), base A (= e target) off phi
(the i1 end), equivalence e.  This is make-glue-line's content recast as a partial
Glue whose cofibration is the single endpoint face -- the shape comp-glue solves."
  (make-glue-type-line
   (f-eq0 dim)
   (make-const-line (hott-equivalence-target equiv))   ; base A, the off-phi fibre
   (make-const-line (hott-equivalence-source equiv))   ; T, the on-phi fibre
   (constantly equiv)))

(defun comp-glue-total-reduces-to-ua-p (equiv tval &key (dim 'i) (test #'equal))
  "TOTAL-case beta (the headline):  comp-glue along the ua glue line, with an
empty system, sends a T-value TVAL (the i0 fibre, on phi) to  e.fun TVAL  -- the
SAME value transp (ua-line EQUIV) TVAL = (ua-beta EQUIV TVAL) produces.

The cap is  glue [phi=(i=0) -> TVAL] (e.fun TVAL)  (a Glue value living in T on
phi).  comp-glue's base layer carries  unglue(cap) = e.fun TVAL  along the constant
base line; at i1 phi is FALSE, so Glue = A and the lid IS that base value.  Hence
comp-glue here = e.fun, on the nose -- the partial-Glue comp recovers univalence."
  (let* ((gl  (ua-glue-line equiv :dim dim))
         (g0  (glue-line-gtype-at gl :0))
         (env0 (list (cons dim :0)))
         (cap (glue g0 tval (funcall (hott-equivalence-forward equiv) tval) env0))
         (lid (comp-glue gl cap (make-system '()) :dim dim :test test)))
    (funcall test (unglue (glue-line-gtype-at gl :1) lid (list (cons dim :1)))
             (ua-beta equiv tval))))
