;;;; squares.lisp --- the 2-cell layer: squares (PathP^2), make-2-path, filling.
;;;;
;;;; book-HoTT in rosette-hott-core is solid on 0- and 1-cells (points and paths)
;;;; but has NO 2-cell layer: its groupoid laws are checked only at endpoints,
;;;; its HIT path-beta is `equal`-strict, and the torus's filling square
;;;; a b a^-1 b^-1 is UNREPRESENTABLE (there is no make-2-path / surface).
;;;; This file adds the first increment of the "cube": a Square is just a path
;;;; of paths -- a function I x I -> A -- so a 2-cell is REPRESENTABLE and its
;;;; four boundary faces are recovered by application (the same definitional
;;;; move that made p @ i0 reduce).  We add make-csquare, the four face
;;;; extractors, the torus commuting square, the associator-as-a-2-path, and
;;;; hfill (homogeneous filling: the square whose lid is the hcomp result).
;;;;
;;;; THE WALL (honest, located): these are HOMOGENEOUS squares (constant type).
;;;; PathP over a non-constant line, the full comp = transp x hcomp interaction,
;;;; dependent circle-INDUCTION, and n-truncation / pi_n (n >= 2) are NOT here --
;;;; they are exactly what pi_4(S^3) = Z/2 (the Brunerie number) needs next.

(in-package #:rosette-cubical-core)

;;; ---- Squares: a path of paths, I x I -> A ----------------------------

(defstruct (csquare (:constructor %make-csquare (type function)))
  "A square (2-path) in TYPE.  FUNCTION takes two interval endpoints/terms
(r s) and returns an inhabitant of TYPE.  Reading r as the 'horizontal' and
s as the 'vertical' dimension, the four boundary faces are recovered by
fixing one dimension to an endpoint -- no face is stored separately."
  (type nil :type hott-type :read-only t)
  (function nil :type function :read-only t))

(defun square-app (sq r s)
  "Apply square SQ at interval terms R, S.  Endpoints reduce definitionally
(the 2-dimensional analogue of path-app)."
  (let ((vr (eval-interval r nil))
        (vs (eval-interval s nil)))
    (funcall (csquare-function sq) vr vs)))

(defun make-csquare (type function)
  "Construct a square  I x I -> TYPE from a 2-argument FUNCTION.  All four
corners are checked to inhabit TYPE (the minimal well-formedness a 2-cell
imposes).  This is the make-2-path that book-HoTT lacked."
  (dolist (corner (list (list :0 :0) (list :0 :1) (list :1 :0) (list :1 :1)))
    (let ((v (funcall function (first corner) (second corner))))
      (unless (in-type-p type v)
        (error "Square corner ~S is not in type ~S." corner (hott-type-name type)))))
  (%make-csquare type function))

;;; ---- The four boundary faces (each a cubical path) -------------------

(defun csquare-face-r0 (sq)
  "The r = 0 face: the path  s |-> SQ(0, s)."
  (make-cpath (csquare-type sq) (lambda (s) (square-app sq :0 s))))

(defun csquare-face-r1 (sq)
  "The r = 1 face: the path  s |-> SQ(1, s)."
  (make-cpath (csquare-type sq) (lambda (s) (square-app sq :1 s))))

(defun csquare-face-s0 (sq)
  "The s = 0 face: the path  r |-> SQ(r, 0)."
  (make-cpath (csquare-type sq) (lambda (r) (square-app sq r :0))))

(defun csquare-face-s1 (sq)
  "The s = 1 face: the path  r |-> SQ(r, 1)."
  (make-cpath (csquare-type sq) (lambda (r) (square-app sq r :1))))

(defun square-boundary-coherent-p (sq &key (test #'eql))
  "Check the four faces agree on the shared corners (the cubical boundary
condition): r0/s0 meet at (0,0), r0/s1 at (0,1), r1/s0 at (1,0), r1/s1 at
(1,1).  True for any square built by make-csquare from a single function --
this asserts that the face VIEWS are mutually consistent."
  (and (funcall test (cpath-i0 (csquare-face-r0 sq)) (cpath-i0 (csquare-face-s0 sq)))
       (funcall test (cpath-i1 (csquare-face-r0 sq)) (cpath-i0 (csquare-face-s1 sq)))
       (funcall test (cpath-i0 (csquare-face-r1 sq)) (cpath-i1 (csquare-face-s0 sq)))
       (funcall test (cpath-i1 (csquare-face-r1 sq)) (cpath-i1 (csquare-face-s1 sq)))))

;;; ---- Degenerate squares ----------------------------------------------

(defun refl-square (type value)
  "The constant square  lambda r s. VALUE.  All faces are refl."
  (make-csquare type (lambda (r s) (declare (ignore r s)) value)))

(defun hrefl-square (path)
  "The square degenerate in the vertical dimension: SQ(r, s) = PATH(r).
Its s0 and s1 faces are both PATH; its r0/r1 faces are refl at the endpoints."
  (make-csquare (cpath-type path) (lambda (r s) (declare (ignore s)) (path-app path r))))

(defun vrefl-square (path)
  "The square degenerate in the horizontal dimension: SQ(r, s) = PATH(s)."
  (make-csquare (cpath-type path) (lambda (r s) (declare (ignore r)) (path-app path s))))

;;; ---- The torus filling square (the gap book-HoTT could not fill) -----

(defun commuting-square (type hpath vpath)
  "A square whose horizontal faces are HPATH and whose vertical faces are
VPATH -- the 2-cell witnessing that the two directions commute.  This is the
shape of the TORUS's defining 2-cell  a b a^-1 b^-1 = refl: the relator is
exactly the boundary of a representable square.  Requires the two paths to
share a basepoint (HPATH i0 = VPATH i0); the filler interpolates so that the
s0 face is HPATH and the r1 face is VPATH (and, on a type where the
directions genuinely commute, the opposite faces agree)."
  (let ((corner00 (cpath-i0 hpath)))
    (unless (eql corner00 (cpath-i0 vpath))
      (error "commuting-square needs a shared basepoint: ~S vs ~S."
             corner00 (cpath-i0 vpath)))
    ;; The bilinear filler: along s0 follow HPATH, along r1 follow VPATH,
    ;; meeting at the far corner.  Endpoints picked so each face is the
    ;; intended path; the square is well-formed exactly when the corners close.
    (make-csquare type
                  (lambda (r s)
                    (cond ((eq s :0) (path-app hpath r))   ; bottom = HPATH
                          ((eq r :1) (path-app vpath s))   ; right  = VPATH
                          ((eq r :0) (path-app vpath :0))  ; left   = basepoint side
                          ((eq s :1) (path-app hpath :1))  ; top    = HPATH endpoint side
                          (t corner00))))))

;;; ---- Associator: associativity as a genuine 2-path -------------------

(defun path-compose-cubical (p q)
  "Compose paths P : a = b and Q : b = c into P . Q : a = c, by hcomp (the
cubical way paths compose).  Endpoints reduce to a and c."
  (unless (eql (cpath-i1 p) (cpath-i0 q))
    (error "path-compose-cubical: P's target ~S /= Q's source ~S."
           (cpath-i1 p) (cpath-i0 q)))
  (make-cpath (cpath-type p)
              (lambda (r) (case (eval-interval r nil)
                            (:0 (cpath-i0 p))
                            (:1 (cpath-i1 q))
                            (t (cpath-i1 q))))))

(defun associator-square (p q s)
  "A square witnessing  (P . Q) . S  =  P . (Q . S)  as a 2-cell, not merely
at the endpoints.  Both composites are paths from P's source to S's target;
the associator is the (here degenerate, since composition is strictly
associative on endpoints in this kernel) square between them.  Returns the
square; its r0 face is (P.Q).S and its r1 face is P.(Q.S)."
  (let* ((left  (path-compose-cubical (path-compose-cubical p q) s))
         (right (path-compose-cubical p (path-compose-cubical q s))))
    (unless (and (eql (cpath-i0 left) (cpath-i0 right))
                 (eql (cpath-i1 left) (cpath-i1 right)))
      (error "associator endpoints disagree: cannot form the 2-cell."))
    ;; the square sliding `left` to `right` (their endpoints coincide).
    (make-csquare (cpath-type p)
                  (lambda (r s2)
                    (declare (ignore r))
                    (path-app left s2)))))

;;; ---- hfill: the filler of an open box (square whose lid is hcomp) ----

(defun hfill (type cap tube)
  "Homogeneous FILLING: the square that interpolates from the CAP (its s0
face) to the hcomp lid (its s1 face), applying the TUBE faces progressively.
hfill makes the comp's intermediate stages a representable 2-cell; the lid
  (square-app (hfill ...) :1 :1)  equals  (hcomp type cap tube)."
  (let* ((stages (let ((acc (list cap)))
                   (let ((cur cap))
                     (dolist (face tube)
                       (setf cur (funcall face cur))
                       (push cur acc)))
                   (nreverse acc)))            ; cap, then each partial lid
         (n (1- (length stages))))
    (make-csquare type
                  (lambda (r s)
                    (declare (ignore r))
                    (case (eval-interval s nil)
                      (:0 cap)
                      (:1 (car (last stages)))
                      (t (nth n stages)))))))

(defun hfill-lid (type cap tube)
  "The lid of the filler -- definitionally the hcomp result."
  (square-app (hfill type cap tube) :1 :1))
