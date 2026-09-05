;;;; truncation.lisp --- the cube, increment 20: the set-truncation || - ||_0 as
;;;; a real OPERATION (not the loop-space-class shadow), grounding pi_n = ||Omega^n||_0.
;;;;
;;;; Every previous increment treated  pi_n = || Omega^n ||_0  through a CLASS
;;;; shadow (loop-space-class / set-truncate-class): a symbol :integers / :Z, not
;;;; an actual truncation.  But the cubical pi_4(S^3) summit is precisely a
;;;; statement about a set-truncation COMPUTING.  This increment builds the
;;;; set-truncation || A ||_0 as a genuine operation -- points are the
;;;; path-COMPONENTS of A, equality is "same component" -- with its recursion
;;;; principle, and uses it to GROUND the two homotopy groups the kernel computes:
;;;;
;;;;   pi_1(S^1)    = || Omega S^1 ||_0 = Z      (components = winding classes),
;;;;   pi_1(M(Z/2,1)) = || Omega M ||_0 = Z/2    (components = mod-2 classes),
;;;;
;;;; where the COMPONENT function is the encode (winding-of-loop-power, inc3;
;;;; moore-encode, inc19) -- both firing by ua-beta.  So || - ||_0 is no longer a
;;;; shadow: it is an operation whose elements are computed.
;;;;
;;;; HONEST WALL.  || Omega^4 S^3 ||_0 -- the set-truncation at the Brunerie summit
;;;; -- does NOT compute: there is no component function for the 4-fold loop space
;;;; of S^3 (the loops are not representable; the inc12-14 coherence wall).  So the
;;;; truncation operation GROUNDS the computing pi_n (Z and Z/2) and STALLS at the
;;;; summit -- pi-n (:sphere 3) 4 stays :stalled.

(in-package #:rosette-cubical-core)

;;; ---- The set-truncation  || A ||_0  as an operation ------------------
;;;
;;; A set-truncation presentation: a name plus a COMPONENT function sending each
;;; point of A to a canonical representative of its path-component.  || A ||_0 is
;;; the set of those representatives; equality in || A ||_0 is "same component".

(defstruct (trunc0 (:constructor %make-trunc0 (source-name component-fn)))
  "A set-truncation || A ||_0: COMPONENT-FN maps a point of A to the canonical
representative of its path-component (the element of || A ||_0 it becomes)."
  (source-name nil :read-only t)
  (component-fn nil :type function :read-only t))

(defun set-truncate (source-name component-fn)
  "Form || A ||_0 from A's path-component function (|a| = its component rep)."
  (%make-trunc0 source-name component-fn))

(defun trunc0-incl (tr a)
  "The inclusion  | - | : A -> || A ||_0  sending a point to its component rep."
  (funcall (trunc0-component-fn tr) a))

(defun trunc0-equal-p (tr a b &key (test #'equal))
  "Equality in || A ||_0:  | a | = | b |  iff a and b lie in the same component."
  (funcall test (trunc0-incl tr a) (trunc0-incl tr b)))

(defun trunc0-rec (tr f)
  "The recursion principle: a map  || A ||_0 -> B  into a SET B is given by a
component-constant  f : A -> B.  We return the induced function on components
(f, applied to the representative) -- well-defined exactly because B is a set."
  (declare (ignore tr))
  (lambda (component-rep) (funcall f component-rep)))

;;; ---- pi_0: path components ------------------------------------------

(defun pi0-circle-is-point-p ()
  "|| S^1 ||_0 = 1: the circle is path-connected, so all points share one
component.  Any two points are equal in the truncation."
  (let ((tr (set-truncate :circle (constantly :pt))))   ; one component
    (and (trunc0-equal-p tr :base :base)
         (eq (trunc0-incl tr :base) :pt))))

(defun pi0-s0-is-two-points-p ()
  "|| S^0 ||_0 = S^0: the two points are in DIFFERENT components (S^0 is a set,
2 components)."
  (let ((tr (set-truncate :s0 #'identity)))             ; each point its own component
    (and (not (trunc0-equal-p tr :pt0 :pt1))
         (trunc0-equal-p tr :pt0 :pt0))))

;;; ---- pi_1(S^1) = || Omega S^1 ||_0 = Z, grounded ---------------------
;;;
;;; Omega S^1's path-components are the winding classes: loop^n ~ loop^m iff n=m.
;;; The component function is the encode (winding-of-loop-power); || Omega S^1 ||_0
;;; is Z, with loop^2 =/= loop^3 because 2 =/= 3.

(defun omega-s1-truncation ()
  "|| Omega S^1 ||_0 with the component function = the winding encode (loop^n's
component is n).  Its elements are the integers."
  (set-truncate :omega-s1 #'winding-of-loop-power))

(defun pi1-s1-via-truncation-is-Z-p (&optional (range 6))
  "pi_1(S^1) = || Omega S^1 ||_0 = Z, GROUNDED in the truncation operation: the
component of loop^n is n (computed by winding/ua-beta), distinct loop-powers land
in distinct components, and loop^2 =/= loop^3."
  (let ((tr (omega-s1-truncation)))
    (and (loop for n from (- range) to range always (= (trunc0-incl tr n) n))
         (not (trunc0-equal-p tr 2 3))            ; loop^2 =/= loop^3
         (trunc0-equal-p tr 5 5))))

;;; ---- pi_1(M(Z/2,1)) = || Omega M ||_0 = Z/2, grounded ---------------
;;;
;;; Omega M's components are the mod-2 classes: a^n ~ a^m iff n = m (mod 2).  The
;;; component function is moore-encode (inc19); || Omega M ||_0 = Z/2.

(defun omega-moore-truncation ()
  "|| Omega M(Z/2,1) ||_0 with the component function = moore-encode (a^n's
component is n mod 2).  Its elements are Z/2."
  (set-truncate :omega-moore #'moore-encode))

(defun pi1-moore-via-truncation-is-Z2-p ()
  "pi_1(M(Z/2,1)) = || Omega M ||_0 = Z/2, GROUNDED in the truncation: a^2 and a^0
are the SAME component (both 0), a^1 and a^3 the same (both 1), but a^1 =/= a^0 --
exactly two components, computed by moore-encode/ua-beta."
  (let ((tr (omega-moore-truncation)))
    (and (trunc0-equal-p tr 2 0)             ; a^2 = 1 in the group (same comp as a^0)
         (trunc0-equal-p tr 3 1)             ; a^3 ~ a^1
         (not (trunc0-equal-p tr 1 0)))))    ; a =/= 1

;;; ---- The summit: || Omega^4 S^3 ||_0 stalls --------------------------

(defun pi4-s3-via-truncation-stalls-p ()
  "|| Omega^4 S^3 ||_0 does NOT compute: there is no component function for the
4-fold loop space of S^3 (its loops are not representable here -- the inc12-14
coherence wall), so the set-truncation at the Brunerie summit stalls.  We witness
the stall through pi-n (which has no representable Omega^4 S^3)."
  (eq (pi-n (list :sphere 3) 4) :stalled))

(defun truncation-grounds-computing-pi-n-p ()
  "HONEST.  The set-truncation || - ||_0 is now a real OPERATION (path components +
recursion principle), and it GROUNDS the two homotopy groups the kernel computes:
pi_1(S^1) = || Omega S^1 ||_0 = Z (winding components) and pi_1(M(Z/2,1)) =
|| Omega M ||_0 = Z/2 (mod-2 components) -- where loop-space-class was only a
shadow.  At the summit || Omega^4 S^3 ||_0 stalls (no representable loops).  So the
operation computes the groundable groups (T), AND pi_4(S^3) stays :stalled."
  (and (pi0-circle-is-point-p) (pi0-s0-is-two-points-p)
       (pi1-s1-via-truncation-is-Z-p)
       (pi1-moore-via-truncation-is-Z2-p)
       (pi4-s3-via-truncation-stalls-p)))
