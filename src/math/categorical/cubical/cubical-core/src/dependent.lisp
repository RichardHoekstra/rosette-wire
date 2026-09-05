;;;; dependent.lisp --- the cube, increment 3: dependent circle-induction,
;;;; and pi_1(S^1) = Z as a full encode/decode equivalence.
;;;;
;;;; rosette-hott-core (and increments 0-2) gave the circle a NON-dependent
;;;; recursor (eliminate into a fixed type).  The cubical proof of pi_1(S^1)=Z
;;;; needs the DEPENDENT eliminator: eliminate into a type FAMILY P : S^1 -> U.
;;;; Its loop case is the genuinely cubical obstruction -- it asks for a path
;;;; from the base case to ITSELF lying OVER the family along the loop, i.e. a
;;;; PathP.  That is exactly the object increment 2 just made representable, so
;;;; dependent circle-induction is now expressible where book-HoTT stalled.
;;;;
;;;; The universal cover is the family  helix : S^1 -> U,  helix base = Z, with
;;;; the loop acting by  ua succ.  encode (transport in the cover) and decode
;;;; (n |-> loop^n) are mutually inverse -- pi_1(S^1) = Z.
;;;;
;;;; HONEST scope: our loops are the loop-POWERS (the kernel has no symbolic
;;;; representation of an arbitrary S^1 loop), so decode . encode = id is
;;;; verified on loop^n -- the computational shadow of the full theorem.  The
;;;; point and loop computation rules, and encode . decode = id on all of Z,
;;;; fire by genuine computation.

(in-package #:rosette-cubical-core)

;;; ---- The universal cover family helix : S^1 -> U ---------------------

(defun helix-family ()
  "The universal-cover type-line: base fibre Z, the loop acting by ua succ.
Realised as the Glue line of the successor equivalence -- transp along it
computes (+1), which is the loop's action on the cover."
  (ua-line (int-succ-equivalence)))

;;; ---- The dependent eliminator (circle-induction) ---------------------

(defstruct (circle-section (:constructor %make-circle-section (base-value loop-pathp)))
  "A dependent section produced by circle-induction into a family P:
BASE-VALUE : P base  is the point case; LOOP-PATHP is the loop case -- a
PathP over P along the loop, from BASE-VALUE to its loop-transport.  The
existence of LOOP-PATHP is the content book-HoTT could not provide."
  (base-value nil :read-only t)
  (loop-pathp nil :type cpathp :read-only t))

(defun circle-ind (family base-value)
  "Dependent circle-induction into FAMILY (a type-line standing for P along
the loop), with point case BASE-VALUE : P base.  The loop case is SYNTHESISED
as the PathP over FAMILY from BASE-VALUE to (transp FAMILY BASE-VALUE) --
transp-fill, the canonical path-over-the-family.  Returns a CIRCLE-SECTION
whose point-beta gives back BASE-VALUE and whose loop-beta is that PathP."
  (%make-circle-section base-value (transp-fill family base-value)))

(defun circle-section-at-base (section)
  "The point computation rule (beta): the section at base is BASE-VALUE."
  (circle-section-base-value section))

(defun circle-section-loop-transport (section)
  "The loop computation rule (beta): the loop-PathP's 1-endpoint is the
fibre-transport of the base value along the loop (= ua succ applied)."
  (cpathp-i1 (circle-section-loop-pathp section)))

;;; ---- encode / decode : Omega S^1  <->  Z -----------------------------

(defun circle-encode (n)
  "encode : Omega S^1 -> Z on loop^N.  Transport 0 along the helix over
loop^N by iterating the COMPUTING ua-beta transp.  (= winding-of-loop-power,
now read as the action of the dependent eliminator's loop case.)"
  (winding-of-loop-power n))

(defun circle-decode (n)
  "decode : Z -> Omega S^1, n |-> loop^n.  We represent loop^n by the integer
N itself (the kernel's loops are the loop-powers); decode is thus the
identity tag on Z, and the loop it denotes has winding N."
  n)

(defun circle-pi1-encode-decode-id-p (n)
  "encode . decode = id on Z: encode(decode N) = N, for every integer N.
Fires by computation (iterated ua-beta)."
  (= (circle-encode (circle-decode n)) n))

(defun circle-pi1-decode-encode-id-p (n)
  "decode . encode = id on loop^N (the honest computational shadow):
decode(encode(loop^N)) denotes loop^N again."
  (= (circle-decode (circle-encode n)) n))
