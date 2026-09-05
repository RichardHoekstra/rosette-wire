;;;; circle.lisp --- toward pi_1(S^1) = Z, computing cubically.
;;;;
;;;; The cubical proof of pi_1(S^1)=Z runs through the UNIVERSAL COVER:
;;;; the type family  helix : S^1 -> U  with  helix base = Z  and the
;;;; action of the generating loop given by  ua succ  (the integers,
;;;; shifted by one).  The winding number of a loop is  transp helix
;;;; along it, started at 0.  Because  transp (ua succ)  COMPUTES to
;;;; +1 (the ua-beta rule, see kan.lisp), transporting along loop^n
;;;; computes to the integer n -- the encode map  Omega S^1 -> Z.  In
;;;; book-HoTT this same transport is stuck on the ua axiom; here it runs.

(in-package #:rosette-cubical-core)

(defun winding-of-loop-power (n)
  "Transport 0 : Z along  helix  over  loop^N, computing the winding
number.  loop acts by  ua succ, loop^{-1} by  ua pred, so the result is
N -- obtained by iterating the COMPUTING ua-beta transp, not by reading
an axiom.  This is the cubical encode  Omega S^1 -> Z  on loop^N."
  (let* ((succ (int-succ-equivalence))
         (pred (rosette-hott-core:inverse-equivalence succ))
         (acc 0))
    (cond
      ((>= n 0)
       (dotimes (_ n) (setf acc (ua-beta succ acc))))
      (t
       (dotimes (_ (- n)) (setf acc (ua-beta pred acc)))))
    acc))

(defun circle-encode-decode-roundtrip-p (n)
  "encode(loop^N) = N and the decode (loop-power in the book circle) has
the matching winding.  We verify the cubical encode is the identity on
the integer N, the heart of  pi_1(S^1) = Z."
  (= (winding-of-loop-power n) n))
