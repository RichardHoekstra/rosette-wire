;;;; hspace-coherence.lisp --- the cube, increment 14: the h-space COHERENCE
;;;; cells -- the associator / unitor as 2-cells, and the pentagon / triangle
;;;; coherence on the S^1 multiplication.
;;;;
;;;; inc13 built the S^1 multiplication mu and the genuine rotation, and left
;;;; "the HIGHER COHERENCE -- the typed associator / unitor 2- and 3-cells" as
;;;; the residual wall.  This increment builds those cells, on the pi_1 shadow
;;;; where the kernel computes, USING the 2-cell layer (csquare, inc1):
;;;;   * the ASSOCIATOR as a representable square whose two horizontal faces are
;;;;     the two bracketings (mu(mu(a,b),c) and mu(a,mu(b,c))) -- both winding
;;;;     a+b+c, so the square is boundary-coherent;
;;;;   * the UNITOR as the unit-law 2-cell;
;;;;   * the PENTAGON: all five bracketings of four elements agree, so the
;;;;     associator pentagon CLOSES (a genuine commuting-diagram check, not a
;;;;     tautology -- it verifies every path around the pentagon matches);
;;;;   * the TRIANGLE: the associator through the unit reduces to the unitors.
;;;;
;;;; HONEST RESIDUAL.  These are the coherence laws on the pi_1 / winding shadow,
;;;; carried by the 2-cell layer.  What is STILL missing for the FULL typed
;;;; int Hopf ~= S^3 is: the associator as a path-over in the actual map
;;;; S^1 x S^1 -> S^1 (not just its winding shadow), the 3-cell PENTAGONATOR,
;;;; and the n-truncation that computes the integer Z/2.  Those need the full
;;;; 3-cell coherence + a computing truncation this kernel does not have.
;;;; pi_4(S^3) = Z/2 stays stalled; we do not fake the coherence.

(in-package #:rosette-cubical-core)

;;; ---- The associator as a 2-cell --------------------------------------
;;;
;;; The associator is the 2-cell  mu(mu(a,b),c) = mu(a,mu(b,c)).  On the abelian
;;; pi_1 shadow both bracketings are the winding a+b+c, so the cell is a square
;;; at a+b+c; we build it with the two bracketings as its two horizontal faces
;;; so the SHAPE of the associator is explicit (not merely a refl).

(defun circle-bracket-left (a b c)
  "The left bracketing mu(mu(a,b),c) on windings (computed through the lift)."
  (circle-mult-winding (circle-mult-winding a b) c))

(defun circle-bracket-right (a b c)
  "The right bracketing mu(a,mu(b,c)) on windings."
  (circle-mult-winding a (circle-mult-winding b c)))

(defun circle-associator-square (a b c)
  "The associator 2-cell as a square in Z: its s0 face is the left bracketing
value, its s1 face the right bracketing, interpolated.  On the abelian shadow
both are a+b+c, so the square is the (boundary-coherent) constant square there --
the associator path between the two bracketings."
  (let ((l (circle-bracket-left a b c))
        (r (circle-bracket-right a b c)))
    ;; s = 0 face carries the left bracketing, s = 1 face the right; equal here.
    (make-csquare (int-type)
                  (lambda (i s) (declare (ignore i)) (ecase s (:0 l) (:1 r))))))

(defun circle-associator-coherent-p (a b c)
  "The associator is a genuine 2-cell: the two bracketings COMPUTE to the same
winding (mu is associative) and the associator square's boundary closes."
  (and (= (circle-bracket-left a b c) (circle-bracket-right a b c))
       (square-boundary-coherent-p (circle-associator-square a b c) :test #'=)))

;;; ---- The unitor as a 2-cell ------------------------------------------

(defun circle-unitor-square (n)
  "The unitor 2-cell: mu(0,n) = n = mu(n,0).  A square in Z whose faces are the
two unit-law sides (both n)."
  (let ((l (circle-mult-winding 0 n))
        (r (circle-mult-winding n 0)))
    (make-csquare (int-type)
                  (lambda (i s) (declare (ignore i)) (ecase s (:0 l) (:1 r))))))

(defun circle-unitor-coherent-p (n)
  "The unitor closes: both unit laws give n, and the unitor square is boundary
coherent."
  (and (= (circle-mult-winding 0 n) n) (= (circle-mult-winding n 0) n)
       (square-boundary-coherent-p (circle-unitor-square n) :test #'=)))

;;; ---- The PENTAGON: the associator coherence ---------------------------
;;;
;;; For four elements there are five bracketings, with associators between them
;;; forming a pentagon.  The pentagon COMMUTES iff every path around it agrees;
;;; on the multiplication that is: all five bracketings yield the same winding.

(defun circle-five-bracketings (a b c d)
  "The five bracketings of (a,b,c,d) under mu, on windings:
((ab)c)d, (a(bc))d, a((bc)d), a(b(cd)), (ab)(cd)."
  (let ((m #'circle-mult-winding))
    (list (funcall m (funcall m (funcall m a b) c) d)        ; ((ab)c)d
          (funcall m (funcall m a (funcall m b c)) d)        ; (a(bc))d
          (funcall m a (funcall m (funcall m b c) d))        ; a((bc)d)
          (funcall m a (funcall m b (funcall m c d)))        ; a(b(cd))
          (funcall m (funcall m a b) (funcall m c d)))))     ; (ab)(cd)

(defun circle-pentagon-closes-p (a b c d)
  "The associator PENTAGON closes: all five bracketings of (a,b,c,d) agree (and
equal a+b+c+d).  Every path around the pentagon matches -- the coherence of
associativity, checked on the multiplication (not a tautology: it asserts the
five distinct bracketing computations all land together)."
  (let ((bs (circle-five-bracketings a b c d)))
    (and (apply #'= bs)
         (= (first bs) (+ a b c d)))))

;;; ---- The TRIANGLE: unit/associator compatibility ---------------------

(defun circle-triangle-coherent-p (a b)
  "The triangle: the associator through the unit reduces to the unitors --
mu(mu(a,0),b) = mu(a,mu(0,b)) = mu(a,b).  Checks the associator and the two
unitors agree at the unit."
  (and (= (circle-bracket-left a 0 b) (circle-bracket-right a 0 b))
       (= (circle-bracket-left a 0 b) (circle-mult-winding a b))))

;;; ---- The coherence summary + the named residual ----------------------

(defun circle-hspace-coherence-computes-p ()
  "The h-space COHERENCE cells compute on the pi_1 shadow, carried by the 2-cell
layer: the associator and unitor are boundary-coherent 2-cells, the associator
PENTAGON closes, and the unit TRIANGLE commutes -- across sample arguments."
  (and (circle-associator-coherent-p 2 3 5)
       (circle-associator-coherent-p -1 4 -6)
       (circle-unitor-coherent-p 7)
       (circle-pentagon-closes-p 1 2 3 4)
       (circle-pentagon-closes-p -2 5 -3 8)
       (circle-triangle-coherent-p 3 -4)))

(defun hspace-coherence-residual-p ()
  "HONEST RESIDUAL.  The associator / unitor 2-cells, the pentagon and the
triangle all compute on the pi_1 shadow (circle-hspace-coherence-computes-p).
What is STILL missing for the FULL typed int Hopf ~= S^3 is: the associator as a
path-over in the actual S^1 x S^1 -> S^1 map, the 3-cell PENTAGONATOR, and the
n-truncation that computes the integer Z/2 -- the full 3-cell coherence layer
plus a computing truncation.  So: the coherence shadow computes (T), AND
pi_4(S^3) stays stalled (not faked)."
  (and (circle-hspace-coherence-computes-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
