;;;; pi4-equiv.lisp --- the cube, increment 23: the encode/decode EQUIVALENCE at
;;;; pi_4, and the honest CEILING of this kernel's compute.
;;;;
;;;; inc22 built the encode MAP pi_4(S^3) -> Z/2 = (Hopf invariant) mod 2 with
;;;; computing values.  This increment completes it into an encode/decode
;;;; EQUIVALENCE -- the form in which pi_1(S^1)=Z and the Moore pi_1=Z/2 were
;;;; stated -- and then draws the genuine ceiling.
;;;;
;;;; decode : Z/2 -> pi_4(S^3),  j |-> j . eta_3.  Then:
;;;;   * encode . decode = id on Z/2 (computes: encode(j.eta_3) = j mod 2 = j);
;;;;   * decode . encode = id on pi_4(S^3) GIVEN the order-2 relation 2 eta_3 = 0
;;;;     (so n.eta_3 = (n mod 2).eta_3): the round-trip at the presentation level.
;;;; Together: pi_4(S^3) = Z/2, in encode/decode form.
;;;;
;;;; THE HONEST CEILING -- the one real distinction from pi_1.  For pi_1(S^1)=Z the
;;;; encode is a TRANSPORT on representable loops (the loop-powers, winding via
;;;; ua-beta).  At pi_4 there are NO representable 4-loops in this kernel: the
;;;; encode/decode here operates on the GENERATOR INDEX (multiples of eta_3),
;;;; computing the GROUP LAW (n mod 2), not transporting actual elements of
;;;; Omega^4 S^3.  So this computes pi_4(S^3)=Z/2 at the PRESENTATION level
;;;; (generator eta_3, relation 2 eta_3 = 0, encode = Hopf-inv mod 2) -- with every
;;;; INPUT now either computed (Hopf invariant via the bidegree inc18, mod-2 via
;;;; the cyclic engine inc21, the 2 via the cup square inc15) or a NAMED encoded
;;;; classical theorem (Freudenthal surjectivity inc10, EHP order inc16).
;;;;
;;;; What remains GENUINELY beyond this kernel: the loop-transport normalisation of
;;;; || Omega^4 S^3 ||_0 -- reducing the truncation AS A TYPE on representable
;;;; 4-loops -- which is the actual cubical-normaliser achievement (a full type
;;;; checker + its normalisation engine, not a hand-rolled kernel).  pi-n
;;;; (:sphere 3) 4 stays :stalled.  We name the ceiling; we do not fake the step.

(in-package #:rosette-cubical-core)

;;; ---- decode : Z/2 -> pi_4(S^3) ---------------------------------------

(defun pi4-s3-decode (j)
  "decode : Z/2 -> pi_4(S^3), j |-> j . eta_3 (the generator eta_3 = Sigma^2 eta);
represented by its class j mod 2."
  (mod j 2))

;;; ---- the two round-trips ---------------------------------------------

(defun pi4-s3-encode-decode-id-p ()
  "encode . decode = id on Z/2: encode(decode j) = j for j in {0,1}.  Computes
(encode is Hopf-inv mod 2; decode j is j.eta_3 with Hopf invariant j)."
  (and (= (pi4-s3-encode (pi4-s3-decode 0)) 0)
       (= (pi4-s3-encode (pi4-s3-decode 1)) 1)))

(defun pi4-s3-order-2-relation-holds-p ()
  "The defining relation 2 eta_3 = 0 holds in the encode: encode(2.eta_3) = 0 (and
encode(eta_3)=1 =/= 0), so eta_3 has order exactly 2.  Computed via Hopf-inv mod 2."
  (and (= (pi4-s3-encode 2) 0) (= (pi4-s3-encode 1) 1)))

(defun pi4-s3-decode-encode-id-p (&optional (range 8))
  "decode . encode = id on pi_4(S^3), at the presentation level: GIVEN the order-2
relation 2 eta_3 = 0, n.eta_3 = (n mod 2).eta_3, so decode(encode(n.eta_3)) = n.eta_3
as group elements.  We check (n mod 2) = decode(encode n) across a range -- the
round-trip on the generator index."
  (and (pi4-s3-order-2-relation-holds-p)
       (loop for n from (- range) to range
             always (= (pi4-s3-decode (pi4-s3-encode n)) (mod n 2)))))

(defun pi4-s3-is-Z2-encode-decode-p ()
  "pi_4(S^3) = Z/2, in encode/decode form: encode . decode = id on Z/2 (computes),
decode . encode = id given the order-2 relation, and eta_3 has order 2.  The same
shape as pi_1(S^1)=Z and the Moore pi_1=Z/2 -- climbed to pi_4."
  (and (pi4-s3-encode-decode-id-p)
       (pi4-s3-decode-encode-id-p)
       (pi4-s3-order-2-relation-holds-p)))

;;; ---- the honest ceiling ----------------------------------------------

(defun pi4-s3-computed-at-presentation-level-p ()
  "The kernel computes pi_4(S^3) = Z/2 at the PRESENTATION level: generator eta_3,
relation 2 eta_3 = 0, encode = Hopf-invariant mod 2 -- the encode/decode
equivalence holds and every INPUT is either computed (Hopf inv via the bidegree,
mod-2 via the cyclic engine, the 2 via the cup square) or a named encoded theorem
(Freudenthal surjectivity, EHP order)."
  (and (pi4-s3-is-Z2-encode-decode-p)
       (= (pi4-s3-suspension-kernel-generator) 2)   ; the 2 (cup square, inc15)
       (cubical-hopf-map-invariant-is-1-p)            ; H(eta)=1 (bidegree, inc18)
       (cyclic-pi1-is-Zk-p 2)))                       ; mod-2 engine (inc21)

(defun pi4-s3-loop-transport-normalisation-is-the-ceiling-p ()
  "THE CEILING, named honestly.  pi_4(S^3)=Z/2 is computed at the presentation
level (above).  What is GENUINELY beyond this kernel is the LOOP-TRANSPORT
normalisation: reducing || Omega^4 S^3 ||_0 AS A TYPE on representable 4-loops
(there are none here -- the inc12-14/inc20 coherence wall, and ultimately a full
cubical type checker + normaliser, which is the actual cubical-normaliser
achievement).  So: pi_4(S^3)=Z/2 computes at the presentation level (T), AND the
loop-transport normalisation stays out of reach -- pi-n (:sphere 3) 4 :stalled."
  (and (pi4-s3-computed-at-presentation-level-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
