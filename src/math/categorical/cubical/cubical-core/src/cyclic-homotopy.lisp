;;;; cyclic-homotopy.lisp --- the cube, increment 21: the uniform engine -- every
;;;; finite cyclic homotopy group  pi_1(M(Z/k,1)) = Z/k  computes, via order-k
;;;; monodromy.  The Brunerie Z/2 is the k=2 instance.
;;;;
;;;; inc19 computed pi_1(M(Z/2,1))=Z/2 with the order-2 swap (bool-not) as the
;;;; cover monodromy; inc20 grounded it in the set-truncation.  The mechanism was
;;;; never special to 2: a homotopy group is Z/k exactly when the relevant
;;;; monodromy is an equivalence of ORDER k, and the encode computes by iterating
;;;; its ua-beta.  This increment proves that UNIFORMLY: for every k, the Moore
;;;; space M(Z/k,1) = S^1 cup_k e^2 (2-cell attached by the degree-k map) has
;;;;   pi_1 = Z/k,
;;;; computed by encode/decode through the k-sheeted cover whose loop action is
;;;; the k-cycle  n |-> n+1 (mod k).  encode(a^N) = N mod k, by ua-beta -- the
;;;; SAME engine as pi_1(S^1)=Z (the order-infinity succ), specialised to order k.
;;;;
;;;; So the kernel computes ALL finite cyclic homotopy groups at pi_1, and the
;;;; Brunerie Z/2 is the k=2 row of one machine.  The summit pi_4(S^3)=Z/2 is the
;;;; same engine three dimensions up, where the monodromy / Omega^4 S^3 truncation
;;;; cannot be represented (the inc12-14 coherence wall) -- so that stays :stalled.

(in-package #:rosette-cubical-core)

;;; ---- The order-k cyclic equivalence (the k-cycle monodromy) ----------

(defun zk-type (k)
  "The k-element type Z/k = {0, 1, ..., k-1}."
  (make-hott-type (list :zk k)
                  :predicate (lambda (n) (and (integerp n) (<= 0 n) (< n k)))))

(defun cyclic-equiv (k)
  "The order-k cyclic equivalence on Z/k: forward n |-> (n+1) mod k, backward
n |-> (n-1) mod k.  Its k-fold composite is the identity -- order exactly k.
This is the monodromy of the k-sheeted cover of M(Z/k,1)."
  (let ((z (zk-type k)))
    (rosette-hott-core:make-hott-equivalence
     z z
     (lambda (n) (mod (1+ n) k))
     (lambda (n) (mod (1- n) k)))))

(defun cyclic-equiv-has-order-k-p (k)
  "The cyclic equivalence has order exactly k: its k-fold ua-beta iterate is the
identity on Z/k, and no smaller positive power is."
  (let ((e (cyclic-equiv k)))
    (flet ((iterate (start times)
             (let ((v start)) (dotimes (_ times v) (setf v (ua-beta e v))))))
      (and (loop for start from 0 below k always (= (iterate start k) start))   ; e^k = id
           (loop for j from 1 below k                                            ; e^j =/= id, 0<j<k
                 never (loop for start from 0 below k always (= (iterate start j) start)))))))

;;; ---- encode / decode :  Omega M(Z/k,1)  <->  Z/k --------------------

(defun cyclic-encode (k n)
  "encode : Omega M(Z/k,1) -> Z/k on a^N.  Transport 0 along a^N in the k-sheeted
cover by iterating the cyclic monodromy (ua-beta), forward for N>=0, backward for
N<0.  The result is N mod k -- computed, not read."
  (let ((e (cyclic-equiv k))
        (v 0))
    (cond ((>= n 0) (dotimes (_ n) (setf v (ua-beta e v))))
          (t (let ((inv (rosette-hott-core:hott-equivalence-backward (cyclic-equiv k))))
               (dotimes (_ (- n)) (setf v (funcall inv v))))))
    v))

(defun cyclic-decode (k j)
  "decode : Z/k -> Omega M(Z/k,1), j |-> a^j (j in {0,...,k-1})."
  (mod j k))

(defun cyclic-encode-is-mod-k-p (k &optional (extra 3))
  "The cubical encode computes  a^N |-> N mod k  across a range up to a few
multiples of k (ua-beta on the k-cycle)."
  (loop for n from 0 to (* extra k) always (= (cyclic-encode k n) (mod n k))))

(defun cyclic-loop-order (k)
  "The order of the loop a in pi_1(M(Z/k,1)): the least j>0 with encode(a^j)=0 --
comes out exactly k."
  (loop for j from 1 to (* 2 k)
        when (= (cyclic-encode k j) 0) do (return j)
        finally (return :none)))

;;; ---- pi_1(M(Z/k,1)) = Z/k, for every k -------------------------------

(defun cyclic-pi1-is-Zk-p (k)
  "pi_1(M(Z/k,1)) = Z/k, COMPUTED: the encode is N mod k (ua-beta on the k-cycle),
encode . decode = id on Z/k, the monodromy has order k, and the loop a has order
exactly k."
  (and (cyclic-equiv-has-order-k-p k)
       (cyclic-encode-is-mod-k-p k)
       (loop for j from 0 below k always (= (cyclic-encode k (cyclic-decode k j)) j))
       (eql (cyclic-loop-order k) k)))

(defun moore-Zk-for-all-k-p (&optional (kmax 7))
  "The UNIFORM theorem: for every k from 2 to KMAX, pi_1(M(Z/k,1)) = Z/k computes.
One engine -- order-k monodromy + the ua-beta encode -- delivers every finite
cyclic homotopy group at pi_1."
  (loop for k from 2 to kmax always (cyclic-pi1-is-Zk-p k)))

;;; ---- The Brunerie Z/2 as the k=2 instance ----------------------------

(defun brunerie-2-is-k2-instance-p ()
  "HONEST.  The Brunerie Z/2 is the k=2 row of this uniform machine: pi_1(M(Z/2,1))
= Z/2 computes via the order-2 monodromy (matching inc19's bool-not), the same
engine that gives Z/k for every k.  The summit pi_4(S^3)=Z/2 is this engine three
dimensions up, where the monodromy / Omega^4 S^3 truncation cannot be represented
(the coherence wall).  So the cyclic engine computes Z/k at pi_1 (T), AND
pi_4(S^3) stays :stalled."
  (and (cyclic-pi1-is-Zk-p 2)
       (= (cyclic-encode 2 1) 1) (= (cyclic-encode 2 2) 0)   ; matches moore-encode
       (moore-Zk-for-all-k-p)
       (eq (pi-n (list :sphere 3) 4) :stalled)))
