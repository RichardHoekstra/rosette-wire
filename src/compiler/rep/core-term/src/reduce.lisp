;;;; reduce.lisp --- the reusable "reduction trace IS the certificate" seam.
;;;;
;;;; rosette-core-term's coincidence 2 is a BIG-STEP reducer that emits one proof
;;;; tree.  The SAME mechanism -- "run the reduction, collect the per-step
;;;; proof-carrier-core nodes, seal them into a re-runnable proof-witness
;;;; certificate so the trace re-checks and tampering is caught" -- is useful
;;;; to ANY rewrite/reduction engine (e.g. rosette-egraph-extract), which can then
;;;; mint proofs BY REDUCTION instead of attaching them after the fact.
;;;;
;;;; This file factors that mechanism into ONE public function CERTIFIED-REDUCE
;;;; parameterised over a caller-supplied SMALL-STEP relation.  The caller
;;;; keeps its own term representation; it only has to hand us a STEP function
;;;;
;;;;     STEP : state -> (values next-state proof-node)          [a redex fired]
;;;;     STEP : state -> (values state      NIL)                 [normal form]
;;;;
;;;; and we drive it to normal form, thread the emitted nodes into a trace, and
;;;; seal the whole reduction into a certificate whose witness INDEPENDENTLY
;;;; re-drives STEP from SEED and requires the recomputed normal form, step
;;;; count, and trace fingerprint to match the sealed ones.  A forged normal
;;;; form, a forged step count, or a doctored trace fingerprint all fail
;;;; RUNTIME-VERIFY.

(in-package #:rosette-core-term)

;;; --- trace fingerprint: a serialisable digest of the proof-node sequence ----
;;; We cannot serialise a caller's step closure, so the certificate carries a
;;; structural DIGEST of the emitted trace (kind/label/size per node) alongside
;;; the sealed normal form.  Re-driving STEP must reproduce the same digest, so
;;; a tampered digest -- or a tampered normal form / step count -- is caught.

(defun %node-signature (node)
  "A stable, comparable signature of one proof node (structure, not identity)."
  (list :kind (proof-node-kind node)
        :label (let ((l (proof-node-label node)))
                 (if (symbolp l) l (princ-to-string l)))
        :size (proof-node-size node)
        :depth (proof-node-depth node)))

(defun reduction-trace-fingerprint (trace)
  "Digest a TRACE (a list of rosette-proof-carrier-core proof-nodes) into a stable
value comparable with EQUAL.  Two reductions with the same step sequence share
a fingerprint; any divergence (extra/missing/altered step) changes it."
  (list :n (length trace)
        :nodes (mapcar #'%node-signature trace)))

;;; --- the driver ------------------------------------------------------------

(defun %drive-reduction (step seed max-steps test)
  "Iterate STEP from SEED to a normal form.  Returns (values NORMAL-FORM TRACE
STEPS).  STEP returns (values next node); NODE = NIL (or NEXT test-equal to the
current state) marks a normal form.  Signals on exceeding MAX-STEPS (a
non-terminating or mis-specified relation is a loud error, not a silent hang)."
  (let ((state seed) (trace '()) (steps 0))
    (loop
      (multiple-value-bind (next node) (funcall step state)
        (when (or (null node) (funcall test next state))
          (return (values state (nreverse trace) steps)))
        (unless (typep node 'proof-node)
          (error "certified-reduce: STEP returned a non proof-node ~S" node))
        (push node trace)
        (setf state next)
        (incf steps)
        (when (> steps max-steps)
          (error "certified-reduce: exceeded MAX-STEPS ~D (non-terminating relation?)"
                 max-steps))))))

(defun certified-reduce (step seed
                         &key (name :certified-reduction) (claim :normal-form)
                              (max-steps 1000000) (test #'equal))
  "Drive the caller's SMALL-STEP relation STEP from SEED to a normal form,
collecting the per-step proof-carrier-core nodes into a trace, and SEAL the
reduction into a re-runnable rosette-proof-witness CERTIFICATE.  THE REDUCTION
TRACE IS THE CERTIFICATE.

STEP is  state -> (values next-state proof-node) , returning (values state NIL)
at a normal form.  TEST compares states (default #'EQUAL, right for symbolic
terms; pass #'EQL for integers).

Returns (values NORMAL-FORM CERTIFICATE TRACE).  The certificate PAYLOAD seals
the SEED, the normal form, the step count, and the trace fingerprint; its
WITNESS re-drives STEP from SEED and requires all three to match -- so a forged
normal form, a forged step count, or a doctored fingerprint fail RUNTIME-VERIFY."
  (check-type step function)
  (multiple-value-bind (nf trace steps) (%drive-reduction step seed max-steps test)
    (let* ((fp (reduction-trace-fingerprint trace))
           (payload (list :seed seed :normal-form nf :steps steps
                          :fingerprint fp
                          :trace (mapcar #'proof-node->plist trace)))
           (cert (make-certificate :name name :kind :proof :claim claim
                                   :payload payload :passed t)))
      (setf (certificate-witness cert)
            (make-lean-witness
             :certified-reduction-re-runs
             (lambda () (%recheck-reduction cert step test max-steps))))
      (values nf cert trace))))

(defun %recheck-reduction (cert step test max-steps)
  "Independently re-drive STEP from the SEED sealed in CERT and require the
recomputed normal form, step count, and trace fingerprint to equal the sealed
ones.  Returns T iff the certificate is honest."
  (let ((pl (certificate-payload cert)))
    (handler-case
        (multiple-value-bind (nf trace steps)
            (%drive-reduction step (getf pl :seed) max-steps test)
          (and (funcall test nf (getf pl :normal-form))
               (eql steps (getf pl :steps))
               (equal (reduction-trace-fingerprint trace) (getf pl :fingerprint))))
      (error () nil))))

(defun verify-reduction-certificate (cert)
  "Re-run CERT through rosette-proof-witness RUNTIME-VERIFY (which fires the
installed witness).  A forged/tampered reduction certificate returns NIL."
  (runtime-verify cert))

(defun reduction-normal-form (cert)
  "The normal form sealed in a CERTIFIED-REDUCE certificate."
  (getf (certificate-payload cert) :normal-form))
