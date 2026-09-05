;;;; parity.lisp --- Stage 5 (CERTIFY): the gauge-check.
;;;;
;;;; A lowering is only trustworthy if the deliverable computes what the
;;;; in-tree witness computes. The parity certificate is that gauge-check: it
;;;; compares the captured output of the in-tree run against the shipped
;;;; artifact's run and PASSES iff they are byte-identical. It rides on the
;;;; substrate's certificate carrier (rosette-proof-witness) so a shipment's proof
;;;; lives in the same falsifiable-witness registry as every other claim.
;;;;
;;;; The library only ADJUDICATES two captured outputs (pure); the CLI runs
;;;; the two processes and hands their stdout here.

(in-package #:rosette-ship)

(defun make-parity-certificate (in-tree-output floor-output
                                &key (floor :bin) (name :ship-parity) metadata)
  "Certificate that the FLOOR artifact reproduces the in-tree run. PASSED iff
both outputs are present and STRING= equal. Returns an rosette-proof-witness
certificate (kind :runtime, claim :floor-parity)."
  (let ((passed (and in-tree-output floor-output
                     (string= in-tree-output floor-output)
                     t)))
    (rosette-proof-witness:make-certificate
     :name name
     :kind :runtime
     :claim :floor-parity
     :passed passed
     :payload (list :floor floor
                    :in-tree-length (length (or in-tree-output ""))
                    :floor-length (length (or floor-output ""))
                    :byte-identical passed)
     :metadata metadata)))

(defun parity-passed-p (certificate)
  "True when a parity CERTIFICATE confirms byte-identical floor parity."
  (rosette-proof-witness:certificate-passed certificate))
