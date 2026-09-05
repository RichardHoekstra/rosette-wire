;;;; rosette-proof-witness/src/certificate.lisp --- Unified certificates.

(in-package #:rosette-proof-witness)

(defstruct (certificate
            (:constructor %make-certificate
                (&key name kind claim payload passed witness metadata)))
  "Small shared certificate record for runtime, proof, benchmark, and optimizer claims."
  (name :certificate :type keyword)
  (kind :runtime :type keyword)
  (claim :claim :type keyword)
  payload
  (passed nil :type boolean)
  witness
  (metadata nil :type list))

(defun make-certificate (&key (name :certificate) (kind :runtime)
                              (claim :claim) payload passed witness metadata)
  "Construct a certificate without retaining caller-owned metadata."
  (%make-certificate
   :name name
   :kind kind
   :claim claim
   :payload payload
   :passed passed
   :witness witness
   :metadata (rosette-metadata-core:copy-plist
              metadata
              :label "Certificate METADATA")))

(defun certificate->plist (certificate)
  "Return CERTIFICATE as a stable machine-readable plist."
  (check-type certificate certificate)
  (list :name (certificate-name certificate)
        :kind (certificate-kind certificate)
        :claim (certificate-claim certificate)
        :payload (certificate-payload certificate)
        :passed (certificate-passed certificate)
        :witness-present (not (null (certificate-witness certificate)))
        :metadata (copy-list (certificate-metadata certificate))))
