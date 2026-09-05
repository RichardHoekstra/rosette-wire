;;;; rosette-proof-witness/src/publish.lisp --- Publication certificates.

(in-package #:rosette-proof-witness)

(defun publish-certificate (certificate &key (name :publish-certificate) metadata)
  "Wrap CERTIFICATE in a publication certificate."
  (check-type certificate certificate)
  (make-certificate
   :name name
   :kind :publish
   :claim :certificate-published
   :payload (certificate->plist certificate)
   :passed (certificate-passed certificate)
   :witness certificate
   :metadata metadata))

(defun publish-delta-report (report &key (name :publish-delta-report) metadata accept)
  "Wrap REPORT in a publication certificate.

PASSED is DERIVED, never hardcoded, and the CLAIM tracks exactly what was
checked:

  ACCEPT absent  -> claim :delta-report-published, passed = report complete.
  ACCEPT present -> claim :delta-report-accepted,  passed = complete AND
                    (funcall ACCEPT report).

ACCEPT is how a caller supplies the acceptance judgement -- a speedup floor, a
numerics tolerance -- that this library deliberately does not own, because no
threshold for it exists in the report.  A certificate therefore never asserts
that a candidate is *good* unless someone supplied the predicate that decides
it.  WITNESS is the report, so `runtime-verify' re-derives completeness
instead of short-circuiting on a null witness."
  (check-type report delta-report)
  (let ((complete (delta-report-complete-p report)))
    (make-certificate
     :name name
     :kind :publish
     :claim (if accept :delta-report-accepted :delta-report-published)
     :payload (delta-report->plist report)
     :passed (and complete
                  (or (null accept)
                      (and (funcall accept report) t)))
     :witness report
     :metadata metadata)))
