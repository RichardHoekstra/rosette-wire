;;;; rosette-proof-witness/src/package.lisp --- Public API.

(defpackage #:rosette-proof-witness
  (:use #:cl)
  (:export
   #:lean-witness
   #:lean-witness-p
   #:lean-witness-theorem-name
   #:lean-witness-lean-path
   #:lean-witness-test-fn
   #:make-lean-witness
   #:certificate
   #:certificate-p
   #:make-certificate
   #:certificate-name
   #:certificate-kind
   #:certificate-claim
   #:certificate-payload
   #:certificate-passed
   #:certificate-witness
   #:certificate-metadata
   #:certificate->plist
   #:delta-report
   #:delta-report-p
   #:make-delta-report
   #:delta-report-name
   #:delta-report-baseline
   #:delta-report-candidate
   #:delta-report-runtime-ns
   #:delta-report-speedup
   #:delta-report-register-count
   #:delta-report-occupancy
   #:delta-report-memory-traffic
   #:delta-report-numerics
   #:delta-report-metadata
   #:delta-report-complete-p
   #:delta-report->plist
   #:publish-certificate
   #:publish-delta-report
   #:runtime-verify
   #:relative-error-pct
   #:*lean-witness-registry*
   #:register-lean-witness
   #:find-lean-witness))
