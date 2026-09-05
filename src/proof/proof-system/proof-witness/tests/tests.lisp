;;;; rosette-proof-witness/tests/tests.lisp --- Test suite.

(defpackage #:rosette-proof-witness/tests
  (:use #:cl #:rosette-proof-witness #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-proof-witness/tests)

(defun signals-error-p (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error () t)))

(defun run-all-tests ()
  "Run rosette-proof-witness tests."
  (with-test-run (run "rosette-proof-witness")
    (clrhash *lean-witness-registry*)
    (let* ((true-witness (make-lean-witness 'trivially-true
                                            (lambda () t)
                                            :lean-path "Proofs/True.lean"))
           (false-witness (make-lean-witness 'trivially-false
                                             (lambda () nil)))
           (error-witness (make-lean-witness 'raises
                                             (lambda () (error "boom"))))
           (certificate (make-certificate
                         :name :runtime-smoke
                         :kind :runtime
                         :claim :true-witness
                         :payload '(:value 1)
                         :passed t
                         :witness (make-lean-witness 'certificate-true
                                                     (lambda () t))
                         :metadata '(:phase :unit)))
           (delta-report (make-delta-report
                          :name :adapter-smoke
                          :baseline :refinterp
                          :candidate :cpu-adapter
                          :runtime-ns 1000
                          :speedup 2.0d0
                          :register-count 12
                          :occupancy 0.5d0
                          :memory-traffic '(:bytes 4096)
                          :numerics '(:max-abs-error 0d0)
                          :metadata '(:adapter :cpu))))

      ;; --- Lean witness construction, accessors, registry, verification ---
      (check run (lean-witness-p true-witness)
             "make-lean-witness returns a lean-witness")
      (check run (eq (lean-witness-theorem-name true-witness) 'trivially-true)
             "lean-witness-theorem-name returns theorem symbol")
      (check run (string= (lean-witness-lean-path true-witness) "Proofs/True.lean")
             "lean-witness-lean-path returns optional Lean path")
      (check run (functionp (lean-witness-test-fn true-witness))
             "lean-witness-test-fn returns executable thunk")
      (check run (runtime-verify true-witness)
             "true witness verifies")
      (check run (not (runtime-verify false-witness))
             "false witness fails")
      (check run (not (runtime-verify error-witness))
             "error witness fails without aborting verification")
      (check run (eq true-witness (register-lean-witness true-witness))
             "register-lean-witness returns registered witness")
      (check run (eq true-witness (find-lean-witness 'trivially-true))
             "find-lean-witness returns registered witness")
      (check run (null (find-lean-witness 'missing-theorem))
             "find-lean-witness returns NIL for unknown theorem")
      (check run (signals-error-p
                  (lambda () (make-lean-witness "bad" (lambda () t))))
             "make-lean-witness requires theorem-name symbol")
      (check run (signals-error-p
                  (lambda () (make-lean-witness 'bad-test :not-function)))
             "make-lean-witness requires function test")
      (check run (signals-error-p
                  (lambda () (register-lean-witness :not-witness)))
             "register-lean-witness rejects non-witnesses")

      ;; --- Certificate construction, accessors, plist, runtime verify -----
      (check run (certificate-p certificate)
             "make-certificate returns a certificate")
      (check run (eq (certificate-name certificate) :runtime-smoke)
             "certificate-name returns supplied name")
      (check run (eq (certificate-kind certificate) :runtime)
             "certificate-kind returns supplied kind")
      (check run (eq (certificate-claim certificate) :true-witness)
             "certificate-claim returns supplied claim")
      (check run (equal (certificate-payload certificate) '(:value 1))
             "certificate-payload returns supplied payload")
      (check run (certificate-passed certificate)
             "certificate-passed returns supplied verdict")
      (check run (lean-witness-p (certificate-witness certificate))
             "certificate-witness carries nested witness")
      (check run (equal (certificate-metadata certificate) '(:phase :unit))
             "certificate-metadata returns supplied metadata")
      (check run (runtime-verify certificate)
             "passed certificate with verifying witness verifies")
      (check run (not (runtime-verify
                       (make-certificate :passed nil)))
             "failed certificate does not verify")
      (check run (not (runtime-verify
                       (make-certificate :passed t :witness false-witness)))
             "certificate fails when nested witness fails")
      (check run (runtime-verify
                  (make-certificate :passed t :witness nil))
             "passed certificate without nested witness verifies")
      (check run (equal (certificate->plist certificate)
                        '(:name :runtime-smoke
                          :kind :runtime
                          :claim :true-witness
                          :payload (:value 1)
                          :passed t
                          :witness-present t
                          :metadata (:phase :unit)))
             "certificate->plist returns stable machine-readable shape")

      ;; --- Delta reports and relative error -------------------------------
      (check run (delta-report-p delta-report)
             "make-delta-report returns a delta-report")
      (check run (eq (delta-report-name delta-report) :adapter-smoke)
             "delta-report-name returns supplied name")
      (check run (eq (delta-report-baseline delta-report) :refinterp)
             "delta-report-baseline returns supplied baseline")
      (check run (eq (delta-report-candidate delta-report) :cpu-adapter)
             "delta-report-candidate returns supplied candidate")
      (check run (= (delta-report-runtime-ns delta-report) 1000)
             "delta-report-runtime-ns returns supplied runtime")
      (check run (= (delta-report-speedup delta-report) 2.0d0)
             "delta-report-speedup returns supplied speedup")
      (check run (= (delta-report-register-count delta-report) 12)
             "delta-report-register-count returns supplied register count")
      (check run (= (delta-report-occupancy delta-report) 0.5d0)
             "delta-report-occupancy returns supplied occupancy")
      (check run (equal (delta-report-memory-traffic delta-report) '(:bytes 4096))
             "delta-report-memory-traffic returns supplied traffic")
      (check run (equal (delta-report-numerics delta-report)
                        '(:max-abs-error 0d0))
             "delta-report-numerics returns supplied numeric residuals")
      (check run (equal (delta-report-metadata delta-report) '(:adapter :cpu))
             "delta-report-metadata returns supplied metadata")
      (check run (equal (delta-report->plist delta-report)
                        '(:name :adapter-smoke
                          :baseline :refinterp
                          :candidate :cpu-adapter
                          :runtime-ns 1000
                          :speedup 2.0d0
                          :register-count 12
                          :occupancy 0.5d0
                          :memory-traffic (:bytes 4096)
                          :numerics (:max-abs-error 0d0)
                          :metadata (:adapter :cpu)))
             "delta-report->plist returns stable machine-readable shape")
      (check run (= (relative-error-pct 110 100) 10d0)
             "relative-error-pct computes percent absolute relative error")
      (check run (= (relative-error-pct -90 -100) 10d0)
             "relative-error-pct uses absolute measured magnitude")
      (check run (null (relative-error-pct 1 0))
             "relative-error-pct returns NIL for zero measured value")
      (check run (null (relative-error-pct :x 1))
             "relative-error-pct returns NIL for nonnumeric predicted value")

      ;; --- Publication certificates ---------------------------------------
      (let ((published (publish-delta-report delta-report
                                             :metadata '(:pub :delta))))
        (check run (certificate-p published)
               "publish-delta-report returns a certificate")
        (check run (runtime-verify published)
               "published delta verifies")
        (check run (eq (certificate-name published) :publish-delta-report)
               "published delta uses default name")
        (check run (eq (certificate-kind published) :publish)
               "published delta has publish kind")
        (check run (eq (certificate-claim published) :delta-report-published)
               "published delta records claim")
        (check run (equal (certificate-payload published)
                          (delta-report->plist delta-report))
               "published delta payload is the delta plist")
        (check run (eq (certificate-witness published) delta-report)
               "published delta carries the report as its witness")
        (check run (equal (certificate-metadata published) '(:pub :delta))
               "published delta metadata is preserved"))

      ;; --- NEGATIVE CONTROLS: publish-delta-report must be able to FAIL ---
      ;; Regression guard.  PASSED was once hardcoded T with a null witness,
      ;; which made (runtime-verify (publish-delta-report R)) reduce to T for
      ;; EVERY R -- a verifier that could not fail, at fan-in 219.
      (let ((incomplete (make-delta-report
                         :name :adapter-incomplete
                         :baseline :refinterp
                         :candidate :cpu-adapter
                         :runtime-ns 1000
                         :speedup 2.0d0
                         ;; register-count / occupancy / memory-traffic /
                         ;; numerics all absent
                         :metadata '(:adapter :cpu))))
        (check run (not (delta-report-complete-p incomplete))
               "an incomplete delta report is not complete")
        (check run (delta-report-complete-p delta-report)
               "a complete delta report is complete")
        (let ((published (publish-delta-report incomplete)))
          (check run (not (certificate-passed published))
                 "publishing an INCOMPLETE delta yields passed = NIL")
          (check run (not (runtime-verify published))
                 "an incomplete published delta FAILS runtime-verify"))
        ;; The witness is re-derived, not taken on trust: a certificate whose
        ;; passed bit is forced T over an incomplete report still fails.
        (let ((forged (make-certificate :kind :publish
                                        :claim :delta-report-published
                                        :passed t
                                        :witness incomplete)))
          (check run (not (runtime-verify forged))
                 "a forged passed=T cannot survive witness re-derivation")))

      ;; ACCEPT supplies the judgement this library does not own, and the
      ;; CLAIM tracks which question was actually answered.
      (let* ((regressed (make-delta-report
                         :name :adapter-regressed
                         :baseline :refinterp
                         :candidate :cpu-adapter
                         :runtime-ns 100000
                         :speedup 0.01d0
                         :register-count 12
                         :occupancy 0.5d0
                         :memory-traffic '(:bytes 4096)
                         :numerics '(:max-abs-error 1d3)
                         :metadata '(:adapter :cpu)))
             (faster (lambda (r) (> (delta-report-speedup r) 1d0)))
             (plain (publish-delta-report regressed))
             (gated (publish-delta-report regressed :accept faster))
             (gated-ok (publish-delta-report delta-report :accept faster)))
        (check run (eq (certificate-claim plain) :delta-report-published)
               "without ACCEPT the claim is only that the report was published")
        (check run (certificate-passed plain)
               "a complete but regressed report IS publishable -- no threshold is invented")
        (check run (eq (certificate-claim gated) :delta-report-accepted)
               "with ACCEPT the claim states that acceptance was decided")
        (check run (not (certificate-passed gated))
               "a regressed report FAILS a caller-supplied acceptance predicate")
        (check run (not (runtime-verify gated))
               "a rejected delta fails runtime-verify")
        (check run (certificate-passed gated-ok)
               "a faster report passes the same acceptance predicate")
        (check run (not (certificate-passed
                         (publish-delta-report
                          (make-delta-report :name :adapter-incomplete
                                             :runtime-ns 1000
                                             :speedup 9d0)
                          :accept (constantly t))))
               "ACCEPT cannot rescue an incomplete report"))
      (let ((published (publish-certificate certificate
                                            :name :pub-cert
                                            :metadata '(:pub :cert))))
        (check run (certificate-p published)
               "publish-certificate returns a certificate")
        (check run (runtime-verify published)
               "published certificate verifies when source certificate verifies")
        (check run (eq (certificate-name published) :pub-cert)
               "publish-certificate honors explicit name")
        (check run (eq (certificate-kind published) :publish)
               "published certificate has publish kind")
        (check run (eq (certificate-claim published) :certificate-published)
               "published certificate records claim")
        (check run (equal (certificate-payload published)
                          (certificate->plist certificate))
               "published certificate payload is certificate plist")
        (check run (eq (certificate-witness published) certificate)
               "published certificate keeps original certificate as witness")
        (check run (equal (certificate-metadata published) '(:pub :cert))
               "published certificate metadata is preserved"))

      ;; --- Copy isolation and validation ----------------------------------
      (let* ((certificate-metadata (list :certificate :caller))
             (delta-metadata (list :delta :caller))
             (publish-metadata (list :publish :caller))
             (isolated-certificate
               (make-certificate :passed t :metadata certificate-metadata))
             (isolated-delta
               (make-delta-report :metadata delta-metadata))
             (published-certificate
               (publish-certificate isolated-certificate
                                    :metadata publish-metadata)))
        (setf (getf certificate-metadata :certificate) :mutated
              (getf delta-metadata :delta) :mutated
              (getf publish-metadata :publish) :mutated)
        (check run (equal (certificate-metadata isolated-certificate)
                          '(:certificate :caller))
               "certificate metadata is isolated")
        (check run (equal (delta-report-metadata isolated-delta)
                          '(:delta :caller))
               "delta report metadata is isolated")
        (check run (equal (certificate-metadata published-certificate)
                          '(:publish :caller))
               "published certificate metadata is isolated")
        (let ((plist (certificate->plist isolated-certificate)))
          (setf (getf (getf plist :metadata) :certificate) :plist-mutated)
          (check run (equal (certificate-metadata isolated-certificate)
                            '(:certificate :caller))
                 "certificate plist metadata is isolated"))
        (let ((plist (delta-report->plist isolated-delta)))
          (setf (getf (getf plist :metadata) :delta) :plist-mutated)
          (check run (equal (delta-report-metadata isolated-delta)
                            '(:delta :caller))
                 "delta plist metadata is isolated")))
      (check run (signals-error-p
                  (lambda () (make-certificate :metadata '(:dangling))))
             "certificate rejects malformed metadata")
      (check run (signals-error-p
                  (lambda () (make-certificate :metadata '(1 :bad))))
             "certificate metadata keys must be symbols")
      (check run (signals-error-p
                  (lambda () (make-delta-report :metadata '(:dangling))))
             "delta report rejects malformed metadata")
      (check run (signals-error-p
                  (lambda () (make-delta-report :metadata '(1 :bad))))
             "delta report metadata keys must be symbols")
      (check run (signals-error-p
                  (lambda () (publish-certificate certificate
                                                  :metadata '(:dangling))))
             "published certificate rejects malformed metadata")
      (check run (signals-error-p
                  (lambda () (publish-delta-report delta-report
                                                   :metadata '(:dangling))))
             "published delta rejects malformed metadata")
      (check run (signals-error-p
                  (lambda () (runtime-verify :not-a-witness)))
             "runtime-verify rejects unsupported objects"))))
