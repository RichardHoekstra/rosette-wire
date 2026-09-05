;;;; rosette-proof-witness/src/runtime-verify.lisp --- Runtime witness checks.

(in-package #:rosette-proof-witness)

(defun relative-error-pct (predicted measured)
  "Percent relative error 100*|PREDICTED-MEASURED|/|MEASURED| in double-float,
or NIL when either is non-numeric or MEASURED is zero.  The shared definition
for predicted-vs-measured verification witnesses (was duplicated verbatim in
rosette-conjecture-test and rosette-verification-witness)."
  (when (and (numberp predicted)
             (numberp measured)
             (not (zerop measured)))
    (* 100d0 (/ (abs (- (coerce predicted 'double-float)
                        (coerce measured 'double-float)))
                (abs (coerce measured 'double-float))))))

(defun runtime-verify (witness)
  "Verify a Lean witness or unified certificate against the current model.

Returns T if the test passes and NIL otherwise. Errors raised inside
TEST-FN are converted to NIL plus a short report, so a single broken
witness does not abort a verification batch.

Future Lean-backed versions can additionally invoke `lean --run` on
WITNESS's LEAN-PATH and require both verdicts to agree."
  (cond
    ((certificate-p witness)
     (and (certificate-passed witness)
          (or (null (certificate-witness witness))
              (runtime-verify (certificate-witness witness)))))
    ;; A delta report as a witness re-derives its own completeness rather
    ;; than being accepted for merely existing.
    ((delta-report-p witness)
     (delta-report-complete-p witness))
    ((lean-witness-p witness)
     (handler-case
         (let ((result (funcall (lean-witness-test-fn witness))))
           (and result t))
       (error (c)
         (format *error-output*
                 "~&;; runtime-verify: witness ~A raised ~A~%"
                 (lean-witness-theorem-name witness) c)
         nil)))
    (t
     (error "runtime-verify expected a LEAN-WITNESS or CERTIFICATE, got ~S"
            (type-of witness)))))
