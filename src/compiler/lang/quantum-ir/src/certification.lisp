;;;; certification.lisp --- proof-carrying ProgramIR structural metadata.

(in-package #:rosette-quantum-ir)

(defconstant +resource-certificates-metadata-key+
  :rosette-quantum-ir/resource-certificates)

(defun %metadata-key-value (metadata key)
  "Return VALUE, PRESENT-P, and occurrence count for KEY in a proper plist."
  (if (and (%proper-list-p metadata) (evenp (length metadata)))
      (let ((count 0)
            (value nil))
        (loop for (candidate candidate-value) on metadata by #'cddr
              when (eq candidate key) do
                (incf count)
                (when (= count 1)
                  (setf value candidate-value)))
        (values value (plusp count) count))
      (values nil nil 0)))

(defun certify-program (program)
  "Return a fresh proof-carrying copy of admitted quantum PROGRAM.

The root ProgramIR metadata contains the exact structural certificate for
every admitted SSA value. The second value is the admission report used to
construct the carrier. Source metadata is neither trusted nor copied."
  (let* ((fresh (make-quantum-program (program-form program)))
         (report (check-program fresh)))
    (unless (program-report-ok-p report)
      (error "Cannot structurally certify an invalid quantum ProgramIR: ~{~A~^; ~}"
             (mapcar #'diagnostic-message (program-report-errors report))))
    ;; Re-admit independently so the returned REPORT and embedded proof trees
    ;; have no mutable derivation objects in common.
    (let ((certificates
            (program-report-resources (check-program fresh))))
      (unless (every #'verify-resource-certificate certificates)
        (error "Internal error: admission emitted a non-replayable resource certificate."))
      (setf (rosette-program-ir:program-metadata fresh)
            (list +resource-certificates-metadata-key+ certificates))
      (values fresh report))))

(defun certified-program-resource-certificates (program)
  "Return fresh copies of PROGRAM's attached structural certificates.

NIL means either that no resource certificates are attached or that the
admitted program defines no values; use VERIFY-CERTIFIED-PROGRAM when the
distinction matters."
  (check-type program rosette-program-ir:program)
  (multiple-value-bind (certificates present-p count)
      (%metadata-key-value
       (rosette-program-ir:program-metadata program)
       +resource-certificates-metadata-key+)
    (declare (ignore certificates count))
    (when present-p
      (unless (verify-certified-program program)
        (error "ProgramIR structural certificate map does not replay."))
      ;; A new admission pass owns fresh certificate and derivation objects.
      (program-report-resources (check-program program)))))

(defun verify-certified-program (program)
  "Re-admit PROGRAM and reject altered or incomplete structural metadata."
  (and
   (rosette-program-ir:programp program)
   (handler-case
       (multiple-value-bind (stored present-p count)
           (%metadata-key-value
            (rosette-program-ir:program-metadata program)
            +resource-certificates-metadata-key+)
         (let ((report (check-program program)))
           (and present-p
                (= count 1)
                (program-report-ok-p report)
                (listp stored)
                (every #'resource-certificate-p stored)
                (every #'verify-resource-certificate stored)
                (equalp stored (program-report-resources report)))))
     (error () nil))))
