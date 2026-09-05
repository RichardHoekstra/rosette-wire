;;;; distributed-work-claim.lisp --- distributed-work-claim implementation.

(in-package #:distributed-work-claim)

(define-condition work-claim-error (error)
  ((code :initarg :code :reader work-claim-error-code)
   (detail :initarg :detail :reader work-claim-error-detail))
  (:report (lambda (condition stream)
             (format stream "Work claim ~A: ~A"
                     (work-claim-error-code condition)
                     (work-claim-error-detail condition)))))

(defun %fail (code control &rest arguments)
  (error 'work-claim-error :code code
                           :detail (apply #'format nil control arguments)))

(defun %name (value field)
  (unless (and (stringp value) (plusp (length value)))
    (%fail :invalid-worker "~A must be a non-empty string" field))
  (copy-seq value))

(defun %string-set (values field)
  (unless (and (listp values) (every #'stringp values))
    (%fail :invalid-worker "~A must be a list of strings" field))
  (sort (remove-duplicates (mapcar #'copy-seq values) :test #'string=)
        #'string<))

(defun %positive-integer (value field)
  (unless (and (integerp value) (plusp value))
    (%fail :invalid-bound "~A must be a positive integer" field))
  value)

(defstruct (work-worker
             (:constructor %make-work-worker)
             (:conc-name %work-worker-)
             (:copier nil))
  (id "" :type string :read-only t)
  (capabilities nil :type list :read-only t)
  (max-steps 1 :type (integer 1 *) :read-only t)
  (max-output-bytes 1 :type (integer 1 *) :read-only t))

(defun make-work-worker (&key id capabilities max-steps max-output-bytes)
  "Describe a worker's advertised capability and resource envelope.

This is scheduling authority only. It contains no payment, ownership,
publication, or bearer-token semantics."
  (%make-work-worker
   :id (%name id "worker id")
   :capabilities (%string-set capabilities "worker capabilities")
   :max-steps (%positive-integer max-steps "worker max-steps")
   :max-output-bytes
   (%positive-integer max-output-bytes "worker max-output-bytes")))

(defun work-worker-id (worker) (copy-seq (%work-worker-id worker)))
(defun work-worker-capabilities (worker)
  (mapcar #'copy-seq (%work-worker-capabilities worker)))
(defun work-worker-max-steps (worker) (%work-worker-max-steps worker))
(defun work-worker-max-output-bytes (worker)
  (%work-worker-max-output-bytes worker))

(defun %composition-grants (composition)
  (mapcar #'copy-seq
          (cdr (assoc "capabilityGrants" (composition->value composition)
                      :test #'string=))))

(defun %composition-required-capabilities (composition)
  (sort
   (remove-duplicates
    (loop for node in (composition-nodes composition)
          append (component-descriptor-capabilities
                  (component-node-descriptor node)))
    :test #'string=)
   #'string<))

(defun %subsetp (required available)
  (every (lambda (capability)
           (member capability available :test #'string=))
         required))

(defstruct (distributed-work-claim
             (:constructor %make-distributed-work-claim)
             (:conc-name %distributed-work-claim-)
             (:copier nil))
  (id "" :type string :read-only t)
  (composition-id "" :type string :read-only t)
  (worker-id "" :type string :read-only t)
  (input-id "" :type string :read-only t)
  (inputs nil :read-only t)
  (required-capabilities nil :type list :read-only t)
  (max-steps 1 :type (integer 1 *) :read-only t)
  (max-output-bytes 1 :type (integer 1 *) :read-only t)
  (depends-on nil :type list :read-only t))

(defun %claim-form (composition-id worker-id input-id required max-steps
                    max-output-bytes depends-on)
  `(("authority" . (("financial" . "absent")
                     ("publication" . "absent")
                     ("transfer" . "worker-bound")))
    ("compositionId" . ,composition-id)
    ("dependsOn" . ,depends-on)
    ("inputId" . ,input-id)
    ("limits" . (("maxOutputBytes" . ,max-output-bytes)
                  ("maxSteps" . ,max-steps)))
    ("requiredCapabilities" . ,required)
    ("schema" . "org.rosette.distributed-work-claim/v1")
    ("workerId" . ,worker-id)))

(defun make-distributed-work-claim
    (worker composition inputs &key max-steps max-output-bytes depends-on)
  "Bind exact Rosette work and inputs to WORKER under explicit ceilings.

The Composition must grant exactly the capabilities declared by its
Components: surplus ambient grants are refused. WORKER may advertise a
superset, but the claim records only the exact required subset."
  (check-type worker work-worker)
  (check-type composition composition)
  (let* ((validation (validate-composition composition))
         (required (%composition-required-capabilities composition))
         (grants (%composition-grants composition))
         (steps (or max-steps (length (composition-steps composition))))
         (output-bytes (or max-output-bytes
                           (work-worker-max-output-bytes worker)))
         (dependencies (%string-set (or depends-on nil) "claim dependencies"))
         (input-copy (copy-tree inputs))
         (input-id (canonical-id input-copy)))
    (unless (eq :pass (wire-receipt-verdict validation))
      (%fail :invalid-composition "Rosette validation refused the Composition"))
    (unless (equal required grants)
      (%fail :surplus-authority
             "Composition grants ~S but Components require exactly ~S"
             grants required))
    (unless (%subsetp required (work-worker-capabilities worker))
      (%fail :capability-unavailable "worker ~A lacks one of ~S"
             (work-worker-id worker) required))
    (%positive-integer steps "claim max-steps")
    (%positive-integer output-bytes "claim max-output-bytes")
    (when (< steps (length (composition-steps composition)))
      (%fail :step-budget "claim permits ~D steps but Composition has ~D"
             steps (length (composition-steps composition))))
    (when (> steps (work-worker-max-steps worker))
      (%fail :step-budget "claim exceeds worker step ceiling"))
    (when (> output-bytes (work-worker-max-output-bytes worker))
      (%fail :output-budget "claim exceeds worker output ceiling"))
    (let* ((composition-id (composition-id composition))
           (worker-id (work-worker-id worker))
           (form (%claim-form composition-id worker-id input-id required
                              steps output-bytes dependencies)))
      (%make-distributed-work-claim
       :id (canonical-id form) :composition-id composition-id
       :worker-id worker-id :input-id input-id :inputs input-copy
       :required-capabilities required :max-steps steps
       :max-output-bytes output-bytes :depends-on dependencies))))

(defun distributed-work-claim-id (claim)
  (copy-seq (%distributed-work-claim-id claim)))
(defun distributed-work-claim-composition-id (claim)
  (copy-seq (%distributed-work-claim-composition-id claim)))
(defun distributed-work-claim-worker-id (claim)
  (copy-seq (%distributed-work-claim-worker-id claim)))
(defun distributed-work-claim-input-id (claim)
  (copy-seq (%distributed-work-claim-input-id claim)))
(defun distributed-work-claim-inputs (claim)
  (copy-tree (%distributed-work-claim-inputs claim)))
(defun distributed-work-claim-required-capabilities (claim)
  (mapcar #'copy-seq (%distributed-work-claim-required-capabilities claim)))
(defun distributed-work-claim-max-steps (claim)
  (%distributed-work-claim-max-steps claim))
(defun distributed-work-claim-max-output-bytes (claim)
  (%distributed-work-claim-max-output-bytes claim))
(defun distributed-work-claim-depends-on (claim)
  (mapcar #'copy-seq (%distributed-work-claim-depends-on claim)))
(defun distributed-work-claim-financial-authority (claim)
  (check-type claim distributed-work-claim)
  :absent)
(defun distributed-work-claim-publication-authority (claim)
  (check-type claim distributed-work-claim)
  :absent)

(defstruct (work-claim-receipt
             (:constructor %make-work-claim-receipt)
             (:conc-name %work-claim-receipt-)
             (:copier nil))
  (id "" :type string :read-only t)
  (verdict :refused :type (member :completed :refused) :read-only t)
  (claim-id "" :type string :read-only t)
  (worker-id "" :type string :read-only t)
  (composition-id "" :type string :read-only t)
  (input-id "" :type string :read-only t)
  (output-id nil :type (or null string) :read-only t)
  (step-count 0 :type (integer 0 *) :read-only t)
  (output-bytes 0 :type (integer 0 *) :read-only t)
  (composition-receipt nil :type (or null wire-receipt) :read-only t)
  (refusal nil :type (or null string) :read-only t))

(defun %receipt-form (verdict claim worker composition-receipt output-id
                      step-count output-bytes refusal)
  `(("claimId" . ,(distributed-work-claim-id claim))
    ("compositionId" . ,(distributed-work-claim-composition-id claim))
    ("compositionReceiptId" .
     ,(if composition-receipt (wire-receipt-id composition-receipt) "absent"))
    ("inputId" . ,(distributed-work-claim-input-id claim))
    ("outputBytes" . ,output-bytes)
    ("outputId" . ,(or output-id "absent"))
    ("refusal" . ,(or refusal "absent"))
    ("schema" . "org.rosette.distributed-work-receipt/v1")
    ("stepCount" . ,step-count)
    ("verdict" . ,(ecase verdict
                     (:completed "completed") (:refused "refused")))
    ("workerId" . ,(work-worker-id worker))))

(defun %make-receipt (verdict claim worker &key composition-receipt output-id
                                             (step-count 0) (output-bytes 0)
                                             refusal)
  (let ((form (%receipt-form verdict claim worker composition-receipt output-id
                             step-count output-bytes refusal)))
    (%make-work-claim-receipt
     :id (canonical-id form) :verdict verdict
     :claim-id (distributed-work-claim-id claim)
     :worker-id (work-worker-id worker)
     :composition-id (distributed-work-claim-composition-id claim)
     :input-id (distributed-work-claim-input-id claim)
     :output-id output-id :step-count step-count :output-bytes output-bytes
     :composition-receipt composition-receipt :refusal refusal)))

(defun %claim-preflight-refusal (claim worker composition)
  (cond
    ((not (string= (distributed-work-claim-worker-id claim)
                   (work-worker-id worker)))
     "worker-binding-mismatch")
    ((not (string= (distributed-work-claim-composition-id claim)
                   (composition-id composition)))
     "composition-identity-mismatch")
    ((not (string= (distributed-work-claim-input-id claim)
                   (canonical-id (distributed-work-claim-inputs claim))))
     "input-identity-mismatch")
    ((not (equal (distributed-work-claim-required-capabilities claim)
                 (%composition-required-capabilities composition)))
     "capability-claim-mismatch")
    ((not (%subsetp (distributed-work-claim-required-capabilities claim)
                    (work-worker-capabilities worker)))
     "worker-capability-mismatch")
    ((> (distributed-work-claim-max-steps claim)
        (work-worker-max-steps worker))
     "worker-step-ceiling")
    ((> (distributed-work-claim-max-output-bytes claim)
        (work-worker-max-output-bytes worker))
     "worker-output-ceiling")
    (t nil)))

(defun execute-distributed-work-claim (claim worker composition runner)
  "Execute CLAIM and return a completed or replayable refused receipt.

Rosette's certification verdict is the sole validation authority. This layer
only binds that verdict to content, worker, capabilities, and ceilings."
  (check-type claim distributed-work-claim)
  (check-type worker work-worker)
  (check-type composition composition)
  (let ((preflight (%claim-preflight-refusal claim worker composition)))
    (when preflight
      (return-from execute-distributed-work-claim
        (%make-receipt :refused claim worker :refusal preflight)))
    (handler-case
        (let* ((receipt
                 (certify-composition
                  composition runner (distributed-work-claim-inputs claim)))
               (steps (length (wire-receipt-step-order receipt))))
          (unless (eq :pass (wire-receipt-verdict receipt))
            (return-from execute-distributed-work-claim
              (%make-receipt :refused claim worker
                             :composition-receipt receipt :step-count steps
                             :refusal "rosette-certification-refused")))
          (let* ((outputs (wire-receipt-outputs receipt))
                 (bytes (length (string->bytes (canonical-json outputs))))
                 (output-id (canonical-id outputs)))
            (cond
              ((> steps (distributed-work-claim-max-steps claim))
               (%make-receipt :refused claim worker
                              :composition-receipt receipt :step-count steps
                              :output-bytes bytes :output-id output-id
                              :refusal "claim-step-ceiling"))
              ((> bytes (distributed-work-claim-max-output-bytes claim))
               (%make-receipt :refused claim worker
                              :composition-receipt receipt :step-count steps
                              :output-bytes bytes :output-id output-id
                              :refusal "claim-output-ceiling"))
              (t
               (%make-receipt :completed claim worker
                              :composition-receipt receipt :step-count steps
                              :output-bytes bytes :output-id output-id)))))
      (error (condition)
        (%make-receipt :refused claim worker
                       :refusal (format nil "execution-error: ~A" condition))))))

(defun verify-distributed-work-receipt
    (claim worker composition runner receipt)
  "Freshly execute exact work and compare its content-addressed receipt."
  (check-type receipt work-claim-receipt)
  (let ((fresh (execute-distributed-work-claim
                claim worker composition runner)))
    (values (string= (work-claim-receipt-id receipt)
                     (work-claim-receipt-id fresh))
            fresh)))

(defun work-claim-receipt-id (receipt)
  (copy-seq (%work-claim-receipt-id receipt)))
(defun work-claim-receipt-verdict (receipt) (%work-claim-receipt-verdict receipt))
(defun work-claim-receipt-claim-id (receipt)
  (copy-seq (%work-claim-receipt-claim-id receipt)))
(defun work-claim-receipt-worker-id (receipt)
  (copy-seq (%work-claim-receipt-worker-id receipt)))
(defun work-claim-receipt-output-id (receipt)
  (let ((value (%work-claim-receipt-output-id receipt)))
    (and value (copy-seq value))))
(defun work-claim-receipt-step-count (receipt)
  (%work-claim-receipt-step-count receipt))
(defun work-claim-receipt-output-bytes (receipt)
  (%work-claim-receipt-output-bytes receipt))
(defun work-claim-receipt-composition-receipt (receipt)
  (%work-claim-receipt-composition-receipt receipt))
(defun work-claim-receipt-refusal (receipt)
  (let ((value (%work-claim-receipt-refusal receipt)))
    (and value (copy-seq value))))

(defun %claims->items (claims)
  (let ((ids (mapcar #'distributed-work-claim-id claims)))
    (unless (= (length ids) (length (remove-duplicates ids :test #'string=)))
      (%fail :duplicate-claim "claim identities must be unique"))
    (dolist (claim claims)
      (dolist (dependency (distributed-work-claim-depends-on claim))
        (unless (member dependency ids :test #'string=)
          (%fail :missing-dependency "claim ~A depends on absent claim ~A"
                 (distributed-work-claim-id claim) dependency))))
    (mapcar
     (lambda (claim)
       (make-work-item
        :name (distributed-work-claim-id claim)
        :reads (list (distributed-work-claim-input-id claim))
        :writes (list (format nil "receipt:~A"
                              (distributed-work-claim-id claim)))
        :cost (distributed-work-claim-max-steps claim)
        :depends-on (distributed-work-claim-depends-on claim)
        :metadata `((:worker . ,(distributed-work-claim-worker-id claim)))))
     claims)))

(defun plan-distributed-work-claims (claims &key max-workers)
  "Plan independent claims with WORK-SCHEDULER and return plan, certificate."
  (let* ((items (%claims->items claims))
         (plan (plan-work-schedule items :max-workers max-workers
                                  :min-speedup 1d0))
         (certificate (schedule-certificate
                       items plan :name :distributed-work-claims)))
    (values plan certificate)))

(defun verify-distributed-work-schedule (claims plan certificate)
  "Independently verify both the schedule and its stored certificate."
  (let ((items (%claims->items claims)))
    (multiple-value-bind (plan-ok plan-reasons)
        (verify-parallel-plan items plan)
      (multiple-value-bind (certificate-ok certificate-reasons)
          (verify-schedule-certificate items certificate)
        (values (and plan-ok certificate-ok)
                (append plan-reasons certificate-reasons))))))
