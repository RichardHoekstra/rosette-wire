;;;; runtime.lisp --- deterministic, bounded Wire execution and verification.

(in-package #:rosette-wire)

(defstruct (wire-runner (:constructor %make-wire-runner (handlers verifiers)))
  (handlers (make-hash-table :test #'equal) :read-only t)
  (verifiers (make-hash-table :test #'equal) :read-only t))

(defun make-wire-runner ()
  (%make-wire-runner (make-hash-table :test #'equal)
                     (make-hash-table :test #'equal)))

(defun %handler-key (node port operation)
  (list node port operation))

(defun register-wire-handler (runner node port operation function)
  "Register FUNCTION for one node/export/operation in RUNNER.

FUNCTION receives (ARGUMENT-ALIST SERVICE-ALIST CONTEXT-ALIST) and returns one
value. Registration mutates only the explicitly stateful runner, never a Wire
descriptor or graph."
  (check-type runner wire-runner)
  (check-type function function)
  (setf (gethash (%handler-key (%require-name node "$.handler.node")
                               (%require-name port "$.handler.port")
                               (%require-name operation "$.handler.operation"))
                 (wire-runner-handlers runner))
        function)
  runner)

(defun register-wire-verifier (runner name function)
  "Register deterministic FUNCTION for evidence NAME.

FUNCTION receives (GRAPH RECEIPT) and returns (values PASS-P EVIDENCE-VALUE)."
  (check-type runner wire-runner)
  (check-type function function)
  (setf (gethash (%require-name name "$.verifier.name")
                 (wire-runner-verifiers runner))
        function)
  runner)

(defun %object-ref (object name &optional default)
  (let ((entry (and (listp object) (assoc name object :test #'string=))))
    (if entry (cdr entry) default)))

(defun %limit (graph name default)
  (let ((value (%object-ref (%wire-graph-limits graph) name default)))
    (if (and (integerp value) (not (minusp value))) value default)))

(defun %signed-range-p (value bits)
  (and (integerp value)
       (<= (- (ash 1 (1- bits))) value (1- (ash 1 (1- bits))))))

(defun %unsigned-range-p (value bits)
  (and (integerp value) (<= 0 value (1- (ash 1 bits)))))

(defun %bytes-p (value)
  (and (vectorp value)
       (every (lambda (byte) (%unsigned-range-p byte 8)) value)))

(defun %value-conforms-p (value type)
  (let ((kind (%wire-type-kind type)) (payload (%wire-type-payload type)))
    (case kind
      (:scalar
       (ecase payload
         (:bool (or (eq value json-true) (eq value json-false)
                    (eq value t) (null value)))
         (:s64 (%signed-range-p value 64))
         (:u64 (%unsigned-range-p value 64))
         (:f64 (realp value))
         (:string (stringp value))
         (:bytes (%bytes-p value))))
      (:list (and (listp value)
                  (every (lambda (item) (%value-conforms-p item payload)) value)))
      (:option (or (eq value json-null) (null value)
                   (%value-conforms-p value payload)))
      (:result
       (and (listp value)
            (let ((ok (assoc "ok" value :test #'string=))
                  (error (assoc "error" value :test #'string=)))
              (or (and ok (null error)
                       (%value-conforms-p (cdr ok) (first payload)))
                  (and error (null ok)
                       (%value-conforms-p (cdr error) (second payload)))))))
      (:tuple
       (and (or (listp value) (vectorp value))
            (= (length value) (length payload))
            (every #'%value-conforms-p (coerce value 'list) payload)))
      (:record
       (and (listp value)
            (every (lambda (field)
                     (let ((entry (assoc (%wire-field-name field) value :test #'string=)))
                       (and entry (%value-conforms-p
                                   (cdr entry) (%wire-field-type field)))))
                   payload)))
      (:variant
       (and (listp value) (= (length value) 1)
            (let ((field (%lookup (caar value) payload #'%wire-field-name)))
              (and field (%value-conforms-p (cdar value)
                                            (%wire-field-type field))))))
      (:tensor
       ;; The portable v0.1 tensor value is a typed record. Adapters may use a
       ;; zero-copy representation internally, but receipts cross this seam.
       (and (listp value)
            (let ((shape (%object-ref value "shape" :missing))
                  (data (%object-ref value "data" :missing)))
              (and (listp shape) (not (eq data :missing))
                   (= (length shape) (length (second payload)))
                   (every (lambda (dimension) (and (integerp dimension)
                                                   (not (minusp dimension))))
                          shape)))))
      (:resource (and (listp value) (%object-ref value "handle" nil)))
      (otherwise nil))))

(defun %input-value (inputs name)
  (let ((entry (assoc name inputs :test #'string=)))
    (values (and entry (cdr entry)) (not (null entry)))))

(defun %resolved-services (graph node-id)
  (loop for binding in (%wire-graph-services graph)
        when (string= node-id (%service-binding-consumer-node binding))
          collect
          (cons (%service-binding-import-port binding)
                (if (%service-binding-host-port binding)
                    `(("kind" . "host")
                      ("binding" . ,(%service-binding-id binding)))
                    `(("kind" . "wire")
                      ("binding" . ,(%service-binding-id binding))
                      ("node" . ,(%service-binding-provider-node binding))
                      ("port" . ,(%service-binding-provider-port binding)))))))

(defun %runtime-violation (stage path code control &rest arguments)
  (%make-wire-violation stage path code (apply #'format nil control arguments)))

(defun run-wire-graph (graph runner inputs)
  "Validate and execute GRAPH in deterministic topological order.

MAX-STEPS and MAX-OUTPUT-BYTES are hard bounds. MAX-WALL-MILLISECONDS is
checked before and after each in-process call and therefore honestly remains a
cooperative bound; hard interruption belongs to the worker-process adapter."
  (check-type graph wire-graph)
  (check-type runner wire-runner)
  (%require-list inputs "$.run.inputs")
  (let ((validation (validate-wire-graph graph)))
    (when (eq :fail (wire-receipt-verdict validation))
      (return-from run-wire-graph
        (%make-wire-receipt :execution :fail (wire-graph-id graph)
                            (wire-receipt-violations validation) nil nil nil)))
    (let* ((order (wire-receipt-step-order validation))
           (max-steps (%limit graph "maxSteps" (length order)))
           (max-output-bytes (%limit graph "maxOutputBytes" 1048576))
           (max-wall-ms (%limit graph "maxWallMilliseconds" 0))
           (started (get-internal-real-time))
           (ticks internal-time-units-per-second)
           (results (make-hash-table :test #'equal))
           (observed nil)
           (violations nil))
      (labels ((elapsed-ms ()
                 (floor (* 1000 (- (get-internal-real-time) started)) ticks))
               (fail (stage path code control &rest arguments)
                 (push (apply #'%runtime-violation stage path code control arguments)
                       violations)))
        ;; Inputs are exact and all declared values must be present/conform.
        (dolist (field (%wire-graph-inputs graph))
          (multiple-value-bind (value present-p)
              (%input-value inputs (%wire-field-name field))
            (cond ((not present-p)
                   (fail :inputs (format nil "$.inputs.~A" (%wire-field-name field))
                         :missing-input "required graph input is absent"))
                  ((not (%value-conforms-p value (%wire-field-type field)))
                   (fail :inputs (format nil "$.inputs.~A" (%wire-field-name field))
                         :input-type-mismatch "input does not satisfy its Wire type")))))
        (when (> (length order) max-steps)
          (fail :limits "$.limits.maxSteps" :step-budget
                "plan has ~D steps above limit ~D" (length order) max-steps))
        (unless violations
          (dolist (step-id order)
            (when (and (plusp max-wall-ms) (>= (elapsed-ms) max-wall-ms))
              (fail :limits "$.limits.maxWallMilliseconds" :wall-budget
                    "cooperative wall budget exhausted before step ~A" step-id)
              (return))
            (let* ((step (%step graph step-id))
                   (operation (%step-operation-object graph step))
                   (handler (gethash (%handler-key (%wire-step-node-id step)
                                                  (%wire-step-port step)
                                                  (%wire-step-operation step))
                                     (wire-runner-handlers runner)))
                   (arguments nil))
              (unless handler
                (fail :runtime (format nil "$.steps.~A" step-id)
                      :handler-unavailable "no runner handler is registered")
                (return))
              (dolist (binding (%wire-step-bindings step))
                (let* ((source (%data-binding-source binding))
                       (value
                         (ecase (%data-source-kind source)
                           (:input (cdr (assoc (%data-source-reference source) inputs
                                              :test #'string=)))
                           (:literal (%freeze-value (%data-source-value source)))
                           (:step (gethash (%data-source-reference source) results)))))
                  (push (cons (%data-binding-argument binding) value) arguments)))
              (setf arguments (nreverse arguments))
              (handler-case
                  (let ((result
                          (funcall handler arguments
                                   (%resolved-services graph (%wire-step-node-id step))
                                   `(("graphId" . ,(wire-graph-id graph))
                                     ("step" . ,step-id)
                                     ("capabilities" .
                                      ,(%wire-graph-capability-grants graph))))))
                    (unless (%value-conforms-p result
                                              (%wire-operation-output operation))
                      (fail :runtime (format nil "$.steps.~A.output" step-id)
                            :output-type-mismatch
                            "handler result does not satisfy its Wire type")
                      (return))
                    (setf (gethash step-id results) result)
                    (push step-id observed))
                (error (condition)
                  (fail :runtime (format nil "$.steps.~A" step-id)
                        :component-error "component signaled ~A" condition)
                  (return)))
              (when (and (plusp max-wall-ms) (> (elapsed-ms) max-wall-ms))
                (fail :limits "$.limits.maxWallMilliseconds" :wall-budget
                      "step ~A returned after cooperative wall budget" step-id)
                (return)))))
        (let ((outputs
                (unless violations
                  (loop for output in (%wire-graph-outputs graph)
                        collect (cons (%wire-output-name output)
                                      (%typed-protocol-value
                                       (gethash (%wire-output-step-id output)
                                                results)))))))
          (when outputs
            (handler-case
                (let ((size (length (string->bytes (canonical-json outputs)))))
                  (when (> size max-output-bytes)
                    (fail :limits "$.limits.maxOutputBytes" :output-budget
                          "canonical outputs use ~D bytes above limit ~D"
                          size max-output-bytes)))
              (wire-error (condition)
                (push (%make-wire-violation
                       :outputs "$.outputs" (wire-error-code condition)
                       (wire-error-detail condition)) violations))))
          (%make-wire-receipt
           :execution (if violations :fail :pass) (wire-graph-id graph)
           (nreverse violations) (nreverse observed)
           (if violations nil outputs)
           `(("limitEnforcement" .
              (("maxOutputBytes" . "enforced")
               ("maxSteps" . "enforced")
               ("maxWallMilliseconds" . "cooperative"))))))))))

(defun verify-wire-receipt (graph receipt runner)
  "Re-run required verifier callbacks; never trust RECEIPT's prior verdict."
  (check-type graph wire-graph)
  (check-type receipt wire-receipt)
  (check-type runner wire-runner)
  (let ((violations nil) (evidence nil))
    (unless (string= (wire-graph-id graph) (%wire-receipt-graph-id receipt))
      (push (%runtime-violation :verification "$.graph" :graph-id-mismatch
                                "receipt names a different graph") violations))
    (dolist (name (%wire-graph-required-evidence graph))
      (let ((verifier (gethash name (wire-runner-verifiers runner))))
        (if (null verifier)
            (push (%runtime-violation :verification "$.requiredEvidence"
                                      :verifier-unavailable
                                      "verifier ~A is not registered" name)
                  violations)
            (handler-case
                (multiple-value-bind (pass-p value) (funcall verifier graph receipt)
                  (push `(("name" . ,name)
                          ("result" . ,(if pass-p "pass" "fail"))
                          ("value" . ,(%typed-protocol-value value))) evidence)
                  (unless pass-p
                    (push (%runtime-violation :verification
                                              (format nil "$.evidence.~A" name)
                                              :evidence-failed
                                              "verifier returned failure")
                          violations)))
              (error (condition)
                (push (%runtime-violation :verification
                                          (format nil "$.evidence.~A" name)
                                          :verifier-error "verifier signaled ~A"
                                          condition)
                      violations))))))
    (%make-wire-receipt :verification (if violations :fail :pass)
                        (wire-graph-id graph) (nreverse violations)
                        nil nil (nreverse evidence))))

(defun certify-wire-graph (graph runner inputs)
  "Require dependency locks, execute, then independently verify all evidence."
  (let ((validation (validate-wire-graph graph :certify-p t)))
    (when (eq :fail (wire-receipt-verdict validation))
      (return-from certify-wire-graph
        (%make-wire-receipt :certification :fail (wire-graph-id graph)
                            (wire-receipt-violations validation) nil nil nil)))
    (let ((execution (run-wire-graph graph runner inputs)))
      (when (eq :fail (wire-receipt-verdict execution))
        (return-from certify-wire-graph
          (%make-wire-receipt :certification :fail (wire-graph-id graph)
                              (wire-receipt-violations execution)
                              (wire-receipt-step-order execution) nil nil)))
      (let ((verification (verify-wire-receipt graph execution runner)))
        (%make-wire-receipt
         :certification (wire-receipt-verdict verification) (wire-graph-id graph)
         (wire-receipt-violations verification)
         (wire-receipt-step-order execution) (wire-receipt-outputs execution)
         (%wire-receipt-evidence verification))))))
