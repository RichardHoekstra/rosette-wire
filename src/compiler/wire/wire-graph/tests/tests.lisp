;;;; tests.lisp --- executable Wire graph obligations and negative controls.

(defpackage #:wire-graph/tests
  (:use #:cl #:rosette-wire)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:wire-graph/tests)

(defparameter *assertion-count* 0)

(defun expect (condition control &rest arguments)
  (incf *assertion-count*)
  (assert-true condition (apply #'format nil control arguments)))

(defun digest (integer)
  (format nil "sha256:~64,'0x" integer))

(defun violation-code-p (receipt code)
  (some (lambda (violation) (eq code (wire-violation-code violation)))
        (wire-receipt-violations receipt)))

(defun make-fixture (&key (capabilities '("clock/monotonic@1"))
                          (required-evidence '("answer-is-42"))
                          (limits '(("maxSteps" . 2)
                                    ("maxOutputBytes" . 4096)))
                          (consumer-input-type (scalar-type :s64))
                          (second-source :first-step)
                          omit-second-binding
                          duplicate-node)
  (let* ((s64 (scalar-type :s64))
         (ping (make-wire-operation "ping" nil s64))
         (service (make-wire-port "counter" (list ping)))
         (emit (make-wire-operation "emit" nil s64))
         (source-port (make-wire-port "source" (list emit)))
         (double (make-wire-operation
                  "double" (list (make-wire-field "x" consumer-input-type)) s64))
         (math-port (make-wire-port "math" (list double)))
         (producer-descriptor
           (make-wire-descriptor
            :name "org.rosette/source" :version "0.1.0"
            :imports nil :exports (list source-port service)
            :effects '(:pure) :capabilities nil
            :adapter '(("kind" . "test")) :verifiers nil))
         (consumer-descriptor
           (make-wire-descriptor
            :name "org.rosette/double" :version "0.1.0"
            :imports (list service) :exports (list math-port)
            :effects '(:clock) :capabilities '("clock/monotonic@1")
            :adapter '(("kind" . "test"))
            :verifiers '("answer-is-42")))
         (producer (make-wire-node "producer" producer-descriptor (digest 1)
                                   :dependency-set-id (digest 101)))
         (consumer (make-wire-node "consumer" consumer-descriptor (digest 2)
                                   :dependency-set-id (digest 102)))
         (first-step (make-wire-step :id "01-emit" :node-id "producer"
                                     :port "source" :operation "emit"
                                     :bindings nil))
         (source (ecase second-source
                   (:first-step (step-source "01-emit"))
                   (:self (step-source "02-double"))))
         (second-step
           (make-wire-step
            :id "02-double" :node-id "consumer" :port "math"
            :operation "double"
            :bindings (unless omit-second-binding
                        (list (make-data-binding "x" source)))))
         (nodes (if duplicate-node
                    (list producer producer consumer)
                    (list consumer producer))))
    (make-wire-graph
     :name "org.rosette/twice" :nodes nodes
     :services
     (list (make-service-binding
            :id "counter-service" :consumer-node "consumer"
            :import-port "counter" :provider-node "producer"
            :provider-port "counter"))
     :steps (list second-step first-step)
     :inputs nil :outputs (list (make-wire-output "answer" "02-double"))
     :capability-grants capabilities :required-evidence required-evidence
     :limits limits)))

(defun make-runner ()
  (let ((runner (make-wire-runner)))
    (register-wire-handler runner "producer" "source" "emit"
                           (lambda (arguments services context)
                             (declare (ignore arguments services context)) 21))
    (register-wire-handler runner "consumer" "math" "double"
                           (lambda (arguments services context)
                             (declare (ignore services context))
                             (* 2 (cdr (assoc "x" arguments :test #'string=)))))
    (register-wire-verifier
     runner "answer-is-42"
     (lambda (graph receipt)
       (declare (ignore graph))
       (let ((answer (cdr (assoc "answer" (wire-receipt-outputs receipt)
                                 :test #'string=))))
         (values (= answer 42) `(("answer" . ,answer))))))
    runner))

(defun test-canonical-identity ()
  (let ((left '(("z" . 1) ("a" . 2)))
        (right '(("a" . 2) ("z" . 1))))
    (expect (string= (canonical-json left) "{\"a\":2,\"z\":1}")
            "canonical object key order")
    (expect (string= (canonical-id left) (canonical-id right))
            "identity must ignore source key order")
    (expect (handler-case (progn (canonical-json 1.0d0) nil)
              (wire-error (condition)
                (eq :untyped-float (wire-error-code condition))))
            "untyped float must be refused")))

(defun test-descriptor-immutability ()
  (let* ((capability (copy-seq "clock/monotonic@1"))
         (capabilities (list capability))
         (descriptor
           (make-wire-descriptor
            :name "org.rosette/immutable" :version "1.0.0"
            :imports nil :exports nil :effects '(:clock)
            :capabilities capabilities :adapter nil :verifiers nil))
         (identity (wire-contract-id descriptor)))
    (setf (char capability 0) #\X)
    (setf (car capabilities) "mutated")
    (expect (string= identity (wire-contract-id descriptor))
            "caller mutation changed immutable descriptor identity")
    (let ((copy (wire-descriptor-capabilities descriptor)))
      (setf (char (first copy) 0) #\Y)
      (expect (string= identity (wire-contract-id descriptor))
              "reader result mutation changed immutable descriptor identity"))))

(defun test-validation-and-planning ()
  (let* ((graph (make-fixture))
         (receipt (validate-wire-graph graph)))
    (expect (eq :pass (wire-receipt-verdict receipt))
            "valid graph refused: ~S" (mapcar #'wire-violation-code
                                               (wire-receipt-violations receipt)))
    (expect (equal '("01-emit" "02-double")
                   (wire-receipt-step-order receipt))
            "topological order is not deterministic")
    (expect (string= (wire-graph-id graph)
                     (wire-graph-id (make-fixture)))
            "construction order changed graph identity")
    (expect (string= (wire-receipt-id receipt)
                     (wire-receipt-id (validate-wire-graph graph)))
            "validation receipt is not replay-stable")))

(defun test-validation-refusals ()
  (let ((duplicate (validate-wire-graph (make-fixture :duplicate-node t)))
        (missing (validate-wire-graph (make-fixture :omit-second-binding t)))
        (denied (validate-wire-graph (make-fixture :capabilities nil)))
        (mismatch (validate-wire-graph
                   (make-fixture :consumer-input-type (scalar-type :string))))
        (cycle (validate-wire-graph (make-fixture :second-source :self))))
    (expect (violation-code-p duplicate :duplicate-id)
            "duplicate node did not produce deterministic refusal")
    (expect (violation-code-p missing :missing-argument)
            "missing argument did not produce deterministic refusal")
    (expect (violation-code-p denied :capability-denied)
            "missing capability did not produce deterministic refusal")
    (expect (violation-code-p mismatch :type-mismatch)
            "type mismatch did not produce deterministic refusal")
    (expect (violation-code-p cycle :cycle)
            "data cycle did not produce deterministic refusal")))

(defun test-owned-resource-refusal ()
  (let* ((resource (resource-type "buffer" "org.rosette/allocator" :owned))
         (s64 (scalar-type :s64))
         (allocate (make-wire-operation "allocate" nil resource))
         (consume (make-wire-operation
                   "consume" (list (make-wire-field "buffer" resource)) s64))
         (descriptor
           (make-wire-descriptor
            :name "org.rosette/ownership" :version "0.1.0" :imports nil
            :exports (list (make-wire-port "allocator" (list allocate))
                           (make-wire-port "consumer" (list consume)))
            :effects '(:state) :capabilities nil
            :adapter '(("kind" . "test")) :verifiers nil))
         (node (make-wire-node "owner" descriptor (digest 9)))
         (allocation (make-wire-step :id "allocate" :node-id "owner"
                                     :port "allocator" :operation "allocate"
                                     :bindings nil))
         (consume-a (make-wire-step
                     :id "consume-a" :node-id "owner" :port "consumer"
                     :operation "consume"
                     :bindings (list (make-data-binding
                                      "buffer" (step-source "allocate")))))
         (consume-b (make-wire-step
                     :id "consume-b" :node-id "owner" :port "consumer"
                     :operation "consume"
                     :bindings (list (make-data-binding
                                      "buffer" (step-source "allocate")))))
         (graph (make-wire-graph
                 :name "org.rosette/ownership-duplication" :nodes (list node)
                 :services nil :steps (list allocation consume-a consume-b)
                 :inputs nil
                 :outputs (list (make-wire-output "a" "consume-a")
                                (make-wire-output "b" "consume-b"))
                 :capability-grants nil :required-evidence nil :limits nil)))
    (expect (violation-code-p (validate-wire-graph graph)
                              :resource-duplication)
            "owned resource duplication was not refused")))

(defun test-execution-verification-certification ()
  (let* ((graph (make-fixture))
         (runner (make-runner))
         (execution (run-wire-graph graph runner nil))
         (verification (verify-wire-receipt graph execution runner))
         (certification (certify-wire-graph graph runner nil)))
    (expect (eq :pass (wire-receipt-verdict execution))
            "valid deterministic execution failed")
    (expect (equal '("01-emit" "02-double")
                   (wire-receipt-step-order execution))
            "observed step order differs from plan")
    (expect (= 42 (cdr (assoc "answer" (wire-receipt-outputs execution)
                              :test #'string=)))
            "wrong graph output")
    (expect (eq :pass (wire-receipt-verdict verification))
            "independent verification failed")
    (expect (eq :pass (wire-receipt-verdict certification))
            "locked certification failed")
    (let ((bounded (run-wire-graph
                    (make-fixture :limits '(("maxSteps" . 1))) runner nil)))
      (expect (violation-code-p bounded :step-budget)
              "step budget did not stop execution"))))

(defun test-strict-json-round-trips ()
  (let* ((graph (make-fixture))
         (descriptor (wire-node-descriptor (first (wire-graph-nodes graph))))
         (runner (make-runner))
         (receipt (run-wire-graph graph runner nil))
         (decoded-descriptor
           (wire-descriptor-from-json
            (canonical-json (wire-descriptor->value descriptor))))
         (decoded-graph
           (wire-graph-from-json (canonical-json (wire-graph->value graph))))
         (decoded-receipt
           (wire-receipt-from-json (canonical-json (wire-receipt->value receipt)))))
    (expect (string= (wire-contract-id descriptor)
                     (wire-contract-id decoded-descriptor))
            "descriptor JSON round-trip changed identity")
    (expect (string= (wire-graph-id graph) (wire-graph-id decoded-graph))
            "graph JSON round-trip changed identity")
    (expect (string= (wire-receipt-id receipt)
                     (wire-receipt-id decoded-receipt))
            "receipt JSON round-trip changed identity")
    (let ((with-unknown (append (wire-descriptor->value descriptor)
                                '(("unknown" . 1)))))
      (expect (handler-case
                  (progn (wire-descriptor-from-json (canonical-json with-unknown)) nil)
                (wire-error (condition)
                  (eq :unknown-field (wire-error-code condition))))
              "unknown descriptor field was accepted"))
    (let* ((drifted (copy-tree (wire-graph->value graph)))
           (node (first (cdr (assoc "nodes" drifted :test #'string=)))))
      (setf (cdr (assoc "contractId" node :test #'string=)) (digest 999))
      (expect (handler-case
                  (progn (wire-graph-from-json (canonical-json drifted)) nil)
                (wire-error (condition)
                  (eq :contract-id-mismatch (wire-error-code condition))))
              "supplied contract identity drift was accepted"))))

(defun test-six-command-surface ()
  (let* ((graph (make-fixture))
         (runner (make-runner))
         (descriptor (wire-node-descriptor (first (wire-graph-nodes graph))))
         (execution (run-wire-graph graph runner nil))
         (budget-graph (make-fixture :limits '(("maxSteps" . 1))))
         (denied-graph (make-fixture :capabilities nil))
         (documents
           `(("wire.json" . ,(canonical-json (wire-descriptor->value descriptor)))
             ("graph.json" . ,(canonical-json (wire-graph->value graph)))
             ("receipt.json" . ,(canonical-json (wire-receipt->value execution)))
             ("budget.json" . ,(canonical-json (wire-graph->value budget-graph)))
             ("denied.json" . ,(canonical-json (wire-graph->value denied-graph)))
             ("bad.json" . "{")))
         (loader (lambda (path)
                   (or (cdr (assoc path documents :test #'string=))
                       (error 'file-error :pathname path)))))
    (flet ((invoke (arguments &optional (active-runner runner))
             (rosette-command arguments :runner active-runner :loader loader)))
      (multiple-value-bind (code envelope) (invoke '("describe" "wire.json"))
        (expect (zerop code) "describe exit code ~D" code)
        (expect (string= "wire" (cdr (assoc "kind" envelope :test #'string=)))
                "describe did not identify a wire"))
      (multiple-value-bind (code envelope) (invoke '("connect" "graph.json"))
        (expect (zerop code) "connect exit code ~D" code)
        (expect (string= (wire-graph-id graph)
                         (cdr (assoc "id" envelope :test #'string=)))
                "connect changed graph identity"))
      (multiple-value-bind (code envelope) (invoke '("validate" "graph.json"))
        (declare (ignore envelope))
        (expect (zerop code) "validate exit code ~D" code))
      (multiple-value-bind (code envelope) (invoke '("run" "graph.json"))
        (declare (ignore envelope))
        (expect (zerop code) "run exit code ~D" code))
      (multiple-value-bind (code envelope)
          (invoke '("verify" "receipt.json" "--graph" "graph.json"))
        (declare (ignore envelope))
        (expect (zerop code) "verify exit code ~D" code))
      (multiple-value-bind (code envelope) (invoke '("certify" "graph.json"))
        (declare (ignore envelope))
        (expect (zerop code) "certify exit code ~D" code))
      (multiple-value-bind (code envelope)
          (invoke '("run" "graph.json") (make-wire-runner))
        (declare (ignore envelope))
        (expect (= code 3) "missing handler must exit unavailable (3), got ~D" code))
      (multiple-value-bind (code envelope) (invoke '("run" "budget.json"))
        (declare (ignore envelope))
        (expect (= code 4) "budget exhaustion must exit 4, got ~D" code))
      (multiple-value-bind (code envelope) (invoke '("validate" "denied.json"))
        (declare (ignore envelope))
        (expect (= code 3) "capability denial must exit unavailable (3), got ~D" code))
      (let ((output (make-string-output-stream)))
        (let ((code (rosette-main :arguments '("describe" "bad.json")
                                  :runner runner :loader loader
                                  :output-stream output)))
          (expect (= code 2) "malformed JSON must exit 2, got ~D" code)
          (let ((value (rosette-json:json-parse
                        (get-output-stream-string output))))
            (expect (string= "error" (cdr (assoc "kind" value :test #'string=)))
                    "malformed JSON did not emit a machine error")))))))

(defun test-canonical-semantic-grammar ()
  (let* ((s64 (scalar-type :s64))
         (operation (make-component-operation "emit" nil s64))
         (port (make-port "source" (list operation)))
         (descriptor
           (make-component-descriptor
            :name "org.rosette/component" :version "1.0.0"
            :imports nil :exports (list port) :effects '(:pure)
            :capabilities nil :adapter nil :verifiers nil)))
    (expect (component-operation-p operation)
            "canonical operation does not inhabit compatibility type")
    (expect (port-p port) "canonical Port does not inhabit compatibility type")
    (expect (component-descriptor-p descriptor)
            "canonical Component descriptor was not constructed")
    (expect (string= (component-contract-id descriptor)
                     (wire-contract-id descriptor))
            "semantic alias changed a v0 contract identity")
    (expect (string= +component-schema+ +wire-schema+)
            "compatibility Component schema drifted"))
  (let* ((composition (make-fixture))
         (runner (make-runner))
         (receipt (run-composition composition runner nil)))
    (expect (composition-p composition) "fixture is not a Composition")
    (expect (string= (composition-id composition) (wire-graph-id composition))
            "semantic alias changed a v0 Composition identity")
    (expect (eq :pass (wire-receipt-verdict receipt))
            "canonical Composition runner failed"))
  (let* ((cell (make-cell :tag "result" :media-type "application/octet-stream"
                          :payload #(4 2)))
         (encoded (encode-frame (make-cell-frame cell :stream-id 1 :sequence 0)))
         (decoder (make-wire-decoder :max-feed-bytes (length encoded)))
         (frames (feed-wire-decoder decoder encoded)))
    (expect (= 1 (length frames)) "rosette-wire did not expose Wire framing")
    (expect (string= (cell-id cell)
                     (cell-id (decode-cell (frame-payload (first frames)))))
            "Cell identity changed across the integrated Wire surface")))

(defun run-all-tests ()
  (setf *assertion-count* 0)
  (test-canonical-identity)
  (test-descriptor-immutability)
  (test-validation-and-planning)
  (test-validation-refusals)
  (test-owned-resource-refusal)
  (test-execution-verification-certification)
  (test-strict-json-round-trips)
  (test-six-command-surface)
  (test-canonical-semantic-grammar)
  (format t "wire-graph: ~D assertions passed~%" *assertion-count*)
  t)
