;;;; graph.lisp --- immutable graph model, validation, and receipts.

(in-package #:rosette-wire)

(defun %digest-p (value)
  (and (stringp value) (= (length value) 71)
       (string= value "sha256:" :end1 7 :end2 7)
       (every (lambda (character) (digit-char-p character 16))
              (subseq value 7))
       (string= (subseq value 7) (string-downcase (subseq value 7)))))

(defun %require-digest (value path)
  (unless (%digest-p value)
    (%wire-error :invalid-digest path "expected sha256:<64 lowercase hex>"))
  (copy-seq value))

(defstruct (wire-node
             (:constructor %make-wire-node
                 (id descriptor contract-id implementation-id dependency-set-id))
             (:conc-name %wire-node-))
  (id "" :type string :read-only t)
  (descriptor nil :type wire-descriptor :read-only t)
  (contract-id "" :type string :read-only t)
  (implementation-id "" :type string :read-only t)
  (dependency-set-id nil :type (or null string) :read-only t))

(defun make-wire-node (id descriptor implementation-id &key dependency-set-id)
  (check-type descriptor wire-descriptor)
  (%make-wire-node (%require-name id "$.node.id") descriptor
                   (wire-contract-id descriptor)
                   (%require-digest implementation-id "$.node.implementationId")
                   (when dependency-set-id
                     (%require-digest dependency-set-id
                                      "$.node.dependencySetId"))))

(defun wire-node-id (node) (copy-seq (%wire-node-id node)))
(defun wire-node-descriptor (node) (%wire-node-descriptor node))

(defstruct (service-binding
             (:constructor %make-service-binding
                 (id consumer-node import-port provider-node provider-port host-port))
             (:conc-name %service-binding-))
  (id "" :type string :read-only t)
  (consumer-node "" :type string :read-only t)
  (import-port "" :type string :read-only t)
  (provider-node nil :type (or null string) :read-only t)
  (provider-port nil :type (or null string) :read-only t)
  (host-port nil :type (or null wire-port) :read-only t))

(defun make-service-binding (&key id consumer-node import-port
                                  provider-node provider-port host-port)
  (unless (or (and provider-node provider-port (null host-port))
              (and host-port (null provider-node) (null provider-port)))
    (%wire-error :invalid-service-binding "$.serviceBinding"
                 "choose exactly one provider node/port pair or host port"))
  (when host-port (check-type host-port wire-port))
  (%make-service-binding
   (%require-name id "$.serviceBinding.id")
   (%require-name consumer-node "$.serviceBinding.consumerNode")
   (%require-name import-port "$.serviceBinding.importPort")
   (when provider-node (%require-name provider-node "$.serviceBinding.providerNode"))
   (when provider-port (%require-name provider-port "$.serviceBinding.providerPort"))
   host-port))

(defstruct (data-source
             (:constructor %make-data-source (kind reference value type))
             (:conc-name %data-source-))
  (kind nil :read-only t)
  (reference nil :type (or null string) :read-only t)
  (value nil :read-only t)
  (type nil :type (or null wire-type) :read-only t))

(defun input-source (input-name)
  (%make-data-source :input (%require-name input-name "$.source.input") nil nil))

(defun literal-source (value type)
  (check-type type wire-type)
  (%make-data-source :literal nil (%freeze-value value) type))

(defun step-source (step-id)
  (%make-data-source :step (%require-name step-id "$.source.step") nil nil))

(defstruct (data-binding
             (:constructor %make-data-binding (argument source))
             (:conc-name %data-binding-))
  (argument "" :type string :read-only t)
  (source nil :type data-source :read-only t))

(defun make-data-binding (argument source)
  (check-type source data-source)
  (%make-data-binding (%require-name argument "$.binding.argument") source))

(defstruct (wire-step
             (:constructor %make-wire-step (id node-id port operation bindings))
             (:conc-name %wire-step-))
  (id "" :type string :read-only t)
  (node-id "" :type string :read-only t)
  (port "" :type string :read-only t)
  (operation "" :type string :read-only t)
  (bindings nil :type list :read-only t))

(defun make-wire-step (&key id node-id port operation bindings)
  (%require-list bindings "$.step.bindings")
  (dolist (binding bindings) (check-type binding data-binding))
  (%make-wire-step (%require-name id "$.step.id")
                   (%require-name node-id "$.step.node")
                   (%require-name port "$.step.port")
                   (%require-name operation "$.step.operation")
                   (copy-list bindings)))

(defun wire-step-id (step) (copy-seq (%wire-step-id step)))

(defstruct (wire-output
             (:constructor %make-wire-output (name step-id))
             (:conc-name %wire-output-))
  (name "" :type string :read-only t)
  (step-id "" :type string :read-only t))

(defun make-wire-output (name step-id)
  (%make-wire-output (%require-name name "$.output.name")
                     (%require-name step-id "$.output.step")))

(defstruct (wire-graph
             (:constructor %make-wire-graph
                 (name nodes services steps inputs outputs capability-grants
                  required-evidence limits))
             (:conc-name %wire-graph-))
  (name "" :type string :read-only t)
  (nodes nil :type list :read-only t)
  (services nil :type list :read-only t)
  (steps nil :type list :read-only t)
  (inputs nil :type list :read-only t)
  (outputs nil :type list :read-only t)
  (capability-grants nil :type list :read-only t)
  (required-evidence nil :type list :read-only t)
  (limits nil :read-only t))

(defun make-wire-graph (&key name nodes services steps inputs outputs
                             capability-grants required-evidence limits)
  (dolist (pair `((,nodes wire-node "$.nodes")
                  (,services service-binding "$.serviceBindings")
                  (,steps wire-step "$.steps")
                  (,inputs wire-field "$.inputs")
                  (,outputs wire-output "$.outputs")))
    (%require-list (first pair) (third pair))
    (dolist (item (first pair))
      (unless (typep item (second pair))
        (%wire-error :invalid-graph-item (third pair)
                     "expected ~S, got ~S" (second pair) (type-of item)))))
  (%require-list capability-grants "$.capabilityGrants")
  (%require-list required-evidence "$.requiredEvidence")
  (dolist (grant capability-grants) (%require-name grant "$.capabilityGrants"))
  (dolist (evidence required-evidence) (%require-name evidence "$.requiredEvidence"))
  (%make-wire-graph
   (%require-name name "$.name")
   (sort (copy-list nodes) #'string< :key #'%wire-node-id)
   (sort (copy-list services) #'string< :key #'%service-binding-id)
   (sort (copy-list steps) #'string< :key #'%wire-step-id)
   (sort (copy-list inputs) #'string< :key #'%wire-field-name)
   (sort (copy-list outputs) #'string< :key #'%wire-output-name)
   (sort (remove-duplicates (mapcar #'copy-seq capability-grants)
                            :test #'string=) #'string<)
   (sort (remove-duplicates (mapcar #'copy-seq required-evidence)
                            :test #'string=) #'string<)
   (%freeze-value (or limits :empty-object))))

(defun wire-graph-name (graph) (copy-seq (%wire-graph-name graph)))
(defun wire-graph-nodes (graph) (copy-list (%wire-graph-nodes graph)))
(defun wire-graph-steps (graph) (copy-list (%wire-graph-steps graph)))

(defun %source->value (source)
  (ecase (%data-source-kind source)
    (:input `(("input" . ,(%data-source-reference source)) ("kind" . "input")))
    (:step `(("kind" . "step") ("step" . ,(%data-source-reference source))))
    (:literal `(("kind" . "literal")
                ("type" . ,(wire-type->value (%data-source-type source)))
                ("value" . ,(%typed-protocol-value
                              (%data-source-value source)))))))

(defun %node->value (node)
  `(("contract" . ,(wire-descriptor->value (%wire-node-descriptor node)))
    ("contractId" . ,(%wire-node-contract-id node))
    ("dependencySetId" . ,(or (%wire-node-dependency-set-id node) json-null))
    ("id" . ,(%wire-node-id node))
    ("implementationId" . ,(%wire-node-implementation-id node))))

(defun %service->value (service)
  `(("consumer" . (("node" . ,(%service-binding-consumer-node service))
                    ("port" . ,(%service-binding-import-port service))))
    ("host" . ,(if (%service-binding-host-port service)
                    (%port->value (%service-binding-host-port service)) json-null))
    ("id" . ,(%service-binding-id service))
    ("provider" . ,(if (%service-binding-provider-node service)
                        `(("node" . ,(%service-binding-provider-node service))
                          ("port" . ,(%service-binding-provider-port service)))
                        json-null))))

(defun %step->value (step)
  `(("bindings" .
     ,(mapcar (lambda (binding)
                `(("argument" . ,(%data-binding-argument binding))
                  ("source" . ,(%source->value (%data-binding-source binding)))))
              (sort (copy-list (%wire-step-bindings step)) #'string<
                    :key #'%data-binding-argument)))
    ("id" . ,(%wire-step-id step))
    ("node" . ,(%wire-step-node-id step))
    ("operation" . ,(%wire-step-operation step))
    ("port" . ,(%wire-step-port step))))

(defun wire-graph->value (graph)
  (check-type graph wire-graph)
  `(("capabilityGrants" . ,(mapcar #'copy-seq
                                    (%wire-graph-capability-grants graph)))
    ("inputs" . ,(mapcar #'%field->value (%wire-graph-inputs graph)))
    ("limits" . ,(%freeze-value (%wire-graph-limits graph)))
    ("name" . ,(%wire-graph-name graph))
    ("nodes" . ,(mapcar #'%node->value (%wire-graph-nodes graph)))
    ("outputs" . ,(mapcar (lambda (output)
                            `(("name" . ,(%wire-output-name output))
                              ("step" . ,(%wire-output-step-id output))))
                          (%wire-graph-outputs graph)))
    ("requiredEvidence" . ,(mapcar #'copy-seq
                                    (%wire-graph-required-evidence graph)))
    ("schema" . ,+graph-schema+)
    ("serviceBindings" . ,(mapcar #'%service->value
                                   (%wire-graph-services graph)))
    ("steps" . ,(mapcar #'%step->value (%wire-graph-steps graph)))))

(defun wire-graph-id (graph) (canonical-id (wire-graph->value graph)))

(defstruct (wire-violation
             (:constructor %make-wire-violation (stage path code detail))
             (:conc-name %wire-violation-))
  (stage nil :read-only t)
  (path "" :type string :read-only t)
  (code nil :read-only t)
  (detail "" :type string :read-only t))

(defun wire-violation-stage (violation) (%wire-violation-stage violation))
(defun wire-violation-path (violation) (copy-seq (%wire-violation-path violation)))
(defun wire-violation-code (violation) (%wire-violation-code violation))
(defun wire-violation-detail (violation) (copy-seq (%wire-violation-detail violation)))

(defstruct (wire-receipt
             (:constructor %make-wire-receipt
                 (kind verdict graph-id violations step-order outputs evidence))
             (:conc-name %wire-receipt-))
  (kind nil :read-only t)
  (verdict nil :read-only t)
  (graph-id "" :type string :read-only t)
  (violations nil :type list :read-only t)
  (step-order nil :type list :read-only t)
  (outputs nil :read-only t)
  (evidence nil :read-only t))

(defun wire-receipt-kind (receipt) (%wire-receipt-kind receipt))
(defun wire-receipt-verdict (receipt) (%wire-receipt-verdict receipt))
(defun wire-receipt-graph-id (receipt) (copy-seq (%wire-receipt-graph-id receipt)))
(defun wire-receipt-violations (receipt) (copy-list (%wire-receipt-violations receipt)))
(defun wire-receipt-step-order (receipt) (mapcar #'copy-seq (%wire-receipt-step-order receipt)))
(defun wire-receipt-outputs (receipt) (%freeze-value (%wire-receipt-outputs receipt)))

(defun %violation->value (violation)
  `(("code" . ,(%enum-name (%wire-violation-code violation)))
    ("detail" . ,(%wire-violation-detail violation))
    ("path" . ,(%wire-violation-path violation))
    ("stage" . ,(%enum-name (%wire-violation-stage violation)))))

(defun wire-receipt->value (receipt)
  (check-type receipt wire-receipt)
  `(("evidence" . ,(%freeze-value (%wire-receipt-evidence receipt)))
    ("graph" . (("digest" . ,(%wire-receipt-graph-id receipt))))
    ("kind" . ,(%enum-name (%wire-receipt-kind receipt)))
    ("outputs" . ,(%freeze-value (%wire-receipt-outputs receipt)))
    ("schema" . ,+receipt-schema+)
    ("stepOrder" . ,(mapcar #'copy-seq (%wire-receipt-step-order receipt)))
    ("verdict" . ,(%enum-name (%wire-receipt-verdict receipt)))
    ("violations" . ,(mapcar #'%violation->value
                              (%wire-receipt-violations receipt)))))

(defun wire-receipt-id (receipt) (canonical-id (wire-receipt->value receipt)))

(defun %lookup (name items key)
  (find name items :test #'string= :key key))

(defun %node (graph id) (%lookup id (%wire-graph-nodes graph) #'%wire-node-id))
(defun %step (graph id) (%lookup id (%wire-graph-steps graph) #'%wire-step-id))
(defun %port (name ports) (%lookup name ports #'%wire-port-name))
(defun %operation (name port)
  (and port (%lookup name (%wire-port-operations port) #'%wire-operation-name)))

(defun %step-operation-object (graph step)
  (let* ((node (%node graph (%wire-step-node-id step)))
         (port (and node (%port (%wire-step-port step)
                                (%wire-descriptor-exports
                                 (%wire-node-descriptor node))))))
    (%operation (%wire-step-operation step) port)))

(defun %type= (left right)
  (equal (wire-type->value left) (wire-type->value right)))

(defun %contains-owned-resource-p (type)
  (let ((kind (%wire-type-kind type)) (payload (%wire-type-payload type)))
    (case kind
      (:resource (eq :owned (third payload)))
      ((:list :option) (%contains-owned-resource-p payload))
      (:result (some #'%contains-owned-resource-p payload))
      (:tuple (some #'%contains-owned-resource-p payload))
      ((:record :variant)
       (some (lambda (field)
               (%contains-owned-resource-p (%wire-field-type field))) payload))
      (otherwise nil))))

(defun %port-compatible-p (consumer provider)
  (equal (mapcar #'%operation->value (%wire-port-operations consumer))
         (mapcar #'%operation->value (%wire-port-operations provider))))

(defun %duplicates (items key)
  (let ((seen (make-hash-table :test #'equal)) (duplicates nil))
    (dolist (item items)
      (let ((name (funcall key item)))
        (if (gethash name seen) (pushnew name duplicates :test #'equal)
            (setf (gethash name seen) t))))
    (sort duplicates #'string<)))

(defun %dependency-edges (graph)
  (loop for target in (%wire-graph-steps graph)
        append (loop for binding in (%wire-step-bindings target)
                     for source = (%data-binding-source binding)
                     when (eq (%data-source-kind source) :step)
                       collect (cons (%data-source-reference source)
                                     (%wire-step-id target)))))

(defun %topological-plan (graph)
  (let* ((steps (%wire-graph-steps graph))
         (index (loop for step in steps for i from 0
                      collect (cons (%wire-step-id step) i)))
         (edges (loop for (source . target) in (%dependency-edges graph)
                      for source-index = (cdr (assoc source index :test #'string=))
                      for target-index = (cdr (assoc target index :test #'string=))
                      when (and source-index target-index)
                        collect (cons source-index target-index))))
    (multiple-value-bind (order cycle-p)
        (topological-sort (csr-from-edges (length steps) edges :directed-p t))
      (values (mapcar (lambda (i) (%wire-step-id (nth i steps))) order)
              cycle-p))))

(defun validate-wire-graph (graph &key certify-p)
  "Return a deterministic validation receipt; no component code is executed."
  (check-type graph wire-graph)
  (let ((violations nil))
    (flet ((refuse (stage path code control &rest arguments)
             (push (%make-wire-violation
                    stage path code (apply #'format nil control arguments))
                   violations)))
      ;; Identifier uniqueness.
      (dolist (spec `((,(%wire-graph-nodes graph) ,#'%wire-node-id "$.nodes")
                      (,(%wire-graph-services graph) ,#'%service-binding-id
                       "$.serviceBindings")
                      (,(%wire-graph-steps graph) ,#'%wire-step-id "$.steps")
                      (,(%wire-graph-inputs graph) ,#'%wire-field-name "$.inputs")
                      (,(%wire-graph-outputs graph) ,#'%wire-output-name "$.outputs")))
        (dolist (name (%duplicates (first spec) (second spec)))
          (refuse :schema (third spec) :duplicate-id "duplicate identifier ~A" name)))
      ;; Node identities and certification locks.
      (dolist (node (%wire-graph-nodes graph))
        (unless (string= (%wire-node-contract-id node)
                         (wire-contract-id (%wire-node-descriptor node)))
          (refuse :content (format nil "$.nodes.~A" (%wire-node-id node))
                  :contract-id-mismatch "embedded descriptor identity drifted"))
        (when (and certify-p (null (%wire-node-dependency-set-id node)))
          (refuse :lock (format nil "$.nodes.~A.dependencySetId"
                                (%wire-node-id node))
                  :missing-lock "certification requires an exact dependency set")))
      ;; Every import is closed exactly once and is structurally compatible.
      (dolist (node (%wire-graph-nodes graph))
        (dolist (import (%wire-descriptor-imports (%wire-node-descriptor node)))
          (let ((matches
                  (remove-if-not
                   (lambda (binding)
                     (and (string= (%service-binding-consumer-node binding)
                                   (%wire-node-id node))
                          (string= (%service-binding-import-port binding)
                                   (%wire-port-name import))))
                   (%wire-graph-services graph))))
            (unless (= (length matches) 1)
              (refuse :services
                      (format nil "$.nodes.~A.imports.~A"
                              (%wire-node-id node) (%wire-port-name import))
                      :service-cardinality "expected one binding, found ~D"
                      (length matches)))
            (when (= (length matches) 1)
              (let* ((binding (first matches))
                     (provider-node (and (%service-binding-provider-node binding)
                                         (%node graph
                                                (%service-binding-provider-node binding))))
                     (provider-port
                       (if provider-node
                           (%port (%service-binding-provider-port binding)
                                  (%wire-descriptor-exports
                                   (%wire-node-descriptor provider-node)))
                           (%service-binding-host-port binding))))
                (unless provider-port
                  (refuse :services
                          (format nil "$.serviceBindings.~A"
                                  (%service-binding-id binding))
                          :missing-provider "provider node or export is unavailable"))
                (when (and provider-port (not (%port-compatible-p import provider-port)))
                  (refuse :services
                          (format nil "$.serviceBindings.~A"
                                  (%service-binding-id binding))
                          :incompatible-port "import and export contracts differ")))))))
      ;; Step target, argument cardinality, source existence, and type.
      (dolist (step (%wire-graph-steps graph))
        (let* ((operation (%step-operation-object graph step))
               (path (format nil "$.steps.~A" (%wire-step-id step))))
          (unless operation
            (refuse :steps path :missing-operation
                    "node/export/operation target is unavailable"))
          (when operation
            (dolist (duplicate (%duplicates (%wire-step-bindings step)
                                            #'%data-binding-argument))
              (refuse :steps path :duplicate-argument
                      "argument ~A is bound more than once" duplicate))
            (dolist (input (%wire-operation-inputs operation))
              (let ((binding (%lookup (%wire-field-name input)
                                      (%wire-step-bindings step)
                                      #'%data-binding-argument)))
                (unless binding
                  (refuse :steps path :missing-argument "argument ~A is unbound"
                          (%wire-field-name input)))
                (when binding
                  (let* ((source (%data-binding-source binding))
                         (source-type
                           (ecase (%data-source-kind source)
                             (:input
                              (let ((field (%lookup (%data-source-reference source)
                                                    (%wire-graph-inputs graph)
                                                    #'%wire-field-name)))
                                (if field (%wire-field-type field)
                                    (progn
                                      (refuse :data path :missing-input
                                              "graph input ~A is unavailable"
                                              (%data-source-reference source))
                                      nil))))
                             (:literal (%data-source-type source))
                             (:step
                              (let* ((source-step (%step graph
                                                        (%data-source-reference source)))
                                     (source-operation
                                       (and source-step
                                            (%step-operation-object graph source-step))))
                                (if source-operation
                                    (%wire-operation-output source-operation)
                                    (progn
                                      (refuse :data path :missing-step-source
                                              "step source ~A is unavailable"
                                              (%data-source-reference source))
                                      nil)))))))
                    (when (and source-type
                               (not (%type= source-type (%wire-field-type input))))
                      (refuse :data path :type-mismatch
                              "argument ~A source type differs from its contract"
                              (%wire-field-name input)))))))
            (dolist (binding (%wire-step-bindings step))
              (unless (%lookup (%data-binding-argument binding)
                               (%wire-operation-inputs operation)
                               #'%wire-field-name)
                (refuse :steps path :unknown-argument "unknown argument ~A"
                        (%data-binding-argument binding)))))))
      ;; Output projections.
      (dolist (output (%wire-graph-outputs graph))
        (unless (%step graph (%wire-output-step-id output))
          (refuse :outputs (format nil "$.outputs.~A" (%wire-output-name output))
                  :missing-output-step "step ~A is unavailable"
                  (%wire-output-step-id output))))
      ;; Owned values cross exactly one boundary: one downstream argument or
      ;; one graph output. Anything else is duplication or loss.
      (dolist (step (%wire-graph-steps graph))
        (let ((operation (%step-operation-object graph step)))
          (when (and operation
                     (%contains-owned-resource-p
                      (%wire-operation-output operation)))
            (let ((uses
                    (+ (count (%wire-step-id step) (%dependency-edges graph) :test #'string=
                              :key #'car)
                       (count (%wire-step-id step) (%wire-graph-outputs graph) :test #'string=
                              :key #'%wire-output-step-id))))
              (cond ((zerop uses)
                     (refuse :ownership
                             (format nil "$.steps.~A.output" (%wire-step-id step))
                             :resource-loss "owned result has no consuming boundary"))
                    ((> uses 1)
                     (refuse :ownership
                             (format nil "$.steps.~A.output" (%wire-step-id step))
                             :resource-duplication
                             "owned result crosses ~D consuming boundaries" uses)))))))
      (dolist (input (%wire-graph-inputs graph))
        (when (%contains-owned-resource-p (%wire-field-type input))
          (let ((uses
                  (loop for step in (%wire-graph-steps graph)
                        sum (count (%wire-field-name input) (%wire-step-bindings step) :test #'string=
                                   :key (lambda (binding)
                                          (let ((source (%data-binding-source binding)))
                                            (and (eq :input (%data-source-kind source))
                                                 (%data-source-reference source))))))))
            (cond ((zerop uses)
                   (refuse :ownership
                           (format nil "$.inputs.~A" (%wire-field-name input))
                           :resource-loss "owned input is never transferred"))
                  ((> uses 1)
                   (refuse :ownership
                           (format nil "$.inputs.~A" (%wire-field-name input))
                           :resource-duplication
                           "owned input is transferred ~D times" uses))))))
      (dolist (step (%wire-graph-steps graph))
        (dolist (binding (%wire-step-bindings step))
          (let ((source (%data-binding-source binding)))
            (when (and (eq :literal (%data-source-kind source))
                       (%contains-owned-resource-p (%data-source-type source)))
              (refuse :ownership
                      (format nil "$.steps.~A.bindings.~A"
                              (%wire-step-id step) (%data-binding-argument binding))
                      :owned-literal
                      "an owned resource cannot originate as a literal")))))
      ;; Exact capability grants and evidence availability.
      (dolist (node (%wire-graph-nodes graph))
        (dolist (capability
                  (%wire-descriptor-capabilities (%wire-node-descriptor node)))
          (unless (member capability (%wire-graph-capability-grants graph)
                          :test #'string=)
            (refuse :capabilities
                    (format nil "$.nodes.~A.capabilities" (%wire-node-id node))
                    :capability-denied "capability ~A is not granted" capability))))
      (let ((available (loop for node in (%wire-graph-nodes graph)
                             append (%wire-descriptor-verifiers
                                     (%wire-node-descriptor node)))))
        (dolist (required (%wire-graph-required-evidence graph))
          (unless (member required available :test #'string=)
            (refuse :evidence "$.requiredEvidence" :missing-verifier
                    "verifier ~A is unavailable" required))))
      ;; Schedule and cycle obstruction.
      (multiple-value-bind (order cycle-p) (%topological-plan graph)
        (when cycle-p
          (refuse :schedule "$.steps" :cycle
                  "data dependencies contain an unbounded cycle"))
        (setf violations (nreverse violations))
        (%make-wire-receipt :validation (if violations :fail :pass)
                            (wire-graph-id graph) violations
                            (if cycle-p nil order) nil nil)))))

(defun plan-wire-graph (graph)
  "Return (values STEP-IDS VALIDATION-RECEIPT)."
  (let ((receipt (validate-wire-graph graph)))
    (values (when (eq :pass (wire-receipt-verdict receipt))
              (wire-receipt-step-order receipt))
            receipt)))
