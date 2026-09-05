;;;; decode.lisp --- strict JSON-value decoding for the public Wire protocol.

(in-package #:rosette-wire)

(defun %decode-object (value path)
  (cond ((eq value :empty-object) nil)
        ((%object-p value) value)
        (t (%wire-error :expected-object path "expected a JSON object"))))

(defun %decode-fields (value path allowed required)
  (let ((object (%decode-object value path)))
    (dolist (duplicate (%duplicates object #'car))
      (%wire-error :duplicate-field path "duplicate object field ~S" duplicate))
    (dolist (entry object)
      (unless (member (car entry) allowed :test #'string=)
        (%wire-error :unknown-field (format nil "~A.~A" path (car entry))
                     "unknown field")))
    (dolist (name required)
      (unless (assoc name object :test #'string=)
        (%wire-error :missing-field (format nil "~A.~A" path name)
                     "required field is absent")))
    object))

(defun %decode-ref (object name)
  (cdr (assoc name object :test #'string=)))

(defun %decode-string (value path)
  (unless (stringp value)
    (%wire-error :expected-string path "expected a JSON string"))
  value)

(defun %decode-array (value path)
  (unless (and (listp value) (not (%object-p value)))
    (%wire-error :expected-array path "expected a JSON array"))
  value)

(defun %decode-string-array (value path)
  (loop for item in (%decode-array value path)
        for index from 0
        collect (%decode-string item (format nil "~A[~D]" path index))))

(defun %decode-keyword (value path)
  (intern (string-upcase (%decode-string value path)) :keyword))

(defun %type-from-value (value path)
  (let* ((base (%decode-fields value path
                               '("kind" "name" "element" "ok" "error"
                                 "elements" "fields" "cases" "dtype"
                                 "rank" "dimensions" "layout" "device"
                                 "mutability" "owner" "ownership")
                               '("kind")))
         (kind (%decode-string (%decode-ref base "kind")
                               (format nil "~A.kind" path))))
    (cond
      ((string= kind "scalar")
       (%decode-fields value path '("kind" "name") '("kind" "name"))
       (scalar-type (%decode-ref base "name")))
      ((member kind '("list" "option") :test #'string=)
       (%decode-fields value path '("kind" "element") '("kind" "element"))
       (funcall (if (string= kind "list") #'list-type #'option-type)
                (%type-from-value (%decode-ref base "element")
                                  (format nil "~A.element" path))))
      ((string= kind "result")
       (%decode-fields value path '("kind" "ok" "error")
                       '("kind" "ok" "error"))
       (result-type (%type-from-value (%decode-ref base "ok")
                                     (format nil "~A.ok" path))
                    (%type-from-value (%decode-ref base "error")
                                     (format nil "~A.error" path))))
      ((string= kind "tuple")
       (%decode-fields value path '("kind" "elements") '("kind" "elements"))
       (apply #'tuple-type
              (loop for item in (%decode-array (%decode-ref base "elements")
                                               (format nil "~A.elements" path))
                    for index from 0
                    collect (%type-from-value item
                                              (format nil "~A.elements[~D]"
                                                      path index)))))
      ((member kind '("record" "variant") :test #'string=)
       (let ((key (if (string= kind "record") "fields" "cases")))
         (%decode-fields value path (list "kind" key) (list "kind" key))
         (apply (if (string= kind "record") #'record-type #'variant-type)
                (loop for item in (%decode-array (%decode-ref base key)
                                                 (format nil "~A.~A" path key))
                      for index from 0
                      collect (%field-from-value item
                                                 (format nil "~A.~A[~D]"
                                                         path key index))))))
      ((string= kind "tensor")
       (let* ((fields (%decode-fields
                       value path
                       '("kind" "dtype" "rank" "dimensions" "layout"
                         "device" "mutability")
                       '("kind" "dtype" "rank" "dimensions" "layout"
                         "device" "mutability")))
              (dimensions
                (loop for dimension in
                      (%decode-array (%decode-ref fields "dimensions")
                                     (format nil "~A.dimensions" path))
                      collect (if (and (stringp dimension)
                                       (string= dimension "dynamic"))
                                  :dynamic dimension)))
              (rank (%decode-ref fields "rank")))
         (unless (and (integerp rank) (= rank (length dimensions)))
           (%wire-error :rank-mismatch (format nil "~A.rank" path)
                        "rank does not equal dimension count"))
         (tensor-type (%decode-ref fields "dtype") dimensions
                      :layout (%decode-ref fields "layout")
                      :device (%decode-ref fields "device")
                      :mutability (%decode-ref fields "mutability"))))
      ((string= kind "resource")
       (let ((fields (%decode-fields value path
                                     '("kind" "name" "owner" "ownership")
                                     '("kind" "name" "owner" "ownership"))))
         (resource-type (%decode-ref fields "name")
                        (%decode-ref fields "owner")
                        (%decode-ref fields "ownership"))))
      (t (%wire-error :unknown-type-kind (format nil "~A.kind" path)
                      "unknown type kind ~S" kind)))))

(defun %field-from-value (value path)
  (let ((object (%decode-fields value path '("name" "type") '("name" "type"))))
    (make-wire-field (%decode-string (%decode-ref object "name")
                                    (format nil "~A.name" path))
                     (%type-from-value (%decode-ref object "type")
                                       (format nil "~A.type" path)))))

(defun %operation-from-value (value path)
  (let ((object (%decode-fields value path '("name" "inputs" "output")
                                '("name" "inputs" "output"))))
    (make-wire-operation
     (%decode-ref object "name")
     (loop for input in (%decode-array (%decode-ref object "inputs")
                                      (format nil "~A.inputs" path))
           for index from 0
           collect (%field-from-value input (format nil "~A.inputs[~D]" path index)))
     (%type-from-value (%decode-ref object "output")
                       (format nil "~A.output" path)))))

(defun %port-from-value (value path)
  (let ((object (%decode-fields value path '("name" "operations")
                                '("name" "operations"))))
    (make-wire-port
     (%decode-ref object "name")
     (loop for operation in (%decode-array (%decode-ref object "operations")
                                          (format nil "~A.operations" path))
           for index from 0
           collect (%operation-from-value
                    operation (format nil "~A.operations[~D]" path index))))))

(defun wire-descriptor-from-value (value)
  (let ((object (%decode-fields
                 value "$" '("schema" "name" "version" "imports" "exports"
                              "effects" "capabilities" "adapter" "verifiers")
                 '("schema" "name" "version" "imports" "exports" "effects"
                   "capabilities" "adapter" "verifiers"))))
    (unless (string= (%decode-string (%decode-ref object "schema") "$.schema")
                     +wire-schema+)
      (%wire-error :schema-mismatch "$.schema" "unsupported Wire schema"))
    (flet ((ports (name)
             (loop for port in (%decode-array (%decode-ref object name)
                                              (format nil "$.~A" name))
                   for index from 0
                   collect (%port-from-value port
                                             (format nil "$.~A[~D]" name index)))))
      (make-wire-descriptor
       :name (%decode-ref object "name") :version (%decode-ref object "version")
       :imports (ports "imports") :exports (ports "exports")
       :effects (%decode-string-array (%decode-ref object "effects") "$.effects")
       :capabilities (%decode-string-array (%decode-ref object "capabilities")
                                           "$.capabilities")
       :adapter (%decode-ref object "adapter")
       :verifiers (%decode-string-array (%decode-ref object "verifiers")
                                        "$.verifiers")))))

(defun wire-descriptor-from-json (text)
  (wire-descriptor-from-value (json-parse text)))

(defun %node-from-value (value path)
  (let* ((object (%decode-fields
                  value path '("id" "contract" "contractId" "implementationId"
                               "dependencySetId")
                  '("id" "contract" "contractId" "implementationId"
                    "dependencySetId")))
         (descriptor (wire-descriptor-from-value (%decode-ref object "contract")))
         (dependency (%decode-ref object "dependencySetId"))
         (node (make-wire-node (%decode-ref object "id") descriptor
                               (%decode-ref object "implementationId")
                               :dependency-set-id
                               (unless (eq dependency json-null) dependency))))
    (unless (string= (%wire-node-contract-id node)
                     (%decode-string (%decode-ref object "contractId")
                                     (format nil "~A.contractId" path)))
      (%wire-error :contract-id-mismatch (format nil "~A.contractId" path)
                   "supplied identity does not match embedded contract"))
    node))

(defun %service-from-value (value path)
  (let* ((object (%decode-fields value path '("id" "consumer" "provider" "host")
                                '("id" "consumer" "provider" "host")))
         (consumer (%decode-fields (%decode-ref object "consumer")
                                   (format nil "~A.consumer" path)
                                   '("node" "port") '("node" "port")))
         (provider-value (%decode-ref object "provider"))
         (host-value (%decode-ref object "host")))
    (cond
      ((and (not (eq provider-value json-null)) (eq host-value json-null))
       (let ((provider (%decode-fields provider-value (format nil "~A.provider" path)
                                       '("node" "port") '("node" "port"))))
         (make-service-binding
          :id (%decode-ref object "id")
          :consumer-node (%decode-ref consumer "node")
          :import-port (%decode-ref consumer "port")
          :provider-node (%decode-ref provider "node")
          :provider-port (%decode-ref provider "port"))))
      ((and (eq provider-value json-null) (not (eq host-value json-null)))
       (make-service-binding
        :id (%decode-ref object "id")
        :consumer-node (%decode-ref consumer "node")
        :import-port (%decode-ref consumer "port")
        :host-port (%port-from-value host-value (format nil "~A.host" path))))
      (t (%wire-error :invalid-service-binding path
                      "choose exactly one provider or host")))))

(defun %contains-float-p (value)
  (typecase value
    (float t)
    (cons (or (%contains-float-p (car value)) (%contains-float-p (cdr value))))
    (vector (some #'%contains-float-p value))
    (t nil)))

(defun %source-from-value (value path)
  (let* ((base (%decode-fields value path '("kind" "input" "step" "type" "value")
                               '("kind")))
         (kind (%decode-string (%decode-ref base "kind")
                               (format nil "~A.kind" path))))
    (cond ((string= kind "input")
           (%decode-fields value path '("kind" "input") '("kind" "input"))
           (input-source (%decode-ref base "input")))
          ((string= kind "step")
           (%decode-fields value path '("kind" "step") '("kind" "step"))
           (step-source (%decode-ref base "step")))
          ((string= kind "literal")
           (%decode-fields value path '("kind" "type" "value")
                           '("kind" "type" "value"))
           (when (%contains-float-p (%decode-ref base "value"))
             (%wire-error :untyped-float (format nil "~A.value" path)
                          "literal floats require an explicit typed encoding"))
           (literal-source (%decode-ref base "value")
                           (%type-from-value (%decode-ref base "type")
                                             (format nil "~A.type" path))))
          (t (%wire-error :unknown-source-kind (format nil "~A.kind" path)
                          "unknown source kind ~S" kind)))))

(defun %step-from-value (value path)
  (let ((object (%decode-fields value path
                                '("id" "node" "port" "operation" "bindings")
                                '("id" "node" "port" "operation" "bindings"))))
    (make-wire-step
     :id (%decode-ref object "id") :node-id (%decode-ref object "node")
     :port (%decode-ref object "port") :operation (%decode-ref object "operation")
     :bindings
     (loop for binding in (%decode-array (%decode-ref object "bindings")
                                         (format nil "~A.bindings" path))
           for index from 0
           for binding-path = (format nil "~A.bindings[~D]" path index)
           for decoded = (%decode-fields binding binding-path
                                         '("argument" "source")
                                         '("argument" "source"))
           collect (make-data-binding
                    (%decode-ref decoded "argument")
                    (%source-from-value (%decode-ref decoded "source")
                                        (format nil "~A.source" binding-path)))))))

(defun wire-graph-from-value (value)
  (let ((object (%decode-fields
                 value "$" '("schema" "name" "nodes" "serviceBindings" "steps"
                              "inputs" "outputs" "capabilityGrants"
                              "requiredEvidence" "limits")
                 '("schema" "name" "nodes" "serviceBindings" "steps" "inputs"
                   "outputs" "capabilityGrants" "requiredEvidence" "limits"))))
    (unless (string= (%decode-string (%decode-ref object "schema") "$.schema")
                     +graph-schema+)
      (%wire-error :schema-mismatch "$.schema" "unsupported graph schema"))
    (make-wire-graph
     :name (%decode-ref object "name")
     :nodes (loop for node in (%decode-array (%decode-ref object "nodes") "$.nodes")
                  for index from 0 collect (%node-from-value node
                                                            (format nil "$.nodes[~D]" index)))
     :services
     (loop for service in (%decode-array (%decode-ref object "serviceBindings")
                                        "$.serviceBindings")
           for index from 0 collect (%service-from-value
                                     service (format nil "$.serviceBindings[~D]" index)))
     :steps (loop for step in (%decode-array (%decode-ref object "steps") "$.steps")
                  for index from 0 collect (%step-from-value step
                                                            (format nil "$.steps[~D]" index)))
     :inputs (loop for input in (%decode-array (%decode-ref object "inputs") "$.inputs")
                   for index from 0 collect (%field-from-value
                                             input (format nil "$.inputs[~D]" index)))
     :outputs
     (loop for output in (%decode-array (%decode-ref object "outputs") "$.outputs")
           for index from 0
           for path = (format nil "$.outputs[~D]" index)
           for decoded = (%decode-fields output path '("name" "step")
                                         '("name" "step"))
           collect (make-wire-output (%decode-ref decoded "name")
                                     (%decode-ref decoded "step")))
     :capability-grants (%decode-string-array (%decode-ref object "capabilityGrants")
                                              "$.capabilityGrants")
     :required-evidence (%decode-string-array (%decode-ref object "requiredEvidence")
                                              "$.requiredEvidence")
     :limits (%decode-ref object "limits"))))

(defun wire-graph-from-json (text) (wire-graph-from-value (json-parse text)))

(defun %violation-from-value (value path)
  (let ((object (%decode-fields value path '("stage" "path" "code" "detail")
                                '("stage" "path" "code" "detail"))))
    (%make-wire-violation (%decode-keyword (%decode-ref object "stage")
                                           (format nil "~A.stage" path))
                          (%decode-string (%decode-ref object "path")
                                          (format nil "~A.path" path))
                          (%decode-keyword (%decode-ref object "code")
                                           (format nil "~A.code" path))
                          (%decode-string (%decode-ref object "detail")
                                          (format nil "~A.detail" path)))))

(defun wire-receipt-from-value (value)
  (let* ((object (%decode-fields
                  value "$" '("schema" "kind" "verdict" "graph" "violations"
                               "stepOrder" "outputs" "evidence")
                  '("schema" "kind" "verdict" "graph" "violations"
                    "stepOrder" "outputs" "evidence")))
         (graph (%decode-fields (%decode-ref object "graph") "$.graph"
                                '("digest") '("digest"))))
    (unless (string= (%decode-string (%decode-ref object "schema") "$.schema")
                     +receipt-schema+)
      (%wire-error :schema-mismatch "$.schema" "unsupported receipt schema"))
    (%make-wire-receipt
     (%decode-keyword (%decode-ref object "kind") "$.kind")
     (%decode-keyword (%decode-ref object "verdict") "$.verdict")
     (%require-digest (%decode-ref graph "digest") "$.graph.digest")
     (loop for violation in (%decode-array (%decode-ref object "violations")
                                          "$.violations")
           for index from 0 collect (%violation-from-value
                                     violation (format nil "$.violations[~D]" index)))
     (%decode-string-array (%decode-ref object "stepOrder") "$.stepOrder")
     (%freeze-value (%decode-ref object "outputs"))
     (%freeze-value (%decode-ref object "evidence")))))

(defun wire-receipt-from-json (text) (wire-receipt-from-value (json-parse text)))
