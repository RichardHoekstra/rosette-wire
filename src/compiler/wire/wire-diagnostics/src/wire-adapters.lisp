;;;; wire-adapters.lisp --- Eshkol and Moonlab campaigns as ordinary Wire graphs.

(in-package #:rosette-wire-diagnostics)

(defparameter +eshkol-wire-campaign-schema+
  "urn:rosette-wire:schema:diagnostics:eshkol-campaign:1")
(defparameter +eshkol-wire-bundle-schema+
  "urn:rosette-wire:schema:diagnostics:eshkol-bundle:1")
(defparameter +moonlab-wire-campaign-schema+
  "urn:rosette-wire:schema:diagnostics:moonlab-campaign:1")
(defparameter +moonlab-wire-bundle-schema+
  "urn:rosette-wire:schema:diagnostics:moonlab-bundle:1")
(defparameter +eshkol-evidence-name+ "eshkol-bundle-integrity")
(defparameter +moonlab-evidence-name+ "moonlab-bundle-integrity")

(defun %wire-object-ref (object name)
  (cdr (assoc name object :test #'string=)))

(defun %wire-object-fields (object expected context)
  "Require OBJECT to contain exactly EXPECTED string keys once each."
  (unless (and (listp object)
               (every (lambda (entry)
                        (and (consp entry) (stringp (car entry))))
                      object))
    (error "~A must be a JSON object" context))
  (let ((actual (mapcar #'car object)))
    (unless (and (= (length actual) (length expected))
                 (= (length actual)
                    (length (remove-duplicates actual :test #'string=)))
                 (null (set-difference actual expected :test #'string=))
                 (null (set-difference expected actual :test #'string=)))
      (error "~A has unexpected, missing, or duplicate fields" context)))
  object)

(defun %symbol-wire-value (value)
  `(("kind" . "symbol")
    ("name" . ,(symbol-name value))
    ("package" . ,(if (symbol-package value)
                       (package-name (symbol-package value))
                       json:json-null))))

(defun %float-wire-value (value)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float value)
    `(("exponent" . ,(format nil "~D" exponent))
      ("format" . ,(string-downcase (princ-to-string (type-of value))))
      ("kind" . "float")
      ("sign" . ,(format nil "~D" sign))
      ("significand" . ,(format nil "~D" significand)))))

(defun %lisp-wire-value (value)
  "Encode the evidence Lisp subset without relying on a reader or packages."
  (cond
    ((null value) '(("kind" . "null")))
    ((symbolp value) (%symbol-wire-value value))
    ((stringp value) `(("kind" . "string") ("value" . ,value)))
    ((integerp value)
     `(("kind" . "integer") ("value" . ,(format nil "~D" value))))
    ((floatp value) (%float-wire-value value))
    ((rationalp value)
     `(("denominator" . ,(%lisp-wire-value (denominator value)))
       ("kind" . "ratio")
       ("numerator" . ,(%lisp-wire-value (numerator value)))))
    ((complexp value)
     `(("imag" . ,(%lisp-wire-value (imagpart value)))
       ("kind" . "complex")
       ("real" . ,(%lisp-wire-value (realpart value)))))
    ((characterp value)
     `(("code" . ,(format nil "~D" (char-code value)))
       ("kind" . "character")))
    ((consp value)
     `(("car" . ,(%lisp-wire-value (car value)))
       ("cdr" . ,(%lisp-wire-value (cdr value)))
       ("kind" . "cons")))
    (t (error "Unsupported diagnostic evidence value ~S" (type-of value)))))

(defun %wire-integer (string context)
  (unless (and (stringp string) (plusp (length string)))
    (error "~A must be a decimal integer string" context))
  (handler-case (parse-integer string :junk-allowed nil)
    (error () (error "~A must be a decimal integer string" context))))

(defun %wire-float-type (name)
  (cond ((string= name "short-float") 'short-float)
        ((string= name "single-float") 'single-float)
        ((string= name "double-float") 'double-float)
        ((string= name "long-float") 'long-float)
        (t (error "Unsupported floating-point format ~S" name))))

(defun %wire-symbol (object)
  (%wire-object-fields object '("kind" "name" "package") "symbol value")
  (let ((name (%wire-object-ref object "name"))
        (package-name (%wire-object-ref object "package")))
    (unless (and (stringp name) (plusp (length name)))
      (error "Symbol name must be nonempty"))
    (cond
      ((eq package-name json:json-null) (make-symbol name))
      ;; INTERN consumes the exact encoded name; :PRESERVE case is required.
      ((string= package-name "KEYWORD") (intern name :keyword)) ; :preserve
      ((string= package-name "COMMON-LISP")
       (or (find-symbol name :common-lisp) ; :preserve encoded symbol case
           (error "Unknown COMMON-LISP symbol ~S" name)))
      (t (error "Diagnostic evidence refuses package ~S" package-name)))))

(defun %wire-lisp-value (object)
  "Decode the tagged diagnostic value without invoking the Lisp reader."
  (unless (listp object) (error "Tagged Lisp value must be an object"))
  (let ((kind (%wire-object-ref object "kind")))
    (unless (stringp kind) (error "Tagged Lisp value has no string kind"))
    (cond
      ((string= kind "null")
       (%wire-object-fields object '("kind") "null value") nil)
      ((string= kind "symbol") (%wire-symbol object))
      ((string= kind "string")
       (%wire-object-fields object '("kind" "value") "string value")
       (let ((value (%wire-object-ref object "value")))
         (unless (stringp value) (error "String value must be a string"))
         (copy-seq value)))
      ((string= kind "integer")
       (%wire-object-fields object '("kind" "value") "integer value")
       (%wire-integer (%wire-object-ref object "value") "integer value"))
      ((string= kind "float")
       (%wire-object-fields
        object '("exponent" "format" "kind" "sign" "significand")
        "float value")
       (let* ((type (%wire-float-type (%wire-object-ref object "format")))
              (sign (%wire-integer (%wire-object-ref object "sign")
                                   "float sign"))
              (significand
                (%wire-integer (%wire-object-ref object "significand")
                               "float significand"))
              (exponent (%wire-integer (%wire-object-ref object "exponent")
                                       "float exponent")))
         (unless (member sign '(-1 1)) (error "Float sign must be -1 or 1"))
         (scale-float (coerce (* sign significand) type) exponent)))
      ((string= kind "ratio")
       (%wire-object-fields object '("denominator" "kind" "numerator")
                            "ratio value")
       (let ((denominator
               (%wire-lisp-value (%wire-object-ref object "denominator")))
             (numerator (%wire-lisp-value (%wire-object-ref object "numerator"))))
         (unless (and (integerp numerator) (integerp denominator)
                      (not (zerop denominator)))
           (error "Malformed ratio"))
         (/ numerator denominator)))
      ((string= kind "complex")
       (%wire-object-fields object '("imag" "kind" "real") "complex value")
       (let ((real (%wire-lisp-value (%wire-object-ref object "real")))
             (imag (%wire-lisp-value (%wire-object-ref object "imag"))))
         (unless (and (realp real) (realp imag)) (error "Malformed complex"))
         (complex real imag)))
      ((string= kind "character")
       (%wire-object-fields object '("code" "kind") "character value")
       (let ((value (code-char
                     (%wire-integer (%wire-object-ref object "code")
                                    "character code"))))
         (unless value (error "Invalid character code"))
         value))
      ((string= kind "cons")
       (%wire-object-fields object '("car" "cdr" "kind") "cons value")
       (cons (%wire-lisp-value (%wire-object-ref object "car"))
             (%wire-lisp-value (%wire-object-ref object "cdr"))))
      (t (error "Unknown tagged Lisp kind ~S" kind)))))

(defun %exact-plist-keys-p (value keys)
  (and (listp value)
       (evenp (length value))
       (equal (loop for tail on value by #'cddr collect (first tail)) keys)))

(defun %wire-envelope (schema form)
  `(("form" . ,(%lisp-wire-value form)) ("schema" . ,schema)))

(defun %wire-envelope-form (value schema context)
  (%wire-object-fields value '("form" "schema") context)
  (unless (string= (%wire-object-ref value "schema") schema)
    (error "~A uses the wrong schema" context))
  (%wire-lisp-value (%wire-object-ref value "form")))

(defun %eshkol-campaign-wire-value (campaign)
  (%wire-envelope
   +eshkol-wire-campaign-schema+
   (list :id (eshkol-campaign-id campaign)
         :source (eshkol-campaign-source campaign)
         :toolchain-id (eshkol-campaign-toolchain-id campaign))))

(defun %eshkol-campaign-from-wire-value (value)
  (let ((form (%wire-envelope-form value +eshkol-wire-campaign-schema+
                                   "Eshkol campaign")))
    (unless (%exact-plist-keys-p form '(:id :source :toolchain-id))
      (error "Malformed Eshkol campaign form"))
    (let ((campaign
            (make-eshkol-campaign (getf form :source)
                                  :toolchain-id (getf form :toolchain-id))))
      (unless (string= (getf form :id) (eshkol-campaign-id campaign))
        (error "Eshkol campaign identity drift"))
      campaign)))

(defun %moonlab-campaign-wire-value (campaign)
  (%wire-envelope
   +moonlab-wire-campaign-schema+
   (list :id (moonlab-campaign-id campaign)
         :circuit (moonlab-campaign-circuit campaign)
         :implementation-id (moonlab-campaign-implementation-id campaign)
         :required-abi (moonlab-campaign-required-abi campaign)
         :tolerance (moonlab-campaign-tolerance campaign))))

(defun %moonlab-campaign-from-wire-value (value)
  (let ((form (%wire-envelope-form value +moonlab-wire-campaign-schema+
                                   "Moonlab campaign")))
    (unless (%exact-plist-keys-p
             form '(:id :circuit :implementation-id :required-abi :tolerance))
      (error "Malformed Moonlab campaign form"))
    (let ((campaign
            (make-moonlab-campaign
             (getf form :circuit)
             :implementation-id (getf form :implementation-id)
             :required-abi (getf form :required-abi)
             :tolerance (getf form :tolerance))))
      (unless (string= (getf form :id) (moonlab-campaign-id campaign))
        (error "Moonlab campaign identity drift"))
      campaign)))

(defun eshkol-diagnostic-bundle->wire-value (bundle)
  "Return a canonical-JSON-safe, exactly reversible Eshkol bundle value."
  (unless (verify-diagnostic-bundle bundle)
    (error "Refusing to serialize an invalid Eshkol diagnostic bundle"))
  (%wire-envelope +eshkol-wire-bundle-schema+
                  (diagnostic-bundle->form bundle)))

(defun eshkol-diagnostic-bundle-from-wire-value (value)
  "Reconstruct and independently verify an Eshkol bundle from VALUE."
  (let* ((form (%wire-envelope-form value +eshkol-wire-bundle-schema+
                                    "Eshkol bundle"))
         (content (and (%exact-plist-keys-p form '(:id :content))
                       (getf form :content))))
    (unless (and content
                 (%exact-plist-keys-p
                  content
                  '(:schema :campaign-id :source :program-id :toolchain-id
                    :required-floors :comparison :verdict :outcome
                    :earliest-boundary :observations :disagreements
                    :error-status))
                 (eq (getf content :schema) +bundle-schema+)
                 (equal (getf content :required-floors) +floors+)
                 (eq (getf content :comparison) +comparison+))
      (error "Malformed Eshkol bundle form"))
    (let ((bundle
            (%make-diagnostic-bundle
             :id (getf form :id)
             :campaign-id (getf content :campaign-id)
             :source (copy-tree (getf content :source))
             :program-id (getf content :program-id)
             :toolchain-id (getf content :toolchain-id)
             :verdict (getf content :verdict)
             :outcome (getf content :outcome)
             :earliest-boundary (getf content :earliest-boundary)
             :observations (copy-tree (getf content :observations))
             :disagreements (copy-tree (getf content :disagreements))
             :error-status (getf content :error-status))))
      (unless (verify-diagnostic-bundle bundle)
        (error "Eshkol bundle verification failed"))
      bundle)))

(defun moonlab-diagnostic-bundle->wire-value (bundle)
  "Return a canonical-JSON-safe, exactly reversible Moonlab bundle value."
  (unless (verify-moonlab-diagnostic-bundle bundle)
    (error "Refusing to serialize an invalid Moonlab diagnostic bundle"))
  (%wire-envelope +moonlab-wire-bundle-schema+
                  (moonlab-diagnostic-bundle->form bundle)))

(defun moonlab-diagnostic-bundle-from-wire-value (value)
  "Reconstruct and independently verify a Moonlab bundle from VALUE."
  (let* ((form (%wire-envelope-form value +moonlab-wire-bundle-schema+
                                    "Moonlab bundle"))
         (content (and (%exact-plist-keys-p form '(:id :content))
                       (getf form :content))))
    (unless (and content
                 (%exact-plist-keys-p
                  content
                  '(:schema :campaign-id :circuit :circuit-id
                    :implementation-id :required-abi :observed-abi :tolerance
                    :verdict :outcome :earliest-boundary :observations
                    :error-status))
                 (eq (getf content :schema) +moonlab-bundle-schema+))
      (error "Malformed Moonlab bundle form"))
    (unless (typep (getf content :tolerance) 'double-float)
      (error "Malformed Moonlab tolerance"))
    (let ((bundle
            (%make-moonlab-diagnostic-bundle
             :id (getf form :id)
             :campaign-id (getf content :campaign-id)
             :circuit (copy-tree (getf content :circuit))
             :circuit-id (getf content :circuit-id)
             :implementation-id (getf content :implementation-id)
             :required-abi (copy-list (getf content :required-abi))
             :observed-abi (copy-list (getf content :observed-abi))
             :tolerance (getf content :tolerance)
             :verdict (getf content :verdict)
             :outcome (getf content :outcome)
             :earliest-boundary (getf content :earliest-boundary)
             :observations (copy-tree (getf content :observations))
             :error-status (getf content :error-status))))
      (unless (verify-moonlab-diagnostic-bundle bundle)
        (error "Moonlab bundle verification failed"))
      bundle)))

(defun %diagnostic-output-type ()
  (rw:record-type
   (rw:make-wire-field "boundary" (rw:scalar-type :string))
   (rw:make-wire-field "bundle" (rw:scalar-type :string))
   (rw:make-wire-field "bundleId" (rw:scalar-type :string))
   (rw:make-wire-field "kind" (rw:scalar-type :string))
   (rw:make-wire-field "verdict" (rw:scalar-type :string))))

(defun %diagnostic-operation ()
  (rw:make-wire-operation
   "run" (list (rw:make-wire-field "campaign" (rw:scalar-type :string)))
   (%diagnostic-output-type)))

(defun %diagnostic-descriptor (kind)
  (ecase kind
    (:eshkol
     (rw:make-wire-descriptor
      :name "rosette.diagnostics.eshkol" :version "1.0.0"
      :imports nil
      :exports (list (rw:make-wire-port "eshkol" (list (%diagnostic-operation))))
      :effects '(:subprocess)
      :capabilities '("process/spawn@1")
      :adapter '(("protocol" . "eshkol-public-execution-floor/v1"))
      :verifiers (list +eshkol-evidence-name+)))
    (:moonlab
     (rw:make-wire-descriptor
      :name "rosette.diagnostics.moonlab" :version "1.0.0"
      :imports nil
      :exports (list (rw:make-wire-port "moonlab" (list (%diagnostic-operation))))
      :effects '(:subprocess :native-ffi)
      :capabilities '("moonlab.abi/unitary@0.6" "process/spawn@1")
      :adapter '(("protocol" . "moonlab-public-abi/v0.6"))
      :verifiers (list +moonlab-evidence-name+)))))

(defun %diagnostic-implementation-id (kind public-id)
  (rw:canonical-id
   `(("adapter" . ,(ecase kind
                     (:eshkol "eshkol-public-execution-floor/v1")
                     (:moonlab "moonlab-public-abi/v0.6")))
     ("implementation" . ,public-id))))

(defun %diagnostic-graph (kind campaign-value public-id dependency-set-id)
  (let* ((descriptor (%diagnostic-descriptor kind))
         (node-name (ecase kind
                      (:eshkol "eshkol-diagnostic")
                      (:moonlab "moonlab-diagnostic")))
         (port-name (ecase kind (:eshkol "eshkol") (:moonlab "moonlab")))
         (evidence (ecase kind
                     (:eshkol +eshkol-evidence-name+)
                     (:moonlab +moonlab-evidence-name+)))
         (capabilities (rw:wire-descriptor-capabilities descriptor))
         (node (rw:make-wire-node
                node-name descriptor
                (%diagnostic-implementation-id kind public-id)
                :dependency-set-id dependency-set-id))
         (step (rw:make-wire-step
                :id "diagnose" :node-id node-name :port port-name
                :operation "run"
                :bindings
                (list
                 (rw:make-data-binding
                  "campaign"
                  (rw:literal-source (rw:canonical-json campaign-value)
                                     (rw:scalar-type :string)))))))
    (rw:make-wire-graph
     :name (ecase kind
             (:eshkol "eshkol-diagnostic")
             (:moonlab "moonlab-diagnostic"))
     :nodes (list node) :services nil :steps (list step) :inputs nil
     :outputs (list (rw:make-wire-output "diagnostic" "diagnose"))
     :capability-grants capabilities :required-evidence (list evidence)
     :limits '(("maxOutputBytes" . 4194304)
               ("maxSteps" . 1)
               ("maxWallMilliseconds" . 0)))))

(defun make-eshkol-diagnostic-wire-graph (campaign &key dependency-set-id)
  "Bind CAMPAIGN into a one-step, bounded, immutable Wire graph."
  (unless (eshkol-campaign-p campaign)
    (error "Expected an ESHKOL-CAMPAIGN"))
  (%diagnostic-graph :eshkol (%eshkol-campaign-wire-value campaign)
                     (eshkol-campaign-toolchain-id campaign) dependency-set-id))

(defun make-moonlab-diagnostic-wire-graph (campaign &key dependency-set-id)
  "Bind CAMPAIGN into a one-step, bounded, immutable Wire graph."
  (unless (moonlab-campaign-p campaign)
    (error "Expected a MOONLAB-CAMPAIGN"))
  (%diagnostic-graph :moonlab (%moonlab-campaign-wire-value campaign)
                     (moonlab-campaign-implementation-id campaign)
                     dependency-set-id))

(defun %output-token (value)
  (string-downcase (symbol-name value)))

(defun %eshkol-output (bundle)
  `(("boundary" . ,(%output-token (diagnostic-bundle-earliest-boundary bundle)))
    ("bundle" . ,(rw:canonical-json
                   (eshkol-diagnostic-bundle->wire-value bundle)))
    ("bundleId" . ,(diagnostic-bundle-id bundle))
    ("kind" . "eshkol")
    ("verdict" . ,(%output-token (diagnostic-bundle-verdict bundle)))))

(defun %moonlab-output (bundle)
  `(("boundary" . ,(%output-token
                     (moonlab-diagnostic-bundle-earliest-boundary bundle)))
    ("bundle" . ,(rw:canonical-json
                   (moonlab-diagnostic-bundle->wire-value bundle)))
    ("bundleId" . ,(moonlab-diagnostic-bundle-id bundle))
    ("kind" . "moonlab")
    ("verdict" . ,(%output-token
                    (moonlab-diagnostic-bundle-verdict bundle)))))

(defun %receipt-diagnostic-output (receipt)
  (let ((entry (assoc "diagnostic" (rw:wire-receipt-outputs receipt)
                      :test #'string=)))
    (unless entry (error "Receipt has no diagnostic output"))
    (cdr entry)))

(defun %verify-output (receipt kind decoder id-reader verdict-reader boundary-reader)
  (handler-case
      (let* ((output (%receipt-diagnostic-output receipt))
             (bundle-text (%wire-object-ref output "bundle"))
             (bundle (funcall decoder (json:json-parse bundle-text)))
             (bundle-id (funcall id-reader bundle))
             (verdict (%output-token (funcall verdict-reader bundle)))
             (boundary (%output-token (funcall boundary-reader bundle)))
             (pass
               (and (string= (%wire-object-ref output "kind") kind)
                    (string= (%wire-object-ref output "bundleId") bundle-id)
                    (string= (%wire-object-ref output "verdict") verdict)
                    (string= (%wire-object-ref output "boundary") boundary))))
        (values pass
                `(("boundary" . ,boundary)
                  ("bundleId" . ,bundle-id)
                  ("kind" . ,kind)
                  ("verdict" . ,verdict))))
    (error () (values nil `(("kind" . ,kind) ("status" . "invalid"))))))

(defun register-eshkol-diagnostic-adapter
    (runner &key eshkol-command aot-run-prefix policy)
  "Register Eshkol execution and independent bundle verification in RUNNER."
  (rw:register-wire-handler
   runner "eshkol-diagnostic" "eshkol" "run"
   (lambda (arguments services context)
     (declare (ignore services context))
     (let* ((campaign-text (%wire-object-ref arguments "campaign"))
            (campaign (%eshkol-campaign-from-wire-value
                       (json:json-parse campaign-text)))
            (keywords
              (append (list :eshkol-command eshkol-command
                            :aot-run-prefix aot-run-prefix)
                      (when policy (list :policy policy)))))
       (%eshkol-output (apply #'run-eshkol-campaign campaign keywords)))))
  (rw:register-wire-verifier
   runner +eshkol-evidence-name+
   (lambda (graph receipt)
     (declare (ignore graph))
     (%verify-output receipt "eshkol"
                     #'eshkol-diagnostic-bundle-from-wire-value
                     #'diagnostic-bundle-id #'diagnostic-bundle-verdict
                     #'diagnostic-bundle-earliest-boundary)))
  runner)

(defun register-moonlab-diagnostic-adapter
    (runner &key moonlab-command moonlab-library)
  "Register Moonlab execution and independent bundle verification in RUNNER."
  (rw:register-wire-handler
   runner "moonlab-diagnostic" "moonlab" "run"
   (lambda (arguments services context)
     (declare (ignore services context))
     (let* ((campaign-text (%wire-object-ref arguments "campaign"))
            (campaign (%moonlab-campaign-from-wire-value
                       (json:json-parse campaign-text)))
            (bundle
              (run-moonlab-campaign
               campaign :moonlab-command moonlab-command
               :moonlab-library moonlab-library)))
       (%moonlab-output bundle))))
  (rw:register-wire-verifier
   runner +moonlab-evidence-name+
   (lambda (graph receipt)
     (declare (ignore graph))
     (%verify-output receipt "moonlab"
                     #'moonlab-diagnostic-bundle-from-wire-value
                     #'moonlab-diagnostic-bundle-id
                     #'moonlab-diagnostic-bundle-verdict
                     #'moonlab-diagnostic-bundle-earliest-boundary)))
  runner)
