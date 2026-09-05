;;;; cross-boundary.lisp --- graph -> Eshkol -> ABI -> Moonlab -> verifier.

(in-package #:rosette-wire-diagnostics)

(defparameter +moonlab-surface-wire-campaign-schema+
  "urn:rosette-wire:schema:diagnostics:moonlab-surface-campaign:1")
(defparameter +moonlab-surface-wire-bundle-schema+
  "urn:rosette-wire:schema:diagnostics:moonlab-surface-bundle:1")
(defparameter +cross-boundary-wire-bundle-schema+
  "urn:rosette-wire:schema:diagnostics:cross-boundary-bundle:1")
(defconstant +cross-boundary-bundle-schema+
  :rosette-wire-cross-boundary-diagnostic-bundle/v1)
(defparameter +cross-boundary-evidence-name+
  "cross-boundary-bundle-integrity")

(defstruct (cross-boundary-diagnostic-bundle
            (:constructor %make-cross-boundary-diagnostic-bundle
                (&key id eshkol-bundle moonlab-bundle spec verdict
                      earliest-boundary)))
  "One portable chain binding an Eshkol result to a Moonlab request and check."
  id eshkol-bundle moonlab-bundle spec verdict earliest-boundary)

(defun %derived-moonlab-spec (eshkol-bundle)
  (unless (and (verify-diagnostic-bundle eshkol-bundle)
               (eq :pass (diagnostic-bundle-verdict eshkol-bundle)))
    (error "The ABI transfer requires a verified passing Eshkol bundle"))
  (let ((value (getf (diagnostic-bundle-observations eshkol-bundle) :direct)))
    (unless (integerp value) (error "Eshkol transfer value is not an integer"))
    (list :measurement-step (mod value 16)
          :channel-step (mod value 17)
          :qgt-phase (mod value 4)
          :gradient-a (- (mod value 33) 16)
          :gradient-b (- (mod (floor value 33) 33) 16))))

(defun %cross-boundary-content-form
    (eshkol-bundle moonlab-bundle spec verdict earliest-boundary)
  (list :schema +cross-boundary-bundle-schema+
        :eshkol (diagnostic-bundle->form eshkol-bundle)
        :moonlab (moonlab-surface-diagnostic-bundle->form moonlab-bundle)
        :spec spec :verdict verdict :earliest-boundary earliest-boundary))

(defun %finish-cross-boundary-bundle (eshkol-bundle moonlab-bundle)
  (unless (and (verify-diagnostic-bundle eshkol-bundle)
               (verify-moonlab-surface-diagnostic-bundle moonlab-bundle))
    (error "Cross-boundary children must independently verify"))
  (let* ((spec (%derived-moonlab-spec eshkol-bundle))
         (actual (moonlab-surface-diagnostic-bundle-spec moonlab-bundle)))
    (unless (equal spec actual)
      (error "Moonlab request is not derived from the Eshkol transfer value"))
    (let* ((passed (eq :pass
                       (moonlab-surface-diagnostic-bundle-verdict moonlab-bundle)))
           (verdict (if passed :pass :fail))
           (boundary
             (if passed :complete
                 (moonlab-surface-diagnostic-bundle-earliest-boundary
                  moonlab-bundle)))
           (content (%cross-boundary-content-form
                     eshkol-bundle moonlab-bundle spec verdict boundary)))
      (%make-cross-boundary-diagnostic-bundle
       :id (cid:content-id-long content) :eshkol-bundle eshkol-bundle
       :moonlab-bundle moonlab-bundle :spec spec :verdict verdict
       :earliest-boundary boundary))))

(defun cross-boundary-diagnostic-bundle->form (bundle)
  (unless (cross-boundary-diagnostic-bundle-p bundle)
    (error "Expected a CROSS-BOUNDARY-DIAGNOSTIC-BUNDLE"))
  (list :id (cross-boundary-diagnostic-bundle-id bundle) :content
        (%cross-boundary-content-form
         (cross-boundary-diagnostic-bundle-eshkol-bundle bundle)
         (cross-boundary-diagnostic-bundle-moonlab-bundle bundle)
         (cross-boundary-diagnostic-bundle-spec bundle)
         (cross-boundary-diagnostic-bundle-verdict bundle)
         (cross-boundary-diagnostic-bundle-earliest-boundary bundle))))

(defun verify-cross-boundary-diagnostic-bundle (bundle)
  (and
   (cross-boundary-diagnostic-bundle-p bundle)
   (handler-case
       (let* ((eshkol (cross-boundary-diagnostic-bundle-eshkol-bundle bundle))
              (moonlab (cross-boundary-diagnostic-bundle-moonlab-bundle bundle))
              (spec (%derived-moonlab-spec eshkol))
              (passed (eq :pass
                          (moonlab-surface-diagnostic-bundle-verdict moonlab)))
              (verdict (if passed :pass :fail))
              (boundary
                (if passed :complete
                    (moonlab-surface-diagnostic-bundle-earliest-boundary
                     moonlab)))
              (content (%cross-boundary-content-form
                        eshkol moonlab spec verdict boundary)))
         (and (verify-moonlab-surface-diagnostic-bundle moonlab)
              (equal spec (moonlab-surface-diagnostic-bundle-spec moonlab))
              (equal spec (cross-boundary-diagnostic-bundle-spec bundle))
              (eq verdict (cross-boundary-diagnostic-bundle-verdict bundle))
              (eq boundary
                  (cross-boundary-diagnostic-bundle-earliest-boundary bundle))
              (string= (cross-boundary-diagnostic-bundle-id bundle)
                       (cid:content-id-long content))))
     (error () nil))))

(defun %moonlab-surface-campaign-wire-value (campaign)
  (%wire-envelope
   +moonlab-surface-wire-campaign-schema+
   (list :id (moonlab-surface-campaign-id campaign)
         :spec (moonlab-surface-campaign-spec campaign)
         :implementation-id
         (moonlab-surface-campaign-implementation-id campaign)
         :required-abi (moonlab-surface-campaign-required-abi campaign)
         :tolerance (moonlab-surface-campaign-tolerance campaign))))

(defun %moonlab-surface-campaign-from-wire-value (value)
  (let ((form (%wire-envelope-form
               value +moonlab-surface-wire-campaign-schema+
               "Moonlab surface campaign")))
    (unless (%exact-plist-keys-p
             form '(:id :spec :implementation-id :required-abi :tolerance))
      (error "Malformed Moonlab surface campaign"))
    (let ((campaign
            (make-moonlab-surface-campaign
             (getf form :spec) :implementation-id (getf form :implementation-id)
             :required-abi (getf form :required-abi)
             :tolerance (getf form :tolerance))))
      (unless (string= (getf form :id) (moonlab-surface-campaign-id campaign))
        (error "Moonlab surface campaign identity drift"))
      campaign)))

(defun %moonlab-surface-bundle-wire-value (bundle)
  (unless (verify-moonlab-surface-diagnostic-bundle bundle)
    (error "Refusing to serialize invalid Moonlab surface evidence"))
  (%wire-envelope +moonlab-surface-wire-bundle-schema+
                  (moonlab-surface-diagnostic-bundle->form bundle)))

(defun %moonlab-surface-bundle-from-wire-value (value)
  (let* ((form (%wire-envelope-form
                value +moonlab-surface-wire-bundle-schema+
                "Moonlab surface bundle"))
         (content (and (%exact-plist-keys-p form '(:id :content))
                       (getf form :content))))
    (unless (and content
                 (%exact-plist-keys-p
                  content
                  '(:schema :campaign-id :spec :implementation-id :required-abi
                    :observed-abi :tolerance :verdict :outcome
                    :earliest-boundary :observations :error-status))
                 (eq (getf content :schema) +moonlab-surface-bundle-schema+))
      (error "Malformed Moonlab surface bundle"))
    (let ((bundle
            (%make-moonlab-surface-diagnostic-bundle
             :id (getf form :id) :campaign-id (getf content :campaign-id)
             :spec (copy-list (getf content :spec))
             :implementation-id (getf content :implementation-id)
             :required-abi (copy-list (getf content :required-abi))
             :observed-abi (copy-list (getf content :observed-abi))
             :tolerance (getf content :tolerance)
             :verdict (getf content :verdict)
             :outcome (getf content :outcome)
             :earliest-boundary (getf content :earliest-boundary)
             :observations (copy-tree (getf content :observations))
             :error-status (getf content :error-status))))
      (unless (verify-moonlab-surface-diagnostic-bundle bundle)
        (error "Moonlab surface bundle verification failed"))
      bundle)))

(defun %cross-boundary-bundle-wire-value (bundle)
  (unless (verify-cross-boundary-diagnostic-bundle bundle)
    (error "Refusing to serialize invalid cross-boundary evidence"))
  (%wire-envelope +cross-boundary-wire-bundle-schema+
                  (cross-boundary-diagnostic-bundle->form bundle)))

(defun %cross-boundary-bundle-from-wire-value (value)
  (let* ((form (%wire-envelope-form
                value +cross-boundary-wire-bundle-schema+
                "cross-boundary bundle"))
         (content (and (%exact-plist-keys-p form '(:id :content))
                       (getf form :content))))
    (unless (and content
                 (%exact-plist-keys-p
                  content '(:schema :eshkol :moonlab :spec :verdict
                            :earliest-boundary))
                 (eq (getf content :schema) +cross-boundary-bundle-schema+))
      (error "Malformed cross-boundary bundle"))
    (let* ((eshkol
             (eshkol-diagnostic-bundle-from-wire-value
              (%wire-envelope +eshkol-wire-bundle-schema+
                              (getf content :eshkol))))
           (moonlab
             (%moonlab-surface-bundle-from-wire-value
              (%wire-envelope +moonlab-surface-wire-bundle-schema+
                              (getf content :moonlab))))
           (bundle
             (%make-cross-boundary-diagnostic-bundle
              :id (getf form :id) :eshkol-bundle eshkol
              :moonlab-bundle moonlab :spec (copy-list (getf content :spec))
              :verdict (getf content :verdict)
              :earliest-boundary (getf content :earliest-boundary))))
      (unless (verify-cross-boundary-diagnostic-bundle bundle)
        (error "Cross-boundary bundle verification failed"))
      bundle)))

(defun %transfer-output-type ()
  (rw:record-type
   (rw:make-wire-field "campaign" (rw:scalar-type :string))
   (rw:make-wire-field "eshkolBundleId" (rw:scalar-type :string))
   (rw:make-wire-field "kind" (rw:scalar-type :string))))

(defun %cross-output-type ()
  (rw:record-type
   (rw:make-wire-field "boundary" (rw:scalar-type :string))
   (rw:make-wire-field "bundle" (rw:scalar-type :string))
   (rw:make-wire-field "bundleId" (rw:scalar-type :string))
   (rw:make-wire-field "kind" (rw:scalar-type :string))
   (rw:make-wire-field "verdict" (rw:scalar-type :string))))

(defun %transfer-descriptor ()
  (rw:make-wire-descriptor
   :name "rosette.diagnostics.abi-transfer" :version "1.0.0"
   :imports nil
   :exports
   (list (rw:make-wire-port
          "transfer"
          (list (rw:make-wire-operation
                 "bind"
                 (list (rw:make-wire-field "eshkol"
                                           (%diagnostic-output-type))
                       (rw:make-wire-field "moonlabConfig"
                                           (rw:scalar-type :string)))
                 (%transfer-output-type)))))
   :effects '(:pure) :capabilities nil
   :adapter '(("protocol" . "eshkol-integer-to-moonlab-surface/v1"))
   :verifiers nil))

(defun %surface-descriptor ()
  (rw:make-wire-descriptor
   :name "rosette.diagnostics.moonlab-surfaces" :version "1.0.0"
   :imports nil
   :exports
   (list (rw:make-wire-port
          "moonlab"
          (list (rw:make-wire-operation
                 "run"
                 (list (rw:make-wire-field "transfer"
                                           (%transfer-output-type)))
                 (%diagnostic-output-type)))))
   :effects '(:subprocess :native-ffi :accelerator)
   :capabilities '("moonlab.abi/surfaces@0.6" "process/spawn@1")
   :adapter '(("protocol" . "moonlab-public-surfaces/v0.6"))
   :verifiers nil))

(defun %cross-verifier-descriptor ()
  (rw:make-wire-descriptor
   :name "rosette.diagnostics.cross-boundary-verifier" :version "1.0.0"
   :imports nil
   :exports
   (list (rw:make-wire-port
          "verifier"
          (list (rw:make-wire-operation
                 "verify"
                 (list (rw:make-wire-field "eshkol"
                                           (%diagnostic-output-type))
                       (rw:make-wire-field "moonlab"
                                           (%diagnostic-output-type)))
                 (%cross-output-type)))))
   :effects '(:pure) :capabilities nil
   :adapter '(("protocol" . "cross-boundary-verifier/v1"))
   :verifiers (list +cross-boundary-evidence-name+)))

(defun %moonlab-config-wire-value
    (implementation-id required-abi tolerance)
  (%wire-envelope
   "urn:rosette-wire:schema:diagnostics:moonlab-config:1"
   (list :implementation-id implementation-id :required-abi required-abi
         :tolerance (coerce tolerance 'double-float))))

(defun %moonlab-config-from-wire-value (value)
  (let ((form
          (%wire-envelope-form
           value "urn:rosette-wire:schema:diagnostics:moonlab-config:1"
           "Moonlab config")))
    (unless (%exact-plist-keys-p
             form '(:implementation-id :required-abi :tolerance))
      (error "Malformed Moonlab config"))
    form))

(defun make-cross-boundary-diagnostic-wire-graph
    (eshkol-campaign &key moonlab-implementation-id
                          (moonlab-required-abi '(0 6 0))
                          (moonlab-tolerance 1d-8) dependency-set-id)
  "Build the four-step graph binding real Eshkol output into Moonlab."
  (unless (eshkol-campaign-p eshkol-campaign)
    (error "Expected an ESHKOL-CAMPAIGN"))
  ;; Constructor validation is reused even though the runtime spec is derived.
  (make-moonlab-surface-campaign
   '(:measurement-step 0 :channel-step 0 :qgt-phase 0
     :gradient-a 0 :gradient-b 0)
   :implementation-id moonlab-implementation-id
   :required-abi moonlab-required-abi :tolerance moonlab-tolerance)
  (let* ((eshkol-descriptor (%diagnostic-descriptor :eshkol))
         (transfer-descriptor (%transfer-descriptor))
         (moonlab-descriptor (%surface-descriptor))
         (verifier-descriptor (%cross-verifier-descriptor))
         (nodes
           (list
            (rw:make-wire-node
             "eshkol-diagnostic" eshkol-descriptor
             (%diagnostic-implementation-id
              :eshkol (eshkol-campaign-toolchain-id eshkol-campaign))
             :dependency-set-id dependency-set-id)
            (rw:make-wire-node
             "abi-transfer" transfer-descriptor
             (rw:canonical-id '(("adapter" . "eshkol-to-moonlab/v1")))
             :dependency-set-id dependency-set-id)
            (rw:make-wire-node
             "moonlab-surfaces" moonlab-descriptor
             (%diagnostic-implementation-id
              :moonlab moonlab-implementation-id)
             :dependency-set-id dependency-set-id)
            (rw:make-wire-node
             "cross-verifier" verifier-descriptor
             (rw:canonical-id '(("verifier" . "cross-boundary/v1")))
             :dependency-set-id dependency-set-id)))
         (steps
           (list
            (rw:make-wire-step
             :id "diagnose-eshkol" :node-id "eshkol-diagnostic"
             :port "eshkol" :operation "run"
             :bindings
             (list
              (rw:make-data-binding
               "campaign"
               (rw:literal-source
                (rw:canonical-json
                 (%eshkol-campaign-wire-value eshkol-campaign))
                (rw:scalar-type :string)))))
            (rw:make-wire-step
             :id "bind-abi" :node-id "abi-transfer" :port "transfer"
             :operation "bind"
             :bindings
             (list
              (rw:make-data-binding "eshkol"
                                    (rw:step-source "diagnose-eshkol"))
              (rw:make-data-binding
               "moonlabConfig"
               (rw:literal-source
                (rw:canonical-json
                 (%moonlab-config-wire-value
                  moonlab-implementation-id moonlab-required-abi
                  moonlab-tolerance))
                (rw:scalar-type :string)))))
            (rw:make-wire-step
             :id "diagnose-moonlab" :node-id "moonlab-surfaces"
             :port "moonlab" :operation "run"
             :bindings
             (list (rw:make-data-binding "transfer"
                                         (rw:step-source "bind-abi"))))
            (rw:make-wire-step
             :id "verify-boundary" :node-id "cross-verifier"
             :port "verifier" :operation "verify"
             :bindings
             (list
              (rw:make-data-binding "eshkol"
                                    (rw:step-source "diagnose-eshkol"))
              (rw:make-data-binding "moonlab"
                                    (rw:step-source "diagnose-moonlab")))))))
    (rw:make-wire-graph
     :name "eshkol-moonlab-cross-boundary" :nodes nodes :services nil
     :steps steps :inputs nil
     :outputs (list (rw:make-wire-output "diagnostic" "verify-boundary"))
     :capability-grants
     '("moonlab.abi/surfaces@0.6" "process/spawn@1")
     :required-evidence (list +cross-boundary-evidence-name+)
     :limits '(("maxOutputBytes" . 8388608)
               ("maxSteps" . 4) ("maxWallMilliseconds" . 0)))))

(defun %cross-output (bundle)
  `(("boundary" . ,(%output-token
                     (cross-boundary-diagnostic-bundle-earliest-boundary
                      bundle)))
    ("bundle" . ,(rw:canonical-json
                   (%cross-boundary-bundle-wire-value bundle)))
    ("bundleId" . ,(cross-boundary-diagnostic-bundle-id bundle))
    ("kind" . "cross-boundary")
    ("verdict" . ,(%output-token
                    (cross-boundary-diagnostic-bundle-verdict bundle)))))

(defun register-cross-boundary-diagnostic-adapter
    (runner &key eshkol-command aot-run-prefix policy
                 moonlab-command moonlab-library)
  "Register all four execution handlers and the independent final verifier."
  (register-eshkol-diagnostic-adapter
   runner :eshkol-command eshkol-command :aot-run-prefix aot-run-prefix
   :policy policy)
  (rw:register-wire-handler
   runner "abi-transfer" "transfer" "bind"
   (lambda (arguments services context)
     (declare (ignore services context))
     (let* ((eshkol-output (%wire-object-ref arguments "eshkol"))
            (eshkol
              (eshkol-diagnostic-bundle-from-wire-value
               (json:json-parse (%wire-object-ref eshkol-output "bundle"))))
            (config
              (%moonlab-config-from-wire-value
               (json:json-parse (%wire-object-ref arguments "moonlabConfig"))))
            (campaign
              (make-moonlab-surface-campaign
               (%derived-moonlab-spec eshkol)
               :implementation-id (getf config :implementation-id)
               :required-abi (getf config :required-abi)
               :tolerance (getf config :tolerance))))
       `(("campaign" . ,(rw:canonical-json
                          (%moonlab-surface-campaign-wire-value campaign)))
         ("eshkolBundleId" . ,(diagnostic-bundle-id eshkol))
         ("kind" . "abi-transfer")))))
  (rw:register-wire-handler
   runner "moonlab-surfaces" "moonlab" "run"
   (lambda (arguments services context)
     (declare (ignore services context))
     (let* ((transfer (%wire-object-ref arguments "transfer"))
            (campaign
              (%moonlab-surface-campaign-from-wire-value
               (json:json-parse (%wire-object-ref transfer "campaign"))))
            (bundle
              (run-moonlab-surface-campaign
               campaign :moonlab-command moonlab-command
               :moonlab-library moonlab-library)))
       `(("boundary" . ,(%output-token
                          (moonlab-surface-diagnostic-bundle-earliest-boundary
                           bundle)))
         ("bundle" . ,(rw:canonical-json
                        (%moonlab-surface-bundle-wire-value bundle)))
         ("bundleId" . ,(moonlab-surface-diagnostic-bundle-id bundle))
         ("kind" . "moonlab-surfaces")
         ("verdict" . ,(%output-token
                         (moonlab-surface-diagnostic-bundle-verdict bundle)))))))
  (rw:register-wire-handler
   runner "cross-verifier" "verifier" "verify"
   (lambda (arguments services context)
     (declare (ignore services context))
     (let* ((eshkol-output (%wire-object-ref arguments "eshkol"))
            (moonlab-output (%wire-object-ref arguments "moonlab"))
            (eshkol
              (eshkol-diagnostic-bundle-from-wire-value
               (json:json-parse (%wire-object-ref eshkol-output "bundle"))))
            (moonlab
              (%moonlab-surface-bundle-from-wire-value
               (json:json-parse (%wire-object-ref moonlab-output "bundle"))))
            (bundle (%finish-cross-boundary-bundle eshkol moonlab)))
       (%cross-output bundle))))
  (rw:register-wire-verifier
   runner +cross-boundary-evidence-name+
   (lambda (graph receipt)
     (declare (ignore graph))
     (handler-case
         (let* ((output (%receipt-diagnostic-output receipt))
                (bundle
                  (%cross-boundary-bundle-from-wire-value
                   (json:json-parse (%wire-object-ref output "bundle"))))
                (id (cross-boundary-diagnostic-bundle-id bundle))
                (verdict
                  (%output-token
                   (cross-boundary-diagnostic-bundle-verdict bundle)))
                (boundary
                  (%output-token
                   (cross-boundary-diagnostic-bundle-earliest-boundary bundle)))
                (pass
                  (and (string= (%wire-object-ref output "kind")
                                "cross-boundary")
                       (string= (%wire-object-ref output "bundleId") id)
                       (string= (%wire-object-ref output "verdict") verdict)
                       (string= (%wire-object-ref output "boundary") boundary))))
           (values pass `(("boundary" . ,boundary) ("bundleId" . ,id)
                          ("kind" . "cross-boundary")
                          ("verdict" . ,verdict))))
       (error ()
         (values nil '(("kind" . "cross-boundary")
                       ("status" . "invalid")))))))
  runner)
