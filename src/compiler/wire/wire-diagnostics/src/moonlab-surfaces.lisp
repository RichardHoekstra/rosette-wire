;;;; moonlab-surfaces.lisp --- wider public-consumer Moonlab diagnostics.

(in-package #:rosette-wire-diagnostics)

(defconstant +moonlab-surface-campaign-schema+
  :rosette-wire-moonlab-surface-campaign/v2)
(defconstant +moonlab-surface-bundle-schema+
  :rosette-wire-moonlab-surface-diagnostic-bundle/v2)
(defparameter +moonlab-surface-request-version+
  "ROSETTE-MOONLAB-SURFACES/2")
(defparameter +moonlab-surface-spec-keys+
  '(:measurement-step :channel-step :qgt-phase :gradient-a :gradient-b))
(defparameter +moonlab-qgt-masses+ #(-3d0 -1d0 1d0 3d0))

(defstruct (moonlab-surface-campaign
            (:constructor %make-moonlab-surface-campaign
                (&key spec implementation-id required-abi tolerance id)))
  "A content-bound request covering Moonlab surfaces beyond unitary gates."
  spec implementation-id required-abi tolerance id)

(defstruct (moonlab-surface-diagnostic-bundle
            (:constructor %make-moonlab-surface-diagnostic-bundle
                (&key id campaign-id spec implementation-id required-abi
                      observed-abi tolerance verdict outcome earliest-boundary
                      observations error-status)))
  "Path-free evidence from an isolated consumer of Moonlab's public symbols."
  id campaign-id spec implementation-id required-abi observed-abi tolerance
  verdict outcome earliest-boundary observations error-status)

(defun %normalize-moonlab-surface-spec (spec)
  (unless (and (%proper-list-p spec)
               (equal (loop for tail on spec by #'cddr collect (first tail))
                      +moonlab-surface-spec-keys+))
    (error "Surface spec keys/order must be ~S, got ~S"
           +moonlab-surface-spec-keys+ spec))
  (let ((measurement (getf spec :measurement-step))
        (channel (getf spec :channel-step))
        (phase (getf spec :qgt-phase))
        (gradient-a (getf spec :gradient-a))
        (gradient-b (getf spec :gradient-b)))
    (unless (typep measurement '(integer 0 15))
      (error "MEASUREMENT-STEP must be in [0,15]"))
    (unless (typep channel '(integer 0 16))
      (error "CHANNEL-STEP must be in [0,16]"))
    (unless (typep phase '(integer 0 3))
      (error "QGT-PHASE must select one of four gapped QWZ regimes"))
    (unless (and (typep gradient-a '(integer -16 16))
                 (typep gradient-b '(integer -16 16)))
      (error "Gradient lattice coordinates must be in [-16,16]"))
    (list :measurement-step measurement :channel-step channel :qgt-phase phase
          :gradient-a gradient-a :gradient-b gradient-b)))

(defun %moonlab-surface-campaign-content-form
    (spec implementation-id required-abi tolerance)
  (list :schema +moonlab-surface-campaign-schema+
        :spec spec :implementation-id implementation-id
        :required-abi required-abi :tolerance tolerance
        :required-capabilities
        '(:measurement :channels :qgt-callback :native-gradient
          :owned-state :located-error)
        :optional-capabilities '(:gpu-state)))

(defun make-moonlab-surface-campaign
    (spec &key implementation-id (required-abi '(0 6 0)) (tolerance 1d-8))
  "Create a deterministic Moonlab public-consumer surface campaign.

SPEC is an integer lattice, rather than raw floats, so generation, shrinking,
JSON, and replay all select exactly the same physical parameters."
  (unless (%safe-toolchain-id-p implementation-id)
    (error "IMPLEMENTATION-ID must be a path-free artifact identifier"))
  (unless (%moonlab-abi-p required-abi)
    (error "REQUIRED-ABI must be a three-part nonnegative version"))
  (let ((epsilon (coerce tolerance 'double-float))
        (owned (%normalize-moonlab-surface-spec spec))
        (abi (copy-list required-abi)))
    (unless (and (%finite-double-p epsilon) (< 0d0 epsilon 1d-3))
      (error "TOLERANCE must be finite and in (0,1e-3)"))
    (%make-moonlab-surface-campaign
     :spec owned :implementation-id implementation-id :required-abi abi
     :tolerance epsilon
     :id (cid:content-id-long
          (%moonlab-surface-campaign-content-form
           owned implementation-id abi epsilon)))))

(defun %surface-parameter (integer scale)
  (/ (coerce integer 'double-float) scale))

(defun %moonlab-surface-request (spec)
  (with-output-to-string (stream)
    (format stream "~A~%MEASUREMENT ~A~%CHANNEL ~A~%QGT-MASS ~A~%GRADIENT ~A ~A~%END~%"
            +moonlab-surface-request-version+
            (%c-double-token (%surface-parameter
                              (getf spec :measurement-step) 16d0))
            (%c-double-token (%surface-parameter
                              (getf spec :channel-step) 16d0))
            (%c-double-token (aref +moonlab-qgt-masses+
                                   (getf spec :qgt-phase)))
            (%c-double-token (%surface-parameter
                              (getf spec :gradient-a) 8d0))
            (%c-double-token (%surface-parameter
                              (getf spec :gradient-b) 8d0)))))

(defun %parse-exact-row (row name length)
  (and (= (length row) length) (string= (first row) name) row))

(defun %parse-moonlab-surface-output (text)
  (let* ((lines (remove-if (lambda (line) (zerop (length line)))
                           (uiop:split-string text
                                              :separator '(#\Newline #\Return))))
         (cursor lines))
    (labels ((next () (prog1 (first cursor) (setf cursor (rest cursor))))
             (row () (%words (or (next) "")))
             (integer (token) (%parse-integer-token token))
             (double (token) (%parse-double-token token)))
      (unless (string= (or (next) "") +moonlab-surface-request-version+)
        (return-from %parse-moonlab-surface-output nil))
      (let* ((abi-row (%parse-exact-row (row) "ABI" 4))
             (measurement-row (%parse-exact-row (row) "MEASUREMENT" 5))
             (channels-row (%parse-exact-row (row) "CHANNELS" 3))
             (qgt-row (%parse-exact-row (row) "QGT" 6))
             (qgt-reference-row
               (%parse-exact-row (row) "QGT-REFERENCE" 3))
             (gradient-row (%parse-exact-row (row) "GRADIENT" 10))
             (ownership-row (%parse-exact-row (row) "OWNERSHIP" 4))
             (gpu-row (%parse-exact-row (row) "GPU" 4)))
        (unless (and abi-row measurement-row channels-row qgt-row
                     qgt-reference-row gradient-row ownership-row gpu-row
                     (string= (or (next) "") "END") (null cursor))
          (return-from %parse-moonlab-surface-output nil))
        (let ((abi (mapcar #'integer (rest abi-row)))
              (gpu-kind (second gpu-row)))
          (unless (and (%moonlab-abi-p abi)
                       (member gpu-kind '("PASS" "UNAVAILABLE" "FAIL")
                               :test #'string=))
            (return-from %parse-moonlab-surface-output nil))
          (let ((observations
                  (list
                   :measurement
                   (list :probability-one (double (second measurement-row))
                         :outcome (integer (third measurement-row))
                         :collapsed-probability-one
                         (double (fourth measurement-row))
                         :invalid-status (integer (fifth measurement-row)))
                   :channels
                   (list :max-completeness-deviation
                         (double (second channels-row))
                         :invalid-result (double (third channels-row)))
                   :qgt
                   (list :status (integer (second qgt-row))
                         :callback-count (integer (third qgt-row))
                         :chern (double (fourth qgt-row))
                         :expected (integer (fifth qgt-row))
                         :null-callback-rejected (integer (sixth qgt-row))
                         :reference-status
                         (integer (second qgt-reference-row))
                         :reference-chern
                         (double (third qgt-reference-row)))
                   :gradient
                   (list :status (integer (second gradient-row))
                         :native (list (double (third gradient-row))
                                       (double (fourth gradient-row)))
                         :finite-difference
                         (list (double (fifth gradient-row))
                               (double (sixth gradient-row)))
                         :max-residual (double (seventh gradient-row))
                         :null-status (integer (eighth gradient-row))
                         :count-status (integer (ninth gradient-row))
                         :nondegenerate (integer (tenth gradient-row)))
                   :ownership
                   (list :created (integer (second ownership-row))
                         :invalid-target-status (integer (third ownership-row))
                         :zero-qubit-rejected (integer (fourth ownership-row)))
                   :gpu
                   (list :verdict (cond ((string= gpu-kind "PASS") :pass)
                                        ((string= gpu-kind "UNAVAILABLE")
                                         :unavailable)
                                        (t :fail))
                         :status (integer (third gpu-row))
                         :max-amplitude-error (double (fourth gpu-row))))))
            (if (some #'null
                      (labels ((leaves (value)
                                 (cond ((null value) (list nil))
                                       ((consp value)
                                        (mapcan #'leaves value))
                                       (t (list value)))))
                        (leaves observations)))
                nil
                (list :abi abi :observations observations))))))))

(defun %near-p (left right tolerance)
  (and (%finite-double-p left) (%finite-double-p right)
       (<= (abs (- left right)) tolerance)))

(defun %surface-laws (spec observations tolerance)
  "Return (values pass boundary status). GPU unavailability is not promoted."
  (let* ((measurement (getf observations :measurement))
         (channels (getf observations :channels))
         (qgt (getf observations :qgt))
         (gradient (getf observations :gradient))
         (ownership (getf observations :ownership))
         (gpu (getf observations :gpu))
         (measurement-pass
           (and (%near-p (getf measurement :probability-one) 0.5d0 tolerance)
                (member (getf measurement :outcome) '(0 1))
                (%near-p (getf measurement :collapsed-probability-one)
                         (coerce (getf measurement :outcome) 'double-float)
                         tolerance)
                (not (zerop (getf measurement :invalid-status)))))
         (channels-pass
           (and (%finite-double-p
                 (getf channels :max-completeness-deviation))
                (<= 0d0 (getf channels :max-completeness-deviation) tolerance)
                (minusp (getf channels :invalid-result))))
         (expected-qgt (case (getf spec :qgt-phase)
                         ((0 3) 0) (1 1) (2 -1)))
         (qgt-pass
           (and (= expected-qgt (getf qgt :reference-status))
                (%near-p (getf qgt :reference-chern)
                         (coerce expected-qgt 'double-float) (* 10d0 tolerance))
                (zerop (getf qgt :status))
                (plusp (getf qgt :callback-count))
                (= expected-qgt (getf qgt :expected))
                (%near-p (getf qgt :chern)
                         (getf qgt :reference-chern) (* 10d0 tolerance))
                (= 1 (getf qgt :null-callback-rejected))))
         (gradient-pass
           (and (zerop (getf gradient :status))
                (every #'%finite-double-p (getf gradient :native))
                (every #'%finite-double-p (getf gradient :finite-difference))
                (%finite-double-p (getf gradient :max-residual))
                (<= (getf gradient :max-residual) (* 10d0 tolerance))
                (= -1 (getf gradient :null-status))
                (= -2 (getf gradient :count-status))
                (= 1 (getf gradient :nondegenerate))))
         (ownership-pass
           (and (= 1 (getf ownership :created))
                (not (zerop (getf ownership :invalid-target-status)))
                (= 1 (getf ownership :zero-qubit-rejected))))
         (gpu-pass (member (getf gpu :verdict) '(:pass :unavailable))))
    (cond ((not measurement-pass)
           (values nil :moonlab-measurement :measurement-law))
          ((not channels-pass)
           (values nil :moonlab-channels :channel-law))
          ((not qgt-pass)
           (values nil :moonlab-qgt-callback :qgt-law))
          ((not gradient-pass)
           (values nil :moonlab-native-gradient :gradient-law))
          ((not ownership-pass)
           (values nil :moonlab-ownership :ownership-law))
          ((not gpu-pass)
           (values nil :moonlab-gpu :gpu-error))
          (t (values t :complete nil)))))

(defun %moonlab-surface-bundle-content-form
    (campaign-id spec implementation-id required-abi observed-abi tolerance
     verdict outcome earliest-boundary observations error-status)
  (list :schema +moonlab-surface-bundle-schema+ :campaign-id campaign-id
        :spec spec :implementation-id implementation-id
        :required-abi required-abi :observed-abi observed-abi
        :tolerance tolerance :verdict verdict :outcome outcome
        :earliest-boundary earliest-boundary :observations observations
        :error-status error-status))

(defun %finish-moonlab-surface-bundle
    (campaign &key observed-abi verdict outcome earliest-boundary observations
                   error-status)
  (let* ((spec (copy-list (moonlab-surface-campaign-spec campaign)))
         (content
           (%moonlab-surface-bundle-content-form
            (moonlab-surface-campaign-id campaign) spec
            (moonlab-surface-campaign-implementation-id campaign)
            (moonlab-surface-campaign-required-abi campaign) observed-abi
            (moonlab-surface-campaign-tolerance campaign) verdict outcome
            earliest-boundary observations error-status)))
    (%make-moonlab-surface-diagnostic-bundle
     :id (cid:content-id-long content)
     :campaign-id (moonlab-surface-campaign-id campaign) :spec spec
     :implementation-id (moonlab-surface-campaign-implementation-id campaign)
     :required-abi (copy-list (moonlab-surface-campaign-required-abi campaign))
     :observed-abi (copy-list observed-abi)
     :tolerance (moonlab-surface-campaign-tolerance campaign)
     :verdict verdict :outcome outcome :earliest-boundary earliest-boundary
     :observations (copy-tree observations) :error-status error-status)))

(defun run-moonlab-surface-campaign
    (campaign &key moonlab-command moonlab-library)
  "Run the measurement/channel/QGT/gradient/ownership/GPU public probe."
  (unless (moonlab-surface-campaign-p campaign)
    (error "Expected a MOONLAB-SURFACE-CAMPAIGN"))
  (unless (and moonlab-command moonlab-library)
    (return-from run-moonlab-surface-campaign
      (%finish-moonlab-surface-bundle
       campaign :verdict :unavailable :outcome :unavailable
       :earliest-boundary :availability :error-status :probe-unavailable)))
  (handler-case
      (with-input-from-string
          (input (%moonlab-surface-request
                  (moonlab-surface-campaign-spec campaign)))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program
             (append (uiop:ensure-list moonlab-command) (list moonlab-library))
             :input input :output :string :error-output :string
             :ignore-error-status t)
          (declare (ignore error-output))
          (unless (zerop exit-code)
            (return-from run-moonlab-surface-campaign
              (%finish-moonlab-surface-bundle
               campaign :verdict :fail :outcome :error
               :earliest-boundary :native-abi-transfer
               :error-status :probe-exit)))
          (let ((parsed (%parse-moonlab-surface-output output)))
            (unless parsed
              (return-from run-moonlab-surface-campaign
                (%finish-moonlab-surface-bundle
                 campaign :verdict :fail :outcome :error
                 :earliest-boundary :native-abi-transfer
                 :error-status :malformed-probe-output)))
            (let ((abi (getf parsed :abi))
                  (observations (getf parsed :observations)))
              (unless (%abi-at-least-p
                       abi (moonlab-surface-campaign-required-abi campaign))
                (return-from run-moonlab-surface-campaign
                  (%finish-moonlab-surface-bundle
                   campaign :observed-abi abi :verdict :fail :outcome :refused
                   :earliest-boundary :abi-probe :error-status :abi-too-old)))
              (multiple-value-bind (passed boundary status)
                  (%surface-laws (moonlab-surface-campaign-spec campaign)
                                 observations
                                 (moonlab-surface-campaign-tolerance campaign))
                (%finish-moonlab-surface-bundle
                 campaign :observed-abi abi :verdict (if passed :pass :fail)
                 :outcome (if passed :pass :disagreement)
                 :earliest-boundary boundary :observations observations
                 :error-status status))))))
    (error ()
      (%finish-moonlab-surface-bundle
       campaign :verdict :unavailable :outcome :unavailable
       :earliest-boundary :availability :error-status :probe-unavailable))))

(defun moonlab-surface-diagnostic-bundle->form (bundle)
  (unless (moonlab-surface-diagnostic-bundle-p bundle)
    (error "Expected a MOONLAB-SURFACE-DIAGNOSTIC-BUNDLE"))
  (list :id (moonlab-surface-diagnostic-bundle-id bundle) :content
        (%moonlab-surface-bundle-content-form
         (moonlab-surface-diagnostic-bundle-campaign-id bundle)
         (moonlab-surface-diagnostic-bundle-spec bundle)
         (moonlab-surface-diagnostic-bundle-implementation-id bundle)
         (moonlab-surface-diagnostic-bundle-required-abi bundle)
         (moonlab-surface-diagnostic-bundle-observed-abi bundle)
         (moonlab-surface-diagnostic-bundle-tolerance bundle)
         (moonlab-surface-diagnostic-bundle-verdict bundle)
         (moonlab-surface-diagnostic-bundle-outcome bundle)
         (moonlab-surface-diagnostic-bundle-earliest-boundary bundle)
         (moonlab-surface-diagnostic-bundle-observations bundle)
         (moonlab-surface-diagnostic-bundle-error-status bundle))))

(defun verify-moonlab-surface-diagnostic-bundle (bundle)
  "Recompute identity and every surface law; a stored PASS has no authority."
  (and (moonlab-surface-diagnostic-bundle-p bundle)
       (handler-case
           (let* ((spec (%normalize-moonlab-surface-spec
                         (moonlab-surface-diagnostic-bundle-spec bundle)))
                  (implementation-id
                    (moonlab-surface-diagnostic-bundle-implementation-id bundle))
                  (required
                    (moonlab-surface-diagnostic-bundle-required-abi bundle))
                  (observed
                    (moonlab-surface-diagnostic-bundle-observed-abi bundle))
                  (tolerance
                    (moonlab-surface-diagnostic-bundle-tolerance bundle))
                  (verdict (moonlab-surface-diagnostic-bundle-verdict bundle))
                  (outcome (moonlab-surface-diagnostic-bundle-outcome bundle))
                  (boundary
                    (moonlab-surface-diagnostic-bundle-earliest-boundary bundle))
                  (observations
                    (moonlab-surface-diagnostic-bundle-observations bundle))
                  (status
                    (moonlab-surface-diagnostic-bundle-error-status bundle))
                  (campaign-content
                    (%moonlab-surface-campaign-content-form
                     spec implementation-id required tolerance))
                  (bundle-content
                    (%moonlab-surface-bundle-content-form
                     (moonlab-surface-diagnostic-bundle-campaign-id bundle)
                     spec implementation-id required observed tolerance verdict
                     outcome boundary observations status)))
             (and (%safe-toolchain-id-p implementation-id)
                  (%moonlab-abi-p required)
                  (%finite-double-p tolerance) (< 0d0 tolerance 1d-3)
                  (string= (moonlab-surface-diagnostic-bundle-campaign-id bundle)
                           (cid:content-id-long campaign-content))
                  (string= (moonlab-surface-diagnostic-bundle-id bundle)
                           (cid:content-id-long bundle-content))
                  (case verdict
                    (:pass
                     (and (%abi-at-least-p observed required)
                          (eq outcome :pass) (eq boundary :complete)
                          (null status)
                          (nth-value 0
                           (%surface-laws spec observations tolerance))))
                    (:fail
                     (case outcome
                       (:disagreement
                        (multiple-value-bind (passed expected-boundary
                                                     expected-status)
                            (%surface-laws spec observations tolerance)
                          (and (not passed)
                               (eq boundary expected-boundary)
                               (eq status expected-status))))
                       (:refused
                        (and (not (%abi-at-least-p observed required))
                             (eq boundary :abi-probe) (eq status :abi-too-old)
                             (null observations)))
                       (:error
                        (and (eq boundary :native-abi-transfer)
                             (member status
                                     '(:probe-exit :malformed-probe-output))
                             (null observations)))
                       (otherwise nil)))
                    (:unavailable
                     (and (eq outcome :unavailable)
                          (eq boundary :availability)
                          (eq status :probe-unavailable)
                          (null observed) (null observations)))
                    (otherwise nil))))
         (error () nil))))

(defun replay-moonlab-surface-diagnostic-bundle
    (bundle &key moonlab-command moonlab-library)
  (unless (verify-moonlab-surface-diagnostic-bundle bundle)
    (error "Refusing to replay an invalid Moonlab surface bundle"))
  (let* ((campaign
           (make-moonlab-surface-campaign
            (moonlab-surface-diagnostic-bundle-spec bundle)
            :implementation-id
            (moonlab-surface-diagnostic-bundle-implementation-id bundle)
            :required-abi
            (moonlab-surface-diagnostic-bundle-required-abi bundle)
            :tolerance
            (moonlab-surface-diagnostic-bundle-tolerance bundle)))
         (fresh (run-moonlab-surface-campaign
                 campaign :moonlab-command moonlab-command
                 :moonlab-library moonlab-library)))
    (values fresh
            (string= (moonlab-surface-diagnostic-bundle-id bundle)
                     (moonlab-surface-diagnostic-bundle-id fresh)))))
