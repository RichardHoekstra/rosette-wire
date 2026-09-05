;;;; moonlab-diagnostics.lisp --- public-ABI conformance evidence for Moonlab.

(in-package #:rosette-wire-diagnostics)

(defconstant +moonlab-campaign-schema+ :rosette-wire-moonlab-campaign/v1)
(defconstant +moonlab-bundle-schema+ :rosette-wire-moonlab-diagnostic-bundle/v1)
(defparameter +moonlab-request-version+ "ROSETTE-MOONLAB/1")
(defparameter +moonlab-gates+
  '("h" "x" "y" "z" "s" "t" "rx" "ry" "rz" "cnot" "cz" "swap"))

(defstruct (moonlab-campaign
            (:constructor %make-moonlab-campaign
                (&key circuit implementation-id required-abi tolerance id)))
  circuit implementation-id required-abi tolerance id)

(defstruct (moonlab-diagnostic-bundle
            (:constructor %make-moonlab-diagnostic-bundle
                (&key id campaign-id circuit circuit-id implementation-id
                      required-abi observed-abi tolerance verdict outcome
                      earliest-boundary observations error-status)))
  id campaign-id circuit circuit-id implementation-id required-abi observed-abi
  tolerance verdict outcome earliest-boundary observations error-status)

(defun %finite-double-p (value)
  (and (typep value 'double-float)
       (= value value)
       (<= (- most-positive-double-float) value most-positive-double-float)))

(defun %moonlab-abi-p (value)
  (and (listp value) (= (length value) 3)
       (every (lambda (part) (typep part '(integer 0 *))) value)))

(defun %abi-at-least-p (actual required)
  (and (%moonlab-abi-p actual) (%moonlab-abi-p required)
       (or (> (first actual) (first required))
           (and (= (first actual) (first required))
                (or (> (second actual) (second required))
                    (and (= (second actual) (second required))
                         (>= (third actual) (third required))))))))

(defun %proper-list-p (value)
  (loop for tail = value then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %target-p (target n-qubits)
  (typep target `(integer 0 (,n-qubits))))

(defun %valid-moonlab-gate-p (row n-qubits)
  (and (%proper-list-p row)
       (stringp (first row))
       (member (first row) +moonlab-gates+ :test #'string=)
       (cond
         ((member (first row) '("h" "x" "y" "z" "s" "t") :test #'string=)
          (and (= (length row) 2) (%target-p (second row) n-qubits)))
         ((member (first row) '("rx" "ry" "rz") :test #'string=)
          (and (= (length row) 3)
               (realp (second row))
               (%finite-double-p (coerce (second row) 'double-float))
               (%target-p (third row) n-qubits)))
         (t
          (and (= (length row) 3)
               (%target-p (second row) n-qubits)
               (%target-p (third row) n-qubits)
               (/= (second row) (third row)))))))

(defun %normalize-moonlab-circuit (circuit)
  (unless (and (%proper-list-p circuit)
               (equal (loop for tail on circuit by #'cddr collect (first tail))
                      '(:qubits :basis-index :gates)))
    (error "Circuit must be (:QUBITS n :BASIS-INDEX i :GATES rows), got ~S"
           circuit))
  (let* ((n-qubits (getf circuit :qubits))
         (basis-index (getf circuit :basis-index))
         (gates (getf circuit :gates)))
    (unless (typep n-qubits '(integer 1 20))
      (error "Moonlab diagnostic circuits require 1..20 qubits"))
    (unless (typep basis-index `(integer 0 (,(ash 1 n-qubits))))
      (error "Basis index ~S is outside the ~D-qubit state" basis-index n-qubits))
    (unless (and (%proper-list-p gates) (<= (length gates) 256)
                 (every (lambda (row) (%valid-moonlab-gate-p row n-qubits)) gates))
      (error "Malformed or over-budget Moonlab gate rows: ~S" gates))
    (list :qubits n-qubits :basis-index basis-index
          :gates (mapcar (lambda (row)
                           (cons (string-downcase (first row))
                                 (mapcar (lambda (value)
                                           (if (realp value)
                                               (if (integerp value) value
                                                   (coerce value 'double-float))
                                               value))
                                         (rest row))))
                         gates))))

(defun %moonlab-campaign-content-form
    (circuit implementation-id required-abi tolerance)
  (list :schema +moonlab-campaign-schema+
        :circuit circuit
        :implementation-id implementation-id
        :required-abi required-abi
        :comparison (list :global-phase-invariant-fidelity tolerance)
        :required-capabilities '(:unitary-circuit :owned-state :located-error)))

(defun make-moonlab-campaign
    (circuit &key implementation-id (required-abi '(0 6 0))
                  (tolerance 1d-12))
  "Make a deterministic downstream campaign over Moonlab's public ABI."
  (unless (%safe-toolchain-id-p implementation-id)
    (error "IMPLEMENTATION-ID must be a path-free artifact identifier, got ~S"
           implementation-id))
  (unless (%moonlab-abi-p required-abi)
    (error "REQUIRED-ABI must be a three-part nonnegative version"))
  (let ((epsilon (coerce tolerance 'double-float)))
    (unless (and (%finite-double-p epsilon) (< 0d0 epsilon 1d-3))
      (error "TOLERANCE must be finite and in (0, 1e-3)"))
    (let* ((owned (%normalize-moonlab-circuit circuit))
           (abi (copy-list required-abi))
           (id (cid:content-id-long
                (%moonlab-campaign-content-form
                 owned implementation-id abi epsilon))))
      (%make-moonlab-campaign
       :circuit owned :implementation-id implementation-id
       :required-abi abi :tolerance epsilon :id id))))

(defun %wire-state-name (index)
  (intern (format nil "WIRE-Q~D" index) '#:rosette-wire-diagnostics))

(defun %qir-gate (row input output)
  (let ((name (first row)))
    (cond
      ((member name '("h" "x" "y" "z" "s" "t") :test #'string=)
       (list :apply output input (intern (string-upcase name) :keyword)
             (second row)))
      ((member name '("rx" "ry" "rz") :test #'string=)
       (list :apply output input
             (list (intern (string-upcase name) :keyword) (second row))
             (third row)))
      (t
       (list :apply output input (intern (string-upcase name) :keyword)
             (second row) (third row))))))

(defun %c-double-token (value)
  (substitute #\e #\d
              (string-downcase
               (format nil "~,17E" (coerce value 'double-float)))))

(defun %circuit-qir-form (circuit)
  (let* ((n-qubits (getf circuit :qubits))
         (basis-index (getf circuit :basis-index))
         (gates (getf circuit :gates))
         (statements
           (loop for row in gates
                 for index from 0
                 collect (%qir-gate row (%wire-state-name index)
                                    (%wire-state-name (1+ index))))))
    (list :quantum-unit
          (append
           (list :program 'wire-moonlab nil (list (list :qstate n-qubits))
                 (list :prepare (%wire-state-name 0) n-qubits basis-index))
           statements
           (list (list :return (%wire-state-name (length gates))))))))

(defun %rosette-circuit-state (circuit)
  (let* ((program (qir:make-quantum-program (%circuit-qir-form circuit)))
         (tape (qir:lower-quantum-tape program))
         (result (qvm:execute-quantum-tape tape nil))
         (values (qvm:quantum-state-values
                  (qvm:execution-result-state result))))
    (loop for value across values
          collect (list (coerce (realpart value) 'double-float)
                        (coerce (imagpart value) 'double-float)))))

(defun %moonlab-request (circuit)
  (with-output-to-string (stream)
    (format stream "~A~%QUBITS ~D~%BASIS ~D~%GATES ~D~%"
            +moonlab-request-version+
            (getf circuit :qubits)
            (getf circuit :basis-index)
            (length (getf circuit :gates)))
    ;; Moonlab numbers qubit zero from the LSB; quantum-vm uses MSB-first.
    (let ((last (1- (getf circuit :qubits))))
      (dolist (row (getf circuit :gates))
        (let ((name (string-upcase (first row))))
          (cond
            ((member (first row) '("h" "x" "y" "z" "s" "t") :test #'string=)
             (format stream "~A ~D~%" name (- last (second row))))
            ((member (first row) '("rx" "ry" "rz") :test #'string=)
             (format stream "~A ~D ~A~%" name (- last (third row))
                     (%c-double-token (second row))))
            (t
             (format stream "~A ~D ~D~%" name
                     (- last (second row)) (- last (third row))))))))
    (format stream "END~%")))

(defun %parse-integer-token (token)
  (handler-case (parse-integer token :junk-allowed nil) (error () nil)))

(defun %parse-double-token (token)
  (let* ((copy (copy-seq token))
         (marker (position-if (lambda (character) (find character "eE")) copy)))
    (when marker (setf (char copy marker) #\d))
    (handler-case
        (let ((*read-eval* nil))
          (multiple-value-bind (value end) (read-from-string copy nil nil)
            (when (and value (= end (length copy)) (realp value))
              (let ((double (coerce value 'double-float)))
                (and (%finite-double-p double) double)))))
      (error () nil))))

(defun %words (line)
  (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
          :test #'string=))

(defun %parse-moonlab-output (text expected-size)
  (let* ((lines (remove-if (lambda (line) (zerop (length line)))
                           (uiop:split-string text :separator '(#\Newline #\Return))))
         (cursor lines))
    (labels ((next () (prog1 (first cursor) (setf cursor (rest cursor))))
             (fields () (%words (or (next) ""))))
      (unless (string= (or (next) "") +moonlab-request-version+) (return-from %parse-moonlab-output nil))
      (let* ((abi-row (fields))
             (ownership-row (fields))
             (error-row (fields))
             (state-row (fields))
             (abi (mapcar #'%parse-integer-token (rest abi-row)))
             (size (%parse-integer-token (second state-row))))
        (unless (and (string= (first abi-row) "ABI") (%moonlab-abi-p abi)
                     (equal ownership-row '("OWNERSHIP" "1"))
                     (equal error-row '("ERROR-PATH" "1"))
                     (string= (first state-row) "STATE")
                     (= size expected-size))
          (return-from %parse-moonlab-output nil))
        (let ((state
                (loop repeat size
                      for row = (fields)
                      for re = (and (string= (first row) "AMP")
                                    (%parse-double-token (second row)))
                      for im = (and (= (length row) 3)
                                    (%parse-double-token (third row)))
                      unless (and re im) do (return-from %parse-moonlab-output nil)
                      collect (list re im))))
          (unless (and (string= (or (next) "") "END") (null cursor))
            (return-from %parse-moonlab-output nil))
          (list :abi abi :ownership t :error-path t :state state))))))

(defun %state-vector-p (state expected-size)
  (and (%proper-list-p state) (= (length state) expected-size)
       (every (lambda (value)
                (and (%proper-list-p value) (= (length value) 2)
                     (every #'%finite-double-p value)))
              state)))

(defun %state-fidelity (left right)
  (let ((inner #c(0d0 0d0)) (left-norm 0d0) (right-norm 0d0))
    (loop for (lr li) in left for (rr ri) in right
          for a = (complex lr li) for b = (complex rr ri) do
            (incf inner (* (conjugate a) b))
            (incf left-norm (abs (* (conjugate a) a)))
            (incf right-norm (abs (* (conjugate b) b))))
    (if (or (zerop left-norm) (zerop right-norm))
        0d0
        (max 0d0
             (min 1d0
                  (/ (abs (* (conjugate inner) inner))
                     left-norm right-norm))))))

(defun %moonlab-bundle-content-form
    (campaign-id circuit circuit-id implementation-id required-abi observed-abi
     tolerance verdict outcome earliest-boundary observations error-status)
  (list :schema +moonlab-bundle-schema+ :campaign-id campaign-id
        :circuit circuit :circuit-id circuit-id
        :implementation-id implementation-id :required-abi required-abi
        :observed-abi observed-abi :tolerance tolerance :verdict verdict
        :outcome outcome :earliest-boundary earliest-boundary
        :observations observations :error-status error-status))

(defun %finish-moonlab-bundle
    (campaign &key observed-abi verdict outcome earliest-boundary observations
                   error-status)
  (let* ((circuit (copy-tree (moonlab-campaign-circuit campaign)))
         (circuit-id (cid:content-id-long circuit))
         (content (%moonlab-bundle-content-form
                   (moonlab-campaign-id campaign) circuit circuit-id
                   (moonlab-campaign-implementation-id campaign)
                   (moonlab-campaign-required-abi campaign) observed-abi
                   (moonlab-campaign-tolerance campaign) verdict outcome
                   earliest-boundary observations error-status)))
    (%make-moonlab-diagnostic-bundle
     :id (cid:content-id-long content) :campaign-id (moonlab-campaign-id campaign)
     :circuit circuit :circuit-id circuit-id
     :implementation-id (moonlab-campaign-implementation-id campaign)
     :required-abi (copy-list (moonlab-campaign-required-abi campaign))
     :observed-abi (copy-list observed-abi)
     :tolerance (moonlab-campaign-tolerance campaign) :verdict verdict
     :outcome outcome :earliest-boundary earliest-boundary
     :observations (copy-tree observations) :error-status error-status)))

(defun run-moonlab-campaign (campaign &key moonlab-command moonlab-library)
  "Compare quantum-vm with an isolated Moonlab public-ABI probe."
  (unless (moonlab-campaign-p campaign)
    (error "Expected a MOONLAB-CAMPAIGN, got ~S" campaign))
  (unless (and moonlab-command moonlab-library)
    (return-from run-moonlab-campaign
      (%finish-moonlab-bundle campaign :verdict :unavailable
       :outcome :unavailable :earliest-boundary :availability
       :error-status :probe-unavailable)))
  (handler-case
      (let* ((circuit (moonlab-campaign-circuit campaign))
             (rosette-state (%rosette-circuit-state circuit))
             (command (append (uiop:ensure-list moonlab-command)
                              (list moonlab-library))))
        (with-input-from-string (input (%moonlab-request circuit))
          (multiple-value-bind (output error-output exit-code)
              (uiop:run-program command :input input
                                :output :string :error-output :string
                                :ignore-error-status t)
            (declare (ignore error-output))
            (unless (zerop exit-code)
              (return-from run-moonlab-campaign
                (%finish-moonlab-bundle campaign :verdict :unavailable
                 :outcome :unavailable :earliest-boundary :availability
                 :error-status :probe-unavailable)))
            (let ((parsed (%parse-moonlab-output output (ash 1 (getf circuit :qubits)))))
            (unless parsed
              (return-from run-moonlab-campaign
                (%finish-moonlab-bundle campaign :verdict :fail :outcome :error
                 :earliest-boundary :native-abi-transfer
                 :error-status :malformed-probe-output)))
            (let ((abi (getf parsed :abi)))
              (unless (%abi-at-least-p abi (moonlab-campaign-required-abi campaign))
                (return-from run-moonlab-campaign
                  (%finish-moonlab-bundle campaign :observed-abi abi
                   :verdict :fail :outcome :refused
                   :earliest-boundary :abi-probe :error-status :abi-too-old)))
              (let* ((moonlab-state (getf parsed :state))
                     (fidelity (%state-fidelity rosette-state moonlab-state))
                     (observations
                       (list :rosette-state rosette-state :moonlab-state moonlab-state
                             :fidelity fidelity :ownership (getf parsed :ownership)
                             :error-path (getf parsed :error-path)))
                     (pass (>= fidelity
                               (- 1d0 (moonlab-campaign-tolerance campaign)))))
                (%finish-moonlab-bundle
                 campaign :observed-abi abi :verdict (if pass :pass :fail)
                 :outcome (if pass :pass :disagreement)
                 :earliest-boundary (if pass :complete :moonlab-computation)
                 :observations observations
                 :error-status nil)))))))
    (error ()
      (%finish-moonlab-bundle campaign :verdict :fail :outcome :error
       :earliest-boundary :quantum-ir-admission :error-status :invalid-circuit))))

(defun moonlab-diagnostic-bundle->form (bundle)
  "Return the canonical portable form; commands and local paths are absent."
  (unless (moonlab-diagnostic-bundle-p bundle)
    (error "Expected a MOONLAB-DIAGNOSTIC-BUNDLE, got ~S" bundle))
  (list :id (moonlab-diagnostic-bundle-id bundle) :content
        (%moonlab-bundle-content-form
         (moonlab-diagnostic-bundle-campaign-id bundle)
         (moonlab-diagnostic-bundle-circuit bundle)
         (moonlab-diagnostic-bundle-circuit-id bundle)
         (moonlab-diagnostic-bundle-implementation-id bundle)
         (moonlab-diagnostic-bundle-required-abi bundle)
         (moonlab-diagnostic-bundle-observed-abi bundle)
         (moonlab-diagnostic-bundle-tolerance bundle)
         (moonlab-diagnostic-bundle-verdict bundle)
         (moonlab-diagnostic-bundle-outcome bundle)
         (moonlab-diagnostic-bundle-earliest-boundary bundle)
         (moonlab-diagnostic-bundle-observations bundle)
         (moonlab-diagnostic-bundle-error-status bundle))))

(defun verify-moonlab-diagnostic-bundle (bundle)
  "Recompute identities, the independent VM state, fidelity, and verdict law."
  (and (moonlab-diagnostic-bundle-p bundle)
       (handler-case
           (let* ((circuit (%normalize-moonlab-circuit
                            (moonlab-diagnostic-bundle-circuit bundle)))
                  (implementation-id
                    (moonlab-diagnostic-bundle-implementation-id bundle))
                  (required (moonlab-diagnostic-bundle-required-abi bundle))
                  (observed (moonlab-diagnostic-bundle-observed-abi bundle))
                  (tolerance (moonlab-diagnostic-bundle-tolerance bundle))
                  (observations (moonlab-diagnostic-bundle-observations bundle))
                  (expected-reference (%rosette-circuit-state circuit))
                  (size (ash 1 (getf circuit :qubits)))
                  (moonlab-state (getf observations :moonlab-state))
                  (rosette-state (getf observations :rosette-state))
                  (fidelity (and (%state-vector-p rosette-state size)
                                 (%state-vector-p moonlab-state size)
                                 (%state-fidelity rosette-state moonlab-state)))
                  (content (%moonlab-bundle-content-form
                            (moonlab-diagnostic-bundle-campaign-id bundle)
                            circuit (moonlab-diagnostic-bundle-circuit-id bundle)
                            implementation-id required observed tolerance
                            (moonlab-diagnostic-bundle-verdict bundle)
                            (moonlab-diagnostic-bundle-outcome bundle)
                            (moonlab-diagnostic-bundle-earliest-boundary bundle)
                            observations
                            (moonlab-diagnostic-bundle-error-status bundle))))
             (and (%safe-toolchain-id-p implementation-id)
                  (%moonlab-abi-p required)
                  (%finite-double-p tolerance) (< 0d0 tolerance 1d-3)
                  (string= (moonlab-diagnostic-bundle-circuit-id bundle)
                           (cid:content-id-long circuit))
                  (string= (moonlab-diagnostic-bundle-campaign-id bundle)
                           (cid:content-id-long
                            (%moonlab-campaign-content-form
                             circuit implementation-id required tolerance)))
                  (string= (moonlab-diagnostic-bundle-id bundle)
                           (cid:content-id-long content))
                  (case (moonlab-diagnostic-bundle-verdict bundle)
                    (:pass
                     (and (%abi-at-least-p observed required)
                          (equal rosette-state expected-reference)
                          fidelity
                          (= fidelity (getf observations :fidelity))
                          (getf observations :ownership)
                          (getf observations :error-path)
                          (>= fidelity (- 1d0 tolerance))
                          (eq :pass (moonlab-diagnostic-bundle-outcome bundle))
                          (eq :complete
                              (moonlab-diagnostic-bundle-earliest-boundary bundle))
                          (null (moonlab-diagnostic-bundle-error-status bundle))))
                    (:fail
                     (case (moonlab-diagnostic-bundle-outcome bundle)
                       (:disagreement
                        (and (%abi-at-least-p observed required)
                             (equal rosette-state expected-reference) fidelity
                             (= fidelity (getf observations :fidelity))
                             (< fidelity (- 1d0 tolerance))
                             (eq :moonlab-computation
                                 (moonlab-diagnostic-bundle-earliest-boundary bundle))
                             (null (moonlab-diagnostic-bundle-error-status bundle))))
                       (:refused
                        (and (not (%abi-at-least-p observed required))
                             (null observations)
                             (eq :abi-probe
                                 (moonlab-diagnostic-bundle-earliest-boundary bundle))
                             (eq :abi-too-old
                                 (moonlab-diagnostic-bundle-error-status bundle))))
                       (:error
                        (and (null observations)
                             (member (moonlab-diagnostic-bundle-earliest-boundary bundle)
                                     '(:native-abi-transfer :quantum-ir-admission))
                             (moonlab-diagnostic-bundle-error-status bundle)))
                       (otherwise nil)))
                    (:unavailable
                     (and (eq :unavailable
                              (moonlab-diagnostic-bundle-outcome bundle))
                          (eq :availability
                              (moonlab-diagnostic-bundle-earliest-boundary bundle))
                          (null observations) (null observed)
                          (eq :probe-unavailable
                              (moonlab-diagnostic-bundle-error-status bundle))))
                    (otherwise nil))))
         (error () nil))))

(defun replay-moonlab-diagnostic-bundle
    (bundle &key moonlab-command moonlab-library)
  "Re-run a verified Moonlab campaign and report exact bundle identity parity."
  (unless (verify-moonlab-diagnostic-bundle bundle)
    (error "Refusing to replay an invalid Moonlab diagnostic bundle"))
  (let* ((campaign
           (make-moonlab-campaign
            (moonlab-diagnostic-bundle-circuit bundle)
            :implementation-id
            (moonlab-diagnostic-bundle-implementation-id bundle)
            :required-abi (moonlab-diagnostic-bundle-required-abi bundle)
            :tolerance (moonlab-diagnostic-bundle-tolerance bundle)))
         (fresh (run-moonlab-campaign
                 campaign :moonlab-command moonlab-command
                 :moonlab-library moonlab-library)))
    (values fresh (string= (moonlab-diagnostic-bundle-id bundle)
                           (moonlab-diagnostic-bundle-id fresh)))))
