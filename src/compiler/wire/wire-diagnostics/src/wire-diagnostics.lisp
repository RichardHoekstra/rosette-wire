;;;; wire-diagnostics.lisp --- replayable Eshkol floor diagnostics.

(in-package #:rosette-wire-diagnostics)

(defconstant +campaign-schema+ :rosette-wire-eshkol-campaign/v1)
(defconstant +bundle-schema+ :rosette-wire-diagnostic-bundle/v1)
(defconstant +comparison+ :exact-integer)
(defparameter +floors+ '(:direct :kernel-vm :eshkol-jit :eshkol-aot))

(defstruct (eshkol-campaign
            (:constructor %make-eshkol-campaign
                (&key source toolchain-id id)))
  "A content-bound request to compare one front-door program across Eshkol floors."
  source
  toolchain-id
  id)

(defstruct (diagnostic-bundle
            (:constructor %make-diagnostic-bundle
                (&key id campaign-id source program-id toolchain-id verdict
                      outcome earliest-boundary observations disagreements
                      error-status)))
  "A path-free, replayable observation of one Eshkol floor campaign."
  id
  campaign-id
  source
  program-id
  toolchain-id
  verdict
  outcome
  earliest-boundary
  observations
  disagreements
  error-status)

(defun %safe-toolchain-id-p (value)
  "Accept an artifact identity, never a command line or filesystem path."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character ".-_:+@" :test #'char=)))
              value)))

(defun %campaign-content-form (source toolchain-id)
  (list :schema +campaign-schema+
        :source source
        :required-floors +floors+
        :comparison +comparison+
        :toolchain-id toolchain-id))

(defun %normalize-eshkol-source (value)
  "Canonicalize source symbols into the stable KEYWORD package by name."
  (cond ((null value) nil)
        ((keywordp value) value)
        ;; INTERN preserves the supplied symbol-name string exactly; no reader
        ;; case conversion is involved.  :PRESERVE is therefore intentional.
        ((symbolp value) (intern (symbol-name value) :keyword)) ; :preserve
        ((consp value)
         (cons (%normalize-eshkol-source (car value))
               (%normalize-eshkol-source (cdr value))))
        ((stringp value) (copy-seq value))
        (t value)))

(defun make-eshkol-campaign (source &key toolchain-id)
  "Make a deterministic campaign without executing or validating SOURCE."
  (unless (%safe-toolchain-id-p toolchain-id)
    (error "TOOLCHAIN-ID must be a nonempty path-free artifact identifier, got ~S"
           toolchain-id))
  (let* ((owned-source (%normalize-eshkol-source source))
         (id (cid:content-id-long
              (%campaign-content-form owned-source toolchain-id))))
    (%make-eshkol-campaign
     :source owned-source :toolchain-id toolchain-id :id id)))

(defun %disagreement (left-name left-value right-name right-value)
  (unless (= left-value right-value)
    (list :left left-name :left-value left-value
          :right right-name :right-value right-value)))

(defun %report-observations (report)
  (list :direct (esh:eshkol-gate-report-direct-value report)
        :kernel-vm (esh:eshkol-gate-report-kernel-value report)
        :eshkol-jit (esh:eshkol-gate-report-jit-value report)
        :eshkol-aot (esh:eshkol-gate-report-aot-value report)))

(defun %report-disagreements (observations)
  (let ((direct (getf observations :direct)))
    (remove nil
            (list (%disagreement :direct direct
                                 :kernel-vm (getf observations :kernel-vm))
                  (%disagreement :direct direct
                                 :eshkol-jit (getf observations :eshkol-jit))
                  (%disagreement :direct direct
                                 :eshkol-aot (getf observations :eshkol-aot))))))

(defun %earliest-disagreement (disagreements)
  (if disagreements
      (getf (first disagreements) :right)
      :complete))

(defun %bundle-content-form
    (campaign-id source program-id toolchain-id verdict outcome
     earliest-boundary observations disagreements error-status)
  (list :schema +bundle-schema+
        :campaign-id campaign-id
        :source source
        :program-id program-id
        :toolchain-id toolchain-id
        :required-floors +floors+
        :comparison +comparison+
        :verdict verdict
        :outcome outcome
        :earliest-boundary earliest-boundary
        :observations observations
        :disagreements disagreements
        :error-status error-status))

(defun %finish-bundle
    (campaign &key program-id verdict outcome earliest-boundary
                   observations disagreements error-status)
  (let* ((source (copy-tree (eshkol-campaign-source campaign)))
         (campaign-id (eshkol-campaign-id campaign))
         (toolchain-id (eshkol-campaign-toolchain-id campaign))
         (content
           (%bundle-content-form
            campaign-id source program-id toolchain-id verdict outcome
            earliest-boundary observations disagreements error-status)))
    (%make-diagnostic-bundle
     :id (cid:content-id-long content)
     :campaign-id campaign-id
     :source source
     :program-id program-id
     :toolchain-id toolchain-id
     :verdict verdict
     :outcome outcome
     :earliest-boundary earliest-boundary
     :observations observations
     :disagreements disagreements
     :error-status error-status)))

(defun %condition-bundle (campaign condition program-id)
  (let* ((stage (esh:eshkol-backend-error-stage condition))
         (status (esh:eshkol-backend-error-status condition))
         (unavailable-p (typep condition 'esh:eshkol-unavailable)))
    (%finish-bundle
     campaign
     :program-id program-id
     :verdict (if unavailable-p :unavailable :fail)
     :outcome (if unavailable-p :unavailable :error)
     :earliest-boundary stage
     :observations nil
     :disagreements nil
     :error-status status)))

(defun %admission-bundle (campaign status)
  (%finish-bundle
   campaign
   :program-id nil
   :verdict :fail
   :outcome :refused
   :earliest-boundary :front-door-admission
   :observations nil
   :disagreements nil
   :error-status status))

(defun %admit-source (campaign)
  "Return PROGRAM-IR and NIL, or NIL and a content-bound refusal bundle."
  (handler-case
      (values
       (front:surface->program-ir
        (copy-tree (eshkol-campaign-source campaign)))
       nil)
    (front:front-door-refusal (condition)
      (values nil
              (%admission-bundle
               campaign (front:front-door-refusal-stage condition))))
    (error ()
      (values nil (%admission-bundle campaign :invalid-source)))))

(defun run-eshkol-campaign
    (campaign &key eshkol-command aot-run-prefix policy)
  "Run CAMPAIGN and always return a diagnostic bundle.

Runner commands and worker envelopes are intentionally excluded from the
bundle.  The caller-supplied TOOLCHAIN-ID is the public implementation
identity; commands may contain local paths or deployment details and therefore
remain ephemeral."
  (unless (eshkol-campaign-p campaign)
    (error "Expected an ESHKOL-CAMPAIGN, got ~S" campaign))
  (multiple-value-bind (program-ir refusal) (%admit-source campaign)
    (if refusal
        refusal
        (let ((program-id nil))
          (handler-case
              (let* ((emitted-id
                       (nth-value 1 (esh:emit-eshkol-source program-ir)))
                     (arguments
                       (append
                        (list program-ir
                              :eshkol-command eshkol-command
                              :aot-run-prefix aot-run-prefix)
                        (when policy (list :policy policy))))
                     (report
                       (progn
                         (setf program-id emitted-id)
                         (apply #'esh:gate-eshkol arguments)))
                     (observations (%report-observations report))
                     (disagreements (%report-disagreements observations))
                     (passed (and (esh:eshkol-gate-report-passed report)
                                  (null disagreements))))
                (%finish-bundle
                 campaign
                 :program-id program-id
                 :verdict (if passed :pass :fail)
                 :outcome (if passed :pass :disagreement)
                 :earliest-boundary (%earliest-disagreement disagreements)
                 :observations observations
                 :disagreements disagreements
                 :error-status nil))
            (esh:eshkol-backend-error (condition)
              (%condition-bundle campaign condition program-id)))))))

(defun %source-program-id (source)
  (handler-case
      (nth-value
       1
       (esh:emit-eshkol-source (front:surface->program-ir (copy-tree source))))
    (error () nil)))

(defun %valid-observations-p (observations)
  (and (listp observations)
       (equal (loop for tail on observations by #'cddr collect (first tail))
              +floors+)
       (every #'integerp
              (loop for tail on observations by #'cddr collect (second tail)))))

(defun diagnostic-bundle->form (bundle)
  "Return the canonical, path-free external form of BUNDLE."
  (unless (diagnostic-bundle-p bundle)
    (error "Expected a DIAGNOSTIC-BUNDLE, got ~S" bundle))
  (list :id (diagnostic-bundle-id bundle)
        :content
        (%bundle-content-form
         (diagnostic-bundle-campaign-id bundle)
         (diagnostic-bundle-source bundle)
         (diagnostic-bundle-program-id bundle)
         (diagnostic-bundle-toolchain-id bundle)
         (diagnostic-bundle-verdict bundle)
         (diagnostic-bundle-outcome bundle)
         (diagnostic-bundle-earliest-boundary bundle)
         (diagnostic-bundle-observations bundle)
         (diagnostic-bundle-disagreements bundle)
         (diagnostic-bundle-error-status bundle))))

(defun verify-diagnostic-bundle (bundle)
  "Recompute every identity and check the outcome invariants of BUNDLE."
  (and
   (diagnostic-bundle-p bundle)
   (let* ((source (diagnostic-bundle-source bundle))
          (program-id (diagnostic-bundle-program-id bundle))
          (expected-program-id (%source-program-id source))
          (observations (diagnostic-bundle-observations bundle))
          (disagreements (diagnostic-bundle-disagreements bundle))
          (outcome (diagnostic-bundle-outcome bundle)))
     (and
      (cid:durable-content-id-p (diagnostic-bundle-id bundle))
      (cid:durable-content-id-p (diagnostic-bundle-campaign-id bundle))
      (or (and (null program-id) (null expected-program-id))
          (and (cid:durable-content-id-p program-id)
               (string= program-id expected-program-id)))
      (%safe-toolchain-id-p (diagnostic-bundle-toolchain-id bundle))
      (string=
       (diagnostic-bundle-campaign-id bundle)
       (cid:content-id-long
        (%campaign-content-form
         source (diagnostic-bundle-toolchain-id bundle))))
      (string=
       (diagnostic-bundle-id bundle)
       (cid:content-id-long
        (%bundle-content-form
         (diagnostic-bundle-campaign-id bundle)
         source program-id
         (diagnostic-bundle-toolchain-id bundle)
         (diagnostic-bundle-verdict bundle)
         outcome
         (diagnostic-bundle-earliest-boundary bundle)
         observations disagreements
         (diagnostic-bundle-error-status bundle))))
      (case (diagnostic-bundle-verdict bundle)
        (:pass
         (and (eq :pass outcome)
              (eq :complete (diagnostic-bundle-earliest-boundary bundle))
              (%valid-observations-p observations)
              (null (%report-disagreements observations))
              (null disagreements)
              (null (diagnostic-bundle-error-status bundle))))
        (:fail
         (case outcome
           (:disagreement
            (and (%valid-observations-p observations)
                 (equal disagreements (%report-disagreements observations))
                 (eq (diagnostic-bundle-earliest-boundary bundle)
                     (%earliest-disagreement disagreements))
                 (null (diagnostic-bundle-error-status bundle))))
           (:refused
            (and (null program-id)
                 (eq :front-door-admission
                     (diagnostic-bundle-earliest-boundary bundle))
                 (null observations)
                 (null disagreements)
                 (diagnostic-bundle-error-status bundle)))
           (:error
            (and (not (eq :complete
                          (diagnostic-bundle-earliest-boundary bundle)))
                 (null observations)
                 (null disagreements)
                 (diagnostic-bundle-error-status bundle)))
           (otherwise nil)))
        (:unavailable
         (and (eq :unavailable outcome)
              (eq :availability
                  (diagnostic-bundle-earliest-boundary bundle))
              (null observations)
              (null disagreements)
              (diagnostic-bundle-error-status bundle)))
        (otherwise nil))))))

(defun replay-diagnostic-bundle
    (bundle &key eshkol-command aot-run-prefix policy)
  "Re-run BUNDLE's campaign and return the fresh bundle plus identity parity."
  (unless (verify-diagnostic-bundle bundle)
    (error "Refusing to replay an invalid diagnostic bundle"))
  (let* ((campaign
           (make-eshkol-campaign
            (diagnostic-bundle-source bundle)
            :toolchain-id (diagnostic-bundle-toolchain-id bundle)))
         (arguments
           (append
            (list campaign
                  :eshkol-command eshkol-command
                  :aot-run-prefix aot-run-prefix)
            (when policy (list :policy policy))))
         (fresh (apply #'run-eshkol-campaign arguments)))
    (values fresh
            (string= (diagnostic-bundle-id bundle)
                     (diagnostic-bundle-id fresh)))))
