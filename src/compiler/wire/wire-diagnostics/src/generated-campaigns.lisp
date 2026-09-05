;;;; generated-campaigns.lisp --- seeded generation, shrinking, and evidence.

(in-package #:rosette-wire-diagnostics)

(defconstant +generated-report-schema+
  :rosette-wire-generated-diagnostic-report/v2)

(defstruct (generated-diagnostic-report
            (:constructor %make-generated-diagnostic-report
                (&key id kind parameters seed requested-trials trials verdict
                      counterexample minimized shrink-steps evidence)))
  "A content-bound property campaign whose retained bundles re-verify alone."
  id kind parameters seed requested-trials trials verdict counterexample
  minimized shrink-steps evidence)

(defconstant +campaign-rng-modulus+ #.(ash 1 64))
(defconstant +campaign-rng-multiplier+ 6364136223846793005)
(defconstant +campaign-rng-increment+ 1442695040888963407)

(defstruct (%campaign-rng
            (:constructor %make-campaign-rng (state)))
  (state 0 :type (unsigned-byte 64)))

(defun %seeded-campaign-rng (seed)
  (%make-campaign-rng
   (mod (logxor seed #x9e3779b97f4a7c15) +campaign-rng-modulus+)))

(defun %campaign-rng-next (rng)
  (setf (%campaign-rng-state rng)
        (mod (+ (* (%campaign-rng-state rng) +campaign-rng-multiplier+)
                +campaign-rng-increment+)
             +campaign-rng-modulus+)))

(defun %draw (generator rng)
  (funcall generator rng))

(defun %gen-bounded (minimum maximum)
  (let ((width (1+ (- maximum minimum))))
    (lambda (rng)
      (+ minimum (mod (%campaign-rng-next rng) width)))))

(defun %gen-element (elements)
  (let* ((items (coerce elements 'vector))
         (count (length items)))
    (when (zerop count) (error "Cannot select from an empty generator"))
    (lambda (rng) (aref items (mod (%campaign-rng-next rng) count)))))

(defun %gen-one-of (&rest generators)
  (let ((chooser (%gen-element generators)))
    (lambda (rng) (%draw (%draw chooser rng) rng))))

(defun %shrink-integer (integer)
  (unless (zerop integer)
    (remove-duplicates
     (remove integer (list 0 (truncate integer 2)) :test #'=)
     :test #'=)))

(defun %gen-sample (generator count seed)
  (let ((rng (%seeded-campaign-rng seed)))
    (loop repeat count collect (%draw generator rng))))

(defun %eshkol-expression-generator (depth variables)
  (let ((literal (%gen-bounded -8 8)))
    (if (zerop depth)
        (if variables
            (%gen-one-of literal (%gen-element variables))
            literal)
        (let ((choice (%gen-bounded 0 (if variables 6 5)))
              (left (%eshkol-expression-generator (1- depth) variables))
              (right (%eshkol-expression-generator (1- depth) variables))
              (condition-left
                (%eshkol-expression-generator (1- depth) variables))
              (condition-right
                (%eshkol-expression-generator (1- depth) variables))
              (operator (%gen-element '(:+ :- :*)))
              (comparator (%gen-element '(:< :> :=)))
              (variable (intern (format nil "X~D" depth) :keyword)))
          (lambda (rng)
            (case (%draw choice rng)
              (0 (%draw literal rng))
              (1 (if variables
                     (%draw (%gen-element variables) rng)
                     (%draw literal rng)))
              ((2 3)
               (list (%draw operator rng) (%draw left rng) (%draw right rng)))
              (4
               (list :if
                     (list (%draw comparator rng)
                           (%draw condition-left rng)
                           (%draw condition-right rng))
                     (%draw left rng) (%draw right rng)))
              (otherwise
               (let ((initializer (%draw left rng))
                     (body-generator
                       (%eshkol-expression-generator
                        (1- depth) (cons variable variables))))
                 (list :let variable initializer (%draw body-generator rng))))))))))

(defun make-eshkol-source-generator (&key (max-depth 3))
  "Return a seeded generator for the exact admitted Eshkol/front-door subset."
  (unless (typep max-depth '(integer 0 5))
    (error "MAX-DEPTH must be in [0,5]"))
  (let ((expression (%eshkol-expression-generator max-depth nil)))
    (lambda (rng) (list (list :main (%draw expression rng))))))

(defun %expression-weight (expression)
  (cond ((integerp expression) (list 1 (abs expression)))
        ((symbolp expression) (list 1 0))
        ((consp expression)
         (let ((children (mapcar #'%expression-weight (rest expression))))
           (list (1+ (reduce #'+ children :key #'first :initial-value 0))
                 (reduce #'+ children :key #'second :initial-value 0))))
        (t (list most-positive-fixnum most-positive-fixnum))))

(defun %weight< (left right)
  (or (< (first left) (first right))
      (and (= (first left) (first right))
           (< (second left) (second right)))))

(defun %rebuild-candidates (head arguments)
  (loop for index below (length arguments)
        append
        (loop for smaller in (%shrink-eshkol-expression (nth index arguments))
              collect (cons head
                            (loop for argument in arguments for i from 0
                                  collect (if (= i index) smaller argument))))))

(defun %shrink-eshkol-expression (expression)
  (let ((raw
          (cond
            ((integerp expression) (%shrink-integer expression))
            ((symbolp expression) nil)
            ((and (consp expression)
                  (member (symbol-name (first expression))
                          '("+" "-" "*" "<" ">" "=") :test #'string=)
                  (= (length expression) 3))
             (append (list 0 (second expression) (third expression))
                     (%rebuild-candidates (first expression)
                                          (rest expression))))
            ((and (consp expression)
                  (string= (symbol-name (first expression)) "IF")
                  (= (length expression) 4))
             (append (list 0 (third expression) (fourth expression))
                     (%rebuild-candidates (first expression)
                                          (rest expression))))
            ((and (consp expression)
                  (string= (symbol-name (first expression)) "LET")
                  (= (length expression) 4))
             ;; BODY may reference the bound variable, so it is never lifted
             ;; outside the LET. INITIALIZER is always in the outer scope.
             (append
              (list 0 (third expression))
              (loop for smaller in (%shrink-eshkol-expression
                                    (third expression))
                    collect (list (first expression) (second expression)
                                  smaller (fourth expression)))
              (loop for smaller in (%shrink-eshkol-expression
                                    (fourth expression))
                    collect (list (first expression) (second expression)
                                  (third expression) smaller))))
            (t nil)))
        (weight (%expression-weight expression)))
    (remove-duplicates
     (remove-if-not (lambda (candidate)
                      (%weight< (%expression-weight candidate) weight))
                    raw)
     :test #'equal)))

(defun shrink-eshkol-source (source)
  "Offer only strictly simpler, still-well-scoped front-door source forms."
  (if (and (%proper-list-p source) (= (length source) 1)
           (%proper-list-p (first source)) (= (length (first source)) 2)
           (symbolp (first (first source)))
           (string= (symbol-name (first (first source))) "MAIN"))
      (mapcar (lambda (expression) (list (list :main expression)))
              (%shrink-eshkol-expression (second (first source))))
      nil))

(defun make-moonlab-surface-spec-generator ()
  "Return the fixed public parameter lattice used by Moonlab campaigns."
  (let ((measurement (%gen-bounded 0 15))
        (channel (%gen-bounded 0 16))
        (phase (%gen-bounded 0 3))
        (gradient (%gen-bounded -16 16)))
    (lambda (rng)
      (list :measurement-step (%draw measurement rng)
            :channel-step (%draw channel rng)
            :qgt-phase (%draw phase rng)
            :gradient-a (%draw gradient rng)
            :gradient-b (%draw gradient rng)))))

(defun shrink-moonlab-surface-spec (spec)
  "Shrink one integer-lattice Moonlab surface request toward its origin."
  (let ((owned (%normalize-moonlab-surface-spec spec))
        (candidates nil))
    (dolist (key +moonlab-surface-spec-keys+)
      (dolist (smaller (%shrink-integer (getf owned key)))
        (let ((candidate (copy-list owned)))
          (setf (getf candidate key) smaller)
          (handler-case
              (push (%normalize-moonlab-surface-spec candidate) candidates)
            (error () nil)))))
    (remove-duplicates (nreverse candidates) :test #'equal)))

(defun %evidence-form (evidence)
  (typecase evidence
    (diagnostic-bundle (diagnostic-bundle->form evidence))
    (moonlab-surface-diagnostic-bundle
     (moonlab-surface-diagnostic-bundle->form evidence))
    (t (error "Unsupported generated campaign evidence ~S" evidence))))

(defun %generated-report-content-form
    (kind parameters seed requested-trials trials verdict counterexample
     minimized shrink-steps evidence)
  (list :schema +generated-report-schema+ :kind kind :parameters parameters
        :seed seed :requested-trials requested-trials :trials trials
        :verdict verdict :counterexample counterexample :minimized minimized
        :shrink-steps shrink-steps :evidence (mapcar #'%evidence-form evidence)))

(defun %finish-generated-report
    (&key kind parameters seed requested-trials trials verdict counterexample
          minimized shrink-steps evidence)
  (let* ((owned-evidence (copy-list evidence))
         (content (%generated-report-content-form
                   kind parameters seed requested-trials trials verdict
                   counterexample minimized shrink-steps owned-evidence)))
    (%make-generated-diagnostic-report
     :id (cid:content-id-long content) :kind kind
     :parameters (copy-list parameters) :seed seed
     :requested-trials requested-trials :trials trials :verdict verdict
     :counterexample (copy-tree counterexample)
     :minimized (copy-tree minimized) :shrink-steps shrink-steps
     :evidence owned-evidence)))

(defun %cached-runner (runner)
  (let ((cache nil))
    (values
     (lambda (input)
       (let ((found (assoc input cache :test #'equal)))
         (if found
             (cdr found)
             (let ((bundle (funcall runner input)))
               (push (cons (copy-tree input) bundle) cache)
               bundle))))
     (lambda () cache))))

(defun %run-generated-campaign
    (kind generator shrinker runner bundle-verdict
     &key parameters seed trials preflight-input)
  (unless (and (typep seed '(integer 0 *))
               (typep trials '(integer 1 10000)))
    (error "SEED must be nonnegative and TRIALS must be in [1,10000]"))
  (multiple-value-bind (run-cached cache-reader) (%cached-runner runner)
    (declare (ignore cache-reader))
    (let ((preflight (funcall run-cached preflight-input)))
      (when (eq (funcall bundle-verdict preflight) :unavailable)
        (return-from %run-generated-campaign
          (%finish-generated-report
           :kind kind :parameters parameters :seed seed
           :requested-trials trials :trials 0 :verdict :unavailable
           :counterexample nil :minimized nil :shrink-steps 0
           :evidence (list preflight))))
      (let ((rng (%seeded-campaign-rng seed))
            (passing-evidence nil)
            (counterexample nil)
            (failure-trial 0))
        (loop for trial from 1 to trials
              for input = (%draw generator rng)
              for bundle = (funcall run-cached input)
              if (eq :pass (funcall bundle-verdict bundle))
                do (push bundle passing-evidence)
              else
                do (setf counterexample input failure-trial trial)
                   (loop-finish))
        (if (null counterexample)
            (%finish-generated-report
             :kind kind :parameters parameters :seed seed
             :requested-trials trials :trials trials :verdict :pass
             :counterexample nil :minimized nil :shrink-steps 0
             :evidence (nreverse passing-evidence))
            (let ((minimized counterexample)
                  (shrink-steps 0))
              (loop
                (let ((next
                        (find-if
                         (lambda (candidate)
                           (not (eq :pass
                                    (funcall bundle-verdict
                                             (funcall run-cached candidate)))))
                         (funcall shrinker minimized))))
                  (unless next (return))
                  (setf minimized next)
                  (incf shrink-steps)))
              (%finish-generated-report
               :kind kind :parameters parameters :seed seed
               :requested-trials trials :trials failure-trial
               :verdict :counterexample :counterexample counterexample
               :minimized minimized :shrink-steps shrink-steps
               :evidence (list (funcall run-cached minimized)))))))))

(defun run-generated-eshkol-campaign
    (&key (seed 0) (trials 32) (max-depth 3) toolchain-id eshkol-command
          aot-run-prefix policy runner)
  "Differentially check generated exact programs across all Eshkol floors."
  (let ((generator (make-eshkol-source-generator :max-depth max-depth)))
    (%run-generated-campaign
     :eshkol generator #'shrink-eshkol-source
     (or runner
         (lambda (source)
           (run-eshkol-campaign
            (make-eshkol-campaign source :toolchain-id toolchain-id)
            :eshkol-command eshkol-command :aot-run-prefix aot-run-prefix
            :policy policy)))
     #'diagnostic-bundle-verdict
     :parameters (list :max-depth max-depth) :seed seed :trials trials
     :preflight-input '((:main 0)))))

(defun run-generated-moonlab-campaign
    (&key (seed 0) (trials 24) implementation-id
          (required-abi '(0 6 0)) (tolerance 1d-8)
          moonlab-command moonlab-library runner)
  "Generate and shrink public-consumer Moonlab surface requests."
  (let ((generator (make-moonlab-surface-spec-generator)))
    (%run-generated-campaign
     :moonlab generator #'shrink-moonlab-surface-spec
     (or runner
         (lambda (spec)
           (run-moonlab-surface-campaign
            (make-moonlab-surface-campaign
             spec :implementation-id implementation-id
             :required-abi required-abi :tolerance tolerance)
            :moonlab-command moonlab-command :moonlab-library moonlab-library)))
     #'moonlab-surface-diagnostic-bundle-verdict
     :parameters (list :required-abi (copy-list required-abi)
                       :tolerance (coerce tolerance 'double-float))
     :seed seed :trials trials
     :preflight-input '(:measurement-step 0 :channel-step 0 :qgt-phase 0
                        :gradient-a 0 :gradient-b 0))))

(defun generated-diagnostic-report->form (report)
  (unless (generated-diagnostic-report-p report)
    (error "Expected a GENERATED-DIAGNOSTIC-REPORT"))
  (list :id (generated-diagnostic-report-id report) :content
        (%generated-report-content-form
         (generated-diagnostic-report-kind report)
         (generated-diagnostic-report-parameters report)
         (generated-diagnostic-report-seed report)
         (generated-diagnostic-report-requested-trials report)
         (generated-diagnostic-report-trials report)
         (generated-diagnostic-report-verdict report)
         (generated-diagnostic-report-counterexample report)
         (generated-diagnostic-report-minimized report)
         (generated-diagnostic-report-shrink-steps report)
         (generated-diagnostic-report-evidence report))))

(defun %generated-evidence-valid-p (kind evidence)
  (case kind
    (:eshkol (and (diagnostic-bundle-p evidence)
                  (verify-diagnostic-bundle evidence)))
    (:moonlab (and (moonlab-surface-diagnostic-bundle-p evidence)
                   (verify-moonlab-surface-diagnostic-bundle evidence)))
    (otherwise nil)))

(defun %generated-evidence-input (kind evidence)
  (case kind
    (:eshkol (diagnostic-bundle-source evidence))
    (:moonlab (moonlab-surface-diagnostic-bundle-spec evidence))))

(defun %generated-evidence-verdict (kind evidence)
  (case kind
    (:eshkol (diagnostic-bundle-verdict evidence))
    (:moonlab (moonlab-surface-diagnostic-bundle-verdict evidence))))

(defun %report-generator (kind parameters)
  (case kind
    (:eshkol
     (make-eshkol-source-generator :max-depth (getf parameters :max-depth)))
    (:moonlab (make-moonlab-surface-spec-generator))
    (otherwise nil)))

(defun verify-generated-diagnostic-report (report)
  "Verify identities, regenerate trial inputs, and re-run every bundle law."
  (and
   (generated-diagnostic-report-p report)
   (handler-case
       (let* ((kind (generated-diagnostic-report-kind report))
              (parameters (generated-diagnostic-report-parameters report))
              (seed (generated-diagnostic-report-seed report))
              (requested (generated-diagnostic-report-requested-trials report))
              (trials (generated-diagnostic-report-trials report))
              (verdict (generated-diagnostic-report-verdict report))
              (counterexample
                (generated-diagnostic-report-counterexample report))
              (minimized (generated-diagnostic-report-minimized report))
              (shrink-steps
                (generated-diagnostic-report-shrink-steps report))
              (evidence (generated-diagnostic-report-evidence report))
              (content (%generated-report-content-form
                        kind parameters seed requested trials verdict
                        counterexample minimized shrink-steps evidence))
              (generator (%report-generator kind parameters)))
         (and generator
              (typep seed '(integer 0 *))
              (typep requested '(integer 1 10000))
              (typep trials '(integer 0 10000))
              (typep shrink-steps '(integer 0 *))
              (string= (generated-diagnostic-report-id report)
                       (cid:content-id-long content))
              (every (lambda (bundle)
                       (%generated-evidence-valid-p kind bundle))
                     evidence)
              (case verdict
                (:pass
                 (and (= trials requested) (= (length evidence) trials)
                      (null counterexample) (null minimized)
                      (zerop shrink-steps)
                      (every (lambda (bundle)
                               (eq :pass
                                   (%generated-evidence-verdict kind bundle)))
                             evidence)
                      (equal
                       (mapcar (lambda (bundle)
                                 (%generated-evidence-input kind bundle))
                               evidence)
                       (%gen-sample generator trials seed))))
                (:counterexample
                 (and (plusp trials) (= (length evidence) 1)
                      counterexample minimized
                      (not (eq :pass
                               (%generated-evidence-verdict kind
                                                            (first evidence))))
                      (equal minimized
                             (%generated-evidence-input kind (first evidence)))
                      (equal counterexample
                             (nth (1- trials)
                                  (%gen-sample generator trials seed)))))
                (:unavailable
                 (and (zerop trials) (= (length evidence) 1)
                      (null counterexample) (null minimized)
                      (zerop shrink-steps)
                      (eq :unavailable
                          (%generated-evidence-verdict kind (first evidence)))))
                (otherwise nil))))
     (error () nil))))
