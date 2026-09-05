;;;; expert-components.lisp --- expert-components implementation.

(in-package #:expert-components)

(define-condition expert-refusal (error)
  ((code :initarg :code :reader expert-refusal-code)
   (detail :initarg :detail :reader expert-refusal-detail))
  (:report (lambda (condition stream)
             (format stream "Expert inference refused (~A): ~A"
                     (expert-refusal-code condition)
                     (expert-refusal-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'expert-refusal :code code
         :detail (apply #'format nil control arguments)))

(defun %name (value role &key variables)
  (unless (and (stringp value) (plusp (length value))
               (notany (lambda (character)
                         (member character '(#\Space #\Tab #\Newline #\Return)))
                       value)
               (or variables (char/= #\? (char value 0))))
    (%refuse :invalid-name "invalid ~A ~S" role value))
  (copy-seq value))

(defun %variable-p (value)
  (and (stringp value) (> (length value) 1) (char= #\? (char value 0))))

(defstruct (expert-fact
            (:constructor %make-expert-fact (predicate arguments id))
            (:copier nil))
  (predicate "" :type string :read-only t)
  (arguments nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %fact-value (predicate arguments)
  `(("arguments" . ,(mapcar #'copy-seq arguments))
    ("predicate" . ,predicate)))

(defun make-expert-fact (predicate arguments &key pattern)
  (unless (listp arguments)
    (%refuse :invalid-fact "fact arguments must be a list"))
  (let* ((predicate (%name predicate "predicate"))
         (arguments
           (mapcar (lambda (argument)
                     (%name argument "fact argument" :variables pattern))
                   arguments))
         (value (%fact-value predicate arguments)))
    (%make-expert-fact predicate arguments (canonical-id value))))

(defun expert-fact->value (fact)
  (check-type fact expert-fact)
  (%fact-value (expert-fact-predicate fact) (expert-fact-arguments fact)))

(defun %object-ref (object key &optional default)
  (let ((entry (and (listp object) (assoc key object :test #'string=))))
    (if entry (cdr entry) default)))

(defun expert-fact-from-value (value)
  (let ((predicate (%object-ref value "predicate" :missing))
        (arguments (%object-ref value "arguments" :missing)))
    (when (or (eq predicate :missing) (eq arguments :missing))
      (%refuse :invalid-fact "fact value lacks predicate or arguments"))
    (make-expert-fact predicate arguments)))

(defstruct (expert-rule
            (:constructor %make-expert-rule
                (name premises conclusion id))
            (:copier nil))
  (name "" :type string :read-only t)
  (premises nil :type list :read-only t)
  (conclusion nil :type expert-fact :read-only t)
  (id "" :type string :read-only t))

(defun %variables (fact)
  (remove-duplicates
   (remove-if-not #'%variable-p (expert-fact-arguments fact))
   :test #'string=))

(defun %rule-value (name premises conclusion)
  `(("conclusion" . ,(expert-fact->value conclusion))
    ("name" . ,name)
    ("premises" . ,(mapcar #'expert-fact->value premises))))

(defun make-expert-rule (name premises conclusion)
  (unless (and (listp premises) premises)
    (%refuse :invalid-rule "a rule requires at least one premise"))
  (dolist (premise premises) (check-type premise expert-fact))
  (check-type conclusion expert-fact)
  (let* ((name (%name name "rule name"))
         (premise-variables (mapcan #'%variables premises))
         (unbound
           (set-difference (%variables conclusion) premise-variables
                           :test #'string=)))
    (when unbound
      (%refuse :unbound-conclusion
               "rule ~A conclusion has unbound variables ~S" name unbound))
    (let ((value (%rule-value name premises conclusion)))
      (%make-expert-rule name (copy-list premises) conclusion
                         (canonical-id value)))))

(defun expert-rule->value (rule)
  (check-type rule expert-rule)
  (%rule-value (expert-rule-name rule) (expert-rule-premises rule)
               (expert-rule-conclusion rule)))

(defstruct (expert-system
            (:constructor %make-expert-system (name rules id))
            (:copier nil))
  (name "" :type string :read-only t)
  (rules nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %system-value (name rules)
  `(("name" . ,name)
    ("rules" . ,(mapcar #'expert-rule->value rules))
    ("schema" . "urn:rosette-wire:schema:expert-system:1")))

(defun make-expert-system (name rules)
  (unless (and (listp rules) rules)
    (%refuse :empty-system "expert system requires at least one rule"))
  (dolist (rule rules) (check-type rule expert-rule))
  (let* ((name (%name name "expert system name"))
         (rules (sort (copy-list rules) #'string< :key #'expert-rule-name))
         (names (mapcar #'expert-rule-name rules)))
    (unless (= (length names) (length (remove-duplicates names :test #'string=)))
      (%refuse :duplicate-rule "expert system contains duplicate rule names"))
    (let ((value (%system-value name rules)))
      (%make-expert-system name rules (canonical-id value)))))

(defun expert-system->value (system)
  (check-type system expert-system)
  (%system-value (expert-system-name system) (expert-system-rules system)))

(defstruct (expert-derivation
            (:constructor %make-expert-derivation
                (kind fact-id rule-id premise-ids id))
            (:copier nil))
  (kind :input :type keyword :read-only t)
  (fact-id "" :type string :read-only t)
  (rule-id "" :type string :read-only t)
  (premise-ids nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %derivation-value (kind fact-id rule-id premise-ids)
  `(("factId" . ,fact-id)
    ("kind" . ,(string-downcase (symbol-name kind)))
    ("premiseIds" . ,(mapcar #'copy-seq premise-ids))
    ("ruleId" . ,rule-id)))

(defun %make-derivation (kind fact-id rule-id premise-ids)
  (let ((value (%derivation-value kind fact-id rule-id premise-ids)))
    (%make-expert-derivation kind fact-id rule-id (copy-list premise-ids)
                             (canonical-id value))))

(defun expert-derivation->value (derivation)
  (check-type derivation expert-derivation)
  (acons "id" (expert-derivation-id derivation)
         (%derivation-value
          (expert-derivation-kind derivation)
          (expert-derivation-fact-id derivation)
          (expert-derivation-rule-id derivation)
          (expert-derivation-premise-ids derivation))))

(defun %match-argument (pattern value bindings)
  (cond
    ((%variable-p pattern)
     (let ((entry (assoc pattern bindings :test #'string=)))
       (cond (entry (and (string= (cdr entry) value) bindings))
             (t (acons pattern value bindings)))))
    ((string= pattern value) bindings)
    (t nil)))

(defun %match-fact (pattern fact bindings)
  (when (and (string= (expert-fact-predicate pattern)
                      (expert-fact-predicate fact))
             (= (length (expert-fact-arguments pattern))
                (length (expert-fact-arguments fact))))
    (loop with current = bindings
          for pattern-argument in (expert-fact-arguments pattern)
          for value in (expert-fact-arguments fact)
          do (setf current (%match-argument pattern-argument value current))
          unless current do (return nil)
          finally (return current))))

(defun %rule-matches (rule facts)
  (labels ((walk (premises bindings premise-ids)
             (if (null premises)
                 (list (cons bindings (reverse premise-ids)))
                 (loop for fact in facts
                       for next = (%match-fact (first premises) fact bindings)
                       when next append
                         (walk (rest premises) next
                               (cons (expert-fact-id fact) premise-ids))))))
    ;; NIL denotes failure in %MATCH-FACT, so retain an inert binding to make a
    ;; successful constant-only match distinguishable from failure.
    (walk (expert-rule-premises rule) '(("" . "")) nil)))

(defun %instantiate (pattern bindings)
  (make-expert-fact
   (expert-fact-predicate pattern)
   (mapcar (lambda (argument)
             (if (%variable-p argument)
                 (or (cdr (assoc argument bindings :test #'string=))
                     (%refuse :unbound-conclusion
                              "unbound conclusion variable ~A" argument))
                 argument))
           (expert-fact-arguments pattern))))

(defun %canonical-facts (facts)
  (unless (listp facts) (%refuse :invalid-input "facts must be a list"))
  (dolist (fact facts) (check-type fact expert-fact))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (fact facts)
      (setf (gethash (expert-fact-id fact) table) fact))
    (sort (loop for fact being the hash-values of table collect fact)
          #'string< :key #'expert-fact-id)))

(defstruct (expert-run
            (:constructor %make-expert-run)
            (:copier nil))
  (verdict :pass :type keyword :read-only t)
  (system-id "" :type string :read-only t)
  (inputs-id "" :type string :read-only t)
  (facts nil :type list :read-only t)
  (derivations nil :type list :read-only t)
  (rounds 0 :type (integer 0 *) :read-only t)
  (max-rounds 1 :type (integer 1 *) :read-only t)
  (max-facts 1 :type (integer 1 *) :read-only t)
  (id "" :type string :read-only t))

(defun %run-value (run &key include-id)
  (let ((value
          `(("derivations" .
             ,(mapcar #'expert-derivation->value
                      (expert-run-derivations run)))
            ("facts" . ,(mapcar #'expert-fact->value (expert-run-facts run)))
            ("inputsId" . ,(expert-run-inputs-id run))
            ("maxFacts" . ,(expert-run-max-facts run))
            ("maxRounds" . ,(expert-run-max-rounds run))
            ("rounds" . ,(expert-run-rounds run))
            ("schema" . "urn:rosette-wire:schema:expert-run:1")
            ("systemId" . ,(expert-run-system-id run))
            ("verdict" . "pass"))))
    (if include-id (acons "receiptId" (expert-run-id run) value) value)))

(defun expert-run->value (run)
  (check-type run expert-run)
  (%run-value run :include-id t))

(defun %finish-run (system inputs facts derivations rounds max-rounds max-facts)
  (let* ((facts (sort (copy-list facts) #'string< :key #'expert-fact-id))
         (derivations
           (sort (copy-list derivations) #'string< :key #'expert-derivation-id))
         (arguments
           (list :system-id (expert-system-id system)
                 :inputs-id (canonical-id (mapcar #'expert-fact->value inputs))
                 :facts facts :derivations derivations :rounds rounds
                 :max-rounds max-rounds :max-facts max-facts))
         (unsigned (apply #'%make-expert-run arguments))
         (id (canonical-id (%run-value unsigned :include-id nil))))
    (apply #'%make-expert-run (append arguments (list :id id)))))

(defun run-expert-system (system input-facts &key (max-rounds 32) (max-facts 4096))
  "Run bounded deterministic forward chaining and return a proof ledger."
  (check-type system expert-system)
  (unless (and (integerp max-rounds) (plusp max-rounds))
    (%refuse :invalid-limit "max-rounds must be positive"))
  (unless (and (integerp max-facts) (plusp max-facts))
    (%refuse :invalid-limit "max-facts must be positive"))
  (let* ((inputs (%canonical-facts input-facts))
         (facts (copy-list inputs))
         (known (make-hash-table :test #'equal))
         (derivations nil))
    (when (> (length facts) max-facts)
      (%refuse :fact-budget "input facts exceed max-facts"))
    (dolist (fact facts)
      (setf (gethash (expert-fact-id fact) known) fact)
      (push (%make-derivation :input (expert-fact-id fact) "" nil)
            derivations))
    (loop for round from 1 to max-rounds
          do
             (let ((candidates nil))
               (dolist (rule (expert-system-rules system))
                 (dolist (match (%rule-matches rule facts))
                   (let* ((fact (%instantiate (expert-rule-conclusion rule)
                                             (car match)))
                          (derivation
                            (%make-derivation
                             :rule (expert-fact-id fact) (expert-rule-id rule)
                             (cdr match))))
                     (unless (gethash (expert-fact-id fact) known)
                       (push (cons fact derivation) candidates)))))
               (setf candidates
                     (sort candidates
                           (lambda (left right)
                             (or (string< (expert-fact-id (car left))
                                          (expert-fact-id (car right)))
                                 (and (string= (expert-fact-id (car left))
                                               (expert-fact-id (car right)))
                                      (string< (expert-derivation-id (cdr left))
                                               (expert-derivation-id
                                                (cdr right))))))))
               (let ((added 0))
                 (dolist (candidate candidates)
                   (unless (gethash (expert-fact-id (car candidate)) known)
                     (when (>= (length facts) max-facts)
                       (%refuse :fact-budget "inference exceeded max-facts"))
                     (setf (gethash (expert-fact-id (car candidate)) known)
                           (car candidate))
                     (push (car candidate) facts)
                     (push (cdr candidate) derivations)
                     (incf added)))
                 (when (zerop added)
                   (return
                     (%finish-run system inputs facts derivations (1- round)
                                  max-rounds max-facts)))
                 (when (= round max-rounds)
                   (%refuse :round-budget
                            "inference did not reach a fixed point in ~D rounds"
                            max-rounds)))))))

(defun verify-expert-run (system input-facts run)
  (and (expert-run-p run)
       (handler-case
           (let ((fresh
                   (run-expert-system
                    system input-facts
                    :max-rounds (expert-run-max-rounds run)
                    :max-facts (expert-run-max-facts run))))
             (equal (expert-run->value fresh) (expert-run->value run)))
         (error () nil))))

(defun expert-run-certificate (system input-facts run)
  (make-certificate
   :name :expert-run :kind :proof :claim :derivation-replays
   :payload (expert-run->value run)
   :passed (verify-expert-run system input-facts run)
   :witness
   (make-lean-witness
    'expert-derivation-replays
    (lambda () (verify-expert-run system input-facts run)))))

(defun expert-fact-wire-type ()
  (record-type
   (make-component-field "arguments" (list-type (scalar-type :string)))
   (make-component-field "predicate" (scalar-type :string))))

(defun %expert-derivation-wire-type ()
  (record-type
   (make-component-field "factId" (scalar-type :string))
   (make-component-field "id" (scalar-type :string))
   (make-component-field "kind" (scalar-type :string))
   (make-component-field "premiseIds" (list-type (scalar-type :string)))
   (make-component-field "ruleId" (scalar-type :string))))

(defun expert-result-wire-type ()
  (record-type
   (make-component-field "derivations"
                         (list-type (%expert-derivation-wire-type)))
   (make-component-field "facts" (list-type (expert-fact-wire-type)))
   (make-component-field "inputsId" (scalar-type :string))
   (make-component-field "maxFacts" (scalar-type :u64))
   (make-component-field "maxRounds" (scalar-type :u64))
   (make-component-field "receiptId" (scalar-type :string))
   (make-component-field "rounds" (scalar-type :u64))
   (make-component-field "schema" (scalar-type :string))
   (make-component-field "systemId" (scalar-type :string))
   (make-component-field "verdict" (scalar-type :string))))

(defun make-expert-component (system &key (version "1.0.0"))
  (check-type system expert-system)
  (make-component-descriptor
   :name (expert-system-name system) :version version
   :imports nil
   :exports
   (list
    (make-port
     "expert"
     (list
      (make-component-operation
       "infer"
       (list (make-component-field
              "facts" (list-type (expert-fact-wire-type))))
       (expert-result-wire-type)))))
   :effects '(:pure) :capabilities nil
   :adapter
   `(("kind" . "expert-rules-v1")
     ("rules" . ,(mapcar #'expert-rule->value (expert-system-rules system)))
     ("systemId" . ,(expert-system-id system)))
   :verifiers '("expert-derivation-replay/v1")))

(defun make-expert-handler (system &key (max-rounds 32) (max-facts 4096))
  (check-type system expert-system)
  (lambda (arguments services context)
    (declare (ignore services context))
    (let ((entry (assoc "facts" arguments :test #'string=)))
      (unless entry (%refuse :missing-input "expert handler requires facts"))
      (expert-run->value
       (run-expert-system
        system (mapcar #'expert-fact-from-value (cdr entry))
        :max-rounds max-rounds :max-facts max-facts)))))
