;;;; composition-interaction.lisp --- composition-interaction implementation.

(in-package #:composition-interaction)

(define-condition interaction-refusal (error)
  ((code :initarg :code :reader interaction-refusal-code)
   (detail :initarg :detail :reader interaction-refusal-detail))
  (:report (lambda (condition stream)
             (format stream "Interaction Composition refused (~A): ~A"
                     (interaction-refusal-code condition)
                     (interaction-refusal-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'interaction-refusal :code code
         :detail (apply #'format nil control arguments)))

(defun %object-ref (object key &optional default)
  (let ((entry (and (listp object) (assoc key object :test #'string=))))
    (if entry (cdr entry) default)))

(defun %name-p (value)
  (and (stringp value) (plusp (length value))
       (notany (lambda (character)
                 (member character '(#\Space #\Tab #\Newline #\Return)))
               value)))

(defun %require-name (value role)
  (unless (%name-p value)
    (%refuse :invalid-term "~A must be a non-empty whitespace-free string" role))
  (copy-seq value))

;;; Portable lambda terms --------------------------------------------------

(defun interaction-var (name)
  (list "var" (%require-name name "variable name")))

(defun interaction-lam (name body)
  (list "lam" (%require-name name "binder name") (copy-tree body)))

(defun interaction-app (function argument)
  (list "app" (copy-tree function) (copy-tree argument)))

(defun interaction-term->value (term)
  "Project a TLC term into the portable JSON-array interaction term grammar."
  (check-type term tlc-term)
  (ecase (tlc-term-kind term)
    (:var (interaction-var (symbol-name (tlc-var-name term))))
    (:lam (interaction-lam
           (symbol-name (tlc-lam-param term))
           (interaction-term->value (tlc-lam-body term))))
    (:app (interaction-app
           (interaction-term->value (tlc-app-fn term))
           (interaction-term->value (tlc-app-arg term))))))

(defun interaction-term-from-value (value &key (max-nodes 10000))
  "Decode a closed portable interaction term, rejecting free names and size abuse."
  (unless (and (integerp max-nodes) (plusp max-nodes))
    (%refuse :invalid-limit "max-nodes must be a positive integer"))
  (let ((nodes 0))
    (labels ((decode (form environment)
               (when (> (incf nodes) max-nodes)
                 (%refuse :term-size "portable term exceeds ~D nodes" max-nodes))
               (unless (and (listp form) (stringp (first form)))
                 (%refuse :invalid-term "expected a portable term array, got ~S" form))
               (cond
                 ((and (string= (first form) "var") (= (length form) 2))
                  (let ((binding (assoc (%require-name (second form) "variable name")
                                        environment :test #'string=)))
                    (unless binding
                      (%refuse :free-variable "term contains free variable ~S"
                               (second form)))
                    (tlc-var (cdr binding))))
                 ((and (string= (first form) "lam") (= (length form) 3))
                  (let* ((name (%require-name (second form) "binder name"))
                         (symbol (make-symbol name)))
                    (tlc-lam symbol
                             (decode (third form)
                                     (acons name symbol environment)))))
                 ((and (string= (first form) "app") (= (length form) 3))
                  (tlc-app (decode (second form) environment)
                           (decode (third form) environment)))
                 (t (%refuse :invalid-term "unknown or malformed term form ~S" form)))))
      (decode value nil))))

(defun natural-add-term () (interaction-term->value (church-add)))
(defun natural-mul-term () (interaction-term->value (church-mul)))
(defun natural-exp-term () (interaction-term->value (church-exp)))

;;; Component adapter ------------------------------------------------------

(defun make-interaction-operation-adapter (port operation term)
  "Describe one curried, closed natural-number operation implementation."
  (let ((term-value (etypecase term
                      (tlc-term (interaction-term->value term))
                      (list (copy-tree term)))))
    ;; Decode now so malformed or open terms never enter a descriptor.
    (interaction-term-from-value term-value)
    `(("operation" . ,(%require-name operation "operation"))
      ("port" . ,(%require-name port "port"))
      ("term" . ,term-value))))

(defun make-interaction-adapter (operations)
  "Return the canonical adapter value embedded in a Component descriptor."
  (unless (and (listp operations) operations)
    (%refuse :invalid-adapter "at least one operation adapter is required"))
  (let ((seen (make-hash-table :test #'equal))
        (rows nil))
    (dolist (operation operations)
      (let ((port (%object-ref operation "port"))
            (name (%object-ref operation "operation"))
            (term (%object-ref operation "term" :missing)))
        (unless (and (%name-p port) (%name-p name) (not (eq term :missing)))
          (%refuse :invalid-adapter "malformed operation adapter ~S" operation))
        (interaction-term-from-value term)
        (let ((key (list port name)))
          (when (gethash key seen)
            (%refuse :duplicate-operation "duplicate interaction operation ~S" key))
          (setf (gethash key seen) t))
        (push (copy-tree operation) rows)))
    `(("kind" . "interaction-net-v1")
      ("operations" .
       ,(sort rows
              (lambda (left right)
                (let ((lp (%object-ref left "port"))
                      (rp (%object-ref right "port")))
                  (or (string< lp rp)
                      (and (string= lp rp)
                           (string< (%object-ref left "operation")
                                    (%object-ref right "operation")))))))))))

(defun %u64-type-p (type)
  (let ((value (wire-type->value type)))
    (and (string= "scalar" (%object-ref value "kind" ""))
         (string= "u64" (%object-ref value "name" "")))))

(defun %find-port (descriptor name)
  (find name (component-descriptor-exports descriptor)
        :test #'string= :key #'port-name))

(defun %find-operation (port name)
  (and port (find name (port-operations port)
                  :test #'string= :key #'component-operation-name)))

(defun %adapter-operation (descriptor port-name operation-name)
  (let ((adapter (component-descriptor-adapter descriptor)))
    (unless (and (listp adapter)
                 (string= "interaction-net-v1"
                          (%object-ref adapter "kind" "")))
      (%refuse :adapter-unavailable
               "Component ~A has no interaction-net-v1 adapter"
               (rosette-wire:component-descriptor-name descriptor)))
    (or (find-if (lambda (entry)
                   (and (string= port-name (%object-ref entry "port" ""))
                        (string= operation-name
                                 (%object-ref entry "operation" ""))))
                 (%object-ref adapter "operations" nil))
        (%refuse :operation-unavailable
                 "adapter has no implementation for ~A/~A"
                 port-name operation-name))))

(defun %leading-lambdas (term)
  (loop for cursor = term then (tlc-lam-body cursor)
        while (eq :lam (tlc-term-kind cursor))
        count 1))

(defun %apply-terms (function arguments)
  (reduce #'tlc-app arguments :initial-value function))

(defun %canonical-inputs (inputs)
  (unless (listp inputs) (%refuse :invalid-inputs "inputs must be an alist"))
  (let ((copy (copy-tree inputs)) (seen (make-hash-table :test #'equal)))
    (dolist (entry copy)
      (unless (and (consp entry) (%name-p (car entry)))
        (%refuse :invalid-inputs "malformed input entry ~S" entry))
      (when (gethash (car entry) seen)
        (%refuse :invalid-inputs "duplicate input ~A" (car entry)))
      (setf (gethash (car entry) seen) t))
    (sort copy #'string< :key #'car)))

(defun %natural (value role max-natural)
  (unless (and (integerp value) (<= 0 value max-natural))
    (%refuse :natural-bound "~A must be in 0..~D, got ~S"
             role max-natural value))
  value)

(defun %node-table (composition)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (composition-nodes composition) table)
      (setf (gethash (component-node-id node) table) node))))

(defun %symbol-table (names)
  (mapcar (lambda (name) (cons name (make-symbol name))) names))

(defun %lower-source (source input-symbols step-symbols max-natural)
  (let ((kind (%object-ref source "kind" "")))
    (cond
      ((string= kind "input")
       (let ((entry (assoc (%object-ref source "input") input-symbols
                           :test #'string=)))
         (unless entry (%refuse :unknown-input "unknown graph input in binding"))
         (tlc-var (cdr entry))))
      ((string= kind "step")
       (let ((entry (assoc (%object-ref source "step") step-symbols
                           :test #'string=)))
         (unless entry (%refuse :unknown-step "unknown step in binding"))
         (tlc-var (cdr entry))))
      ((string= kind "literal")
       (church-numeral
        (%natural (%object-ref source "value" :missing)
                  "literal" max-natural)))
      (t (%refuse :unsupported-source "unsupported data source kind ~S" kind)))))

(defun %ensure-pure-node (node)
  (let ((descriptor (component-node-descriptor node)))
    (unless (equal '(:pure) (component-descriptor-effects descriptor))
      (%refuse :effectful-component "Component ~A is not explicitly :pure"
               (rosette-wire:component-descriptor-name descriptor)))
    (when (component-descriptor-capabilities descriptor)
      (%refuse :capability-component "Component ~A declares capabilities"
               (rosette-wire:component-descriptor-name descriptor)))
    (when (component-descriptor-imports descriptor)
      (%refuse :service-component "Component ~A imports services"
               (rosette-wire:component-descriptor-name descriptor)))))

(defun lower-composition-to-interaction-term
    (composition inputs &key (max-natural 4096))
  "Lower the closed pure/u64/single-output Composition subset to one TLC term."
  (check-type composition composition)
  (unless (and (integerp max-natural) (not (minusp max-natural)))
    (%refuse :invalid-limit "max-natural must be non-negative"))
  (let ((validation (validate-composition composition)))
    (unless (eq :pass (wire-receipt-verdict validation))
      (%refuse :invalid-composition "Rosette validation refused the Composition"))
    (let* ((value (composition->value composition))
           (services (%object-ref value "serviceBindings" nil))
           (outputs (%object-ref value "outputs" nil))
           (input-fields (%object-ref value "inputs" nil))
           (canonical-inputs (%canonical-inputs inputs))
           (input-names (mapcar (lambda (field) (%object-ref field "name"))
                                input-fields))
           (input-symbols (%symbol-table input-names))
           (order (wire-receipt-step-order validation))
           (step-symbols (%symbol-table order))
           (step-values (%object-ref value "steps" nil))
           (nodes (%node-table composition))
           (expressions (make-hash-table :test #'equal)))
      (when services
        (%refuse :service-binding "interaction floor does not execute services"))
      (unless (= 1 (length outputs))
        (%refuse :output-arity "interaction floor requires exactly one output"))
      (unless (= (length canonical-inputs) (length input-fields))
        (%refuse :input-set "input set does not match the Composition"))
      (dolist (field input-fields)
        (let ((type (%object-ref field "type" nil)))
          (unless (and (string= "scalar" (%object-ref type "kind" ""))
                       (string= "u64" (%object-ref type "name" "")))
            (%refuse :unsupported-type "graph input ~A is not u64"
                     (%object-ref field "name"))))
        (unless (assoc (%object-ref field "name") canonical-inputs
                       :test #'string=)
          (%refuse :input-set "missing graph input ~A"
                   (%object-ref field "name")))
        (%natural (cdr (assoc (%object-ref field "name") canonical-inputs
                              :test #'string=))
                  (format nil "input ~A" (%object-ref field "name"))
                  max-natural))
      (dolist (step-id order)
        (let* ((step (find step-id step-values :test #'string=
                           :key (lambda (item) (%object-ref item "id"))))
               (node-id (%object-ref step "node"))
               (node (gethash node-id nodes))
               (port-name (%object-ref step "port"))
               (operation-name (%object-ref step "operation")))
          (unless node (%refuse :unknown-node "unknown step node ~A" node-id))
          (%ensure-pure-node node)
          (let* ((descriptor (component-node-descriptor node))
                 (port (%find-port descriptor port-name))
                 (operation (%find-operation port operation-name)))
            (unless operation
              (%refuse :unknown-operation "unknown operation ~A/~A"
                       port-name operation-name))
            (unless (and (%u64-type-p (component-operation-output operation))
                         (every (lambda (field)
                                  (%u64-type-p (component-field-type field)))
                                (component-operation-inputs operation)))
              (%refuse :unsupported-type "operation ~A/~A is not u64 -> u64"
                       port-name operation-name))
            (let* ((adapter (%adapter-operation descriptor port-name operation-name))
                   (function (interaction-term-from-value
                              (%object-ref adapter "term")))
                   (fields (component-operation-inputs operation))
                   (bindings (%object-ref step "bindings" nil)))
              (unless (= (length fields) (length bindings))
                (%refuse :binding-arity "step ~A binding arity drifted" step-id))
              (when (< (%leading-lambdas function) (length fields))
                (%refuse :adapter-arity "adapter ~A/~A has too few curried arguments"
                         port-name operation-name))
              (let ((arguments
                      (mapcar
                       (lambda (field)
                         (let ((binding
                                 (find (component-field-name field) bindings
                                       :test #'string=
                                       :key (lambda (item)
                                              (%object-ref item "argument")))))
                           (unless binding
                             (%refuse :missing-binding "step ~A misses argument ~A"
                                      step-id (component-field-name field)))
                           (%lower-source (%object-ref binding "source")
                                          input-symbols step-symbols max-natural)))
                       fields)))
                (setf (gethash step-id expressions)
                      (%apply-terms function arguments)))))))
      (let* ((output-step (%object-ref (first outputs) "step"))
             (output-symbol (assoc output-step step-symbols :test #'string=))
             (body (and output-symbol (tlc-var (cdr output-symbol)))))
        (unless body (%refuse :unknown-output "output references unknown step"))
        ;; Reverse wrapping produces let s1=e1 in let s2=e2 in output, so
        ;; repeated uses become DUP nodes rather than copied source trees.
        (dolist (step-id (reverse order))
          (let ((symbol (cdr (assoc step-id step-symbols :test #'string=))))
            (setf body (tlc-app (tlc-lam symbol body)
                                (gethash step-id expressions)))))
        (dolist (name (reverse input-names))
          (let ((symbol (cdr (assoc name input-symbols :test #'string=)))
                (natural (%natural (cdr (assoc name canonical-inputs
                                                :test #'string=))
                                   (format nil "input ~A" name) max-natural)))
            (setf body (tlc-app (tlc-lam symbol body)
                                (church-numeral natural)))))
        body))))

;;; Execution certificate -------------------------------------------------

(defstruct (interaction-receipt
            (:constructor %make-interaction-receipt)
            (:copier nil))
  (verdict :pass :type keyword :read-only t)
  (composition-id "" :type string :read-only t)
  (inputs-id "" :type string :read-only t)
  (source-term nil :type list :read-only t)
  (output 0 :type (integer 0 *) :read-only t)
  (sequential-normal-form nil :type list :read-only t)
  (parallel-normal-form nil :type list :read-only t)
  (sequential-rounds 0 :type (integer 0 *) :read-only t)
  (parallel-rounds 0 :type (integer 0 *) :read-only t)
  (sequential-interactions 0 :type (integer 0 *) :read-only t)
  (parallel-interactions 0 :type (integer 0 *) :read-only t)
  (max-rounds 0 :type (integer 1 *) :read-only t)
  (max-natural 0 :type (integer 0 *) :read-only t)
  (id "" :type string :read-only t))

(defun %receipt-value (receipt &key include-id)
  (let ((value
          `(("compositionId" . ,(interaction-receipt-composition-id receipt))
            ("inputsId" . ,(interaction-receipt-inputs-id receipt))
            ("maxNatural" . ,(interaction-receipt-max-natural receipt))
            ("maxRounds" . ,(interaction-receipt-max-rounds receipt))
            ("output" . ,(interaction-receipt-output receipt))
            ("parallelInteractions" .
             ,(interaction-receipt-parallel-interactions receipt))
            ("parallelNormalForm" .
             ,(copy-tree (interaction-receipt-parallel-normal-form receipt)))
            ("parallelRounds" . ,(interaction-receipt-parallel-rounds receipt))
            ("schema" . "urn:rosette-wire:schema:interaction-receipt:1")
            ("sequentialInteractions" .
             ,(interaction-receipt-sequential-interactions receipt))
            ("sequentialNormalForm" .
             ,(copy-tree (interaction-receipt-sequential-normal-form receipt)))
            ("sequentialRounds" .
             ,(interaction-receipt-sequential-rounds receipt))
            ("sourceTerm" . ,(copy-tree (interaction-receipt-source-term receipt)))
            ("verdict" . "pass"))))
    (if include-id
        (acons "id" (interaction-receipt-id receipt) value)
        value)))

(defun interaction-receipt->value (receipt)
  (check-type receipt interaction-receipt)
  (%receipt-value receipt :include-id t))

(defun %execute (composition inputs max-rounds max-natural)
  (let* ((canonical-inputs (%canonical-inputs inputs))
         (term (lower-composition-to-interaction-term
                composition canonical-inputs :max-natural max-natural))
         (sequential (lam->net term))
         (parallel (lam->net term)))
    (handler-case
        (let ((sequential-rounds
                (reduce-all sequential :parallel nil :max-rounds max-rounds))
              (parallel-rounds
                (reduce-all parallel :parallel t :max-rounds max-rounds)))
          (let* ((sequential-term (net->term sequential))
                 (parallel-term (net->term parallel))
                 (sequential-value (interaction-term->value sequential-term))
                 (parallel-value (interaction-term->value parallel-term))
                 (output (church-count sequential-term)))
            (unless (equal sequential-value parallel-value)
              (%refuse :non-confluent "sequential and parallel normal forms differ"))
            (unless output
              (%refuse :non-natural-output "normal form is not a Church natural"))
            (%natural output "output" max-natural)
            (let* ((arguments
                     (list :composition-id (composition-id composition)
                           :inputs-id (canonical-id canonical-inputs)
                           :source-term (interaction-term->value term)
                           :output output
                           :sequential-normal-form sequential-value
                           :parallel-normal-form parallel-value
                           :sequential-rounds sequential-rounds
                           :parallel-rounds parallel-rounds
                           :sequential-interactions (net-interactions sequential)
                           :parallel-interactions (net-interactions parallel)
                           :max-rounds max-rounds :max-natural max-natural))
                   (unsigned (apply #'%make-interaction-receipt arguments))
                   (id (canonical-id
                        (%receipt-value unsigned :include-id nil))))
              (apply #'%make-interaction-receipt
                     (append arguments (list :id id))))))
      (interaction-refusal (condition) (error condition))
      (error (condition)
        (%refuse :reduction-fault "interaction reduction failed: ~A" condition)))))

(defun run-interaction-composition
    (composition inputs &key (max-rounds 100000) (max-natural 4096))
  "Execute and certify a pure natural-number Composition on interaction nets."
  (unless (and (integerp max-rounds) (plusp max-rounds))
    (%refuse :invalid-limit "max-rounds must be positive"))
  (%execute composition inputs max-rounds max-natural))

(defun verify-interaction-receipt (composition inputs receipt)
  "Replay RECEIPT from its Composition and inputs; reject any graft or drift."
  (and (interaction-receipt-p receipt)
       (handler-case
           (let ((fresh (%execute composition inputs
                                  (interaction-receipt-max-rounds receipt)
                                  (interaction-receipt-max-natural receipt))))
             (equal (interaction-receipt->value fresh)
                    (interaction-receipt->value receipt)))
         (error () nil))))
