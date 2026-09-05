;;;; wire-graph.lisp --- immutable Wire interface model.

(in-package #:rosette-wire)

(defparameter +wire-schema+ "urn:rosette-wire:schema:component:1")
(defparameter +graph-schema+ "urn:rosette-wire:schema:composition:1")
(defparameter +receipt-schema+ "urn:rosette-wire:schema:receipt:1")

(define-condition wire-error (error)
  ((code :initarg :code :reader wire-error-code)
   (path :initarg :path :reader wire-error-path :initform "$")
   (detail :initarg :detail :reader wire-error-detail :initform ""))
  (:report (lambda (condition stream)
             (format stream "Wire ~A at ~A: ~A"
                     (wire-error-code condition)
                     (wire-error-path condition)
                     (wire-error-detail condition)))))

(defun %wire-error (code path control &rest arguments)
  (error 'wire-error :code code :path path
                     :detail (apply #'format nil control arguments)))

(defun %proper-list-p (value)
  (loop for tail = value then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %require-list (value path)
  (unless (%proper-list-p value)
    (%wire-error :invalid-list path "expected a proper list"))
  value)

(defun %require-name (value path)
  (unless (and (stringp value) (plusp (length value))
               (notany (lambda (character)
                         (member character
                                 '(#\Space #\Tab #\Newline #\Return)))
                       value))
    (%wire-error :invalid-name path
                 "expected a non-empty, whitespace-free string"))
  (copy-seq value))

(defun %require-unique-names (items name-function path)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((name (funcall name-function item)))
        (when (gethash name seen)
          (%wire-error :duplicate-name path "duplicate name ~S" name))
        (setf (gethash name seen) t))))
  items)

(defun %freeze-value (value)
  "Deep-copy a protocol value so caller-owned aggregates cannot mutate a model."
  (typecase value
    (string (copy-seq value))
    (cons (cons (%freeze-value (car value)) (%freeze-value (cdr value))))
    (vector (map 'vector #'%freeze-value value))
    (t value)))

(defstruct (wire-type
             (:constructor %make-wire-type (kind payload))
             (:conc-name %wire-type-))
  (kind nil :read-only t)
  (payload nil :read-only t))

(defun wire-type-kind (type)
  (check-type type wire-type)
  (%wire-type-kind type))

(defparameter +scalar-types+ '(:bool :s64 :u64 :f64 :string :bytes))

(defun scalar-type (name)
  (let ((kind (etypecase name
                (keyword name)
                (symbol (intern (string-upcase (symbol-name name)) :keyword))
                (string (intern (string-upcase name) :keyword)))))
    (unless (member kind +scalar-types+)
      (%wire-error :unknown-scalar "$.type" "unknown scalar type ~S" name))
    (%make-wire-type :scalar kind)))

(defun %unary-type (kind element)
  (check-type element wire-type)
  (%make-wire-type kind element))

(defun list-type (element) (%unary-type :list element))
(defun option-type (element) (%unary-type :option element))

(defun result-type (ok error)
  (check-type ok wire-type)
  (check-type error wire-type)
  (%make-wire-type :result (list ok error)))

(defun tuple-type (&rest elements)
  (dolist (element elements) (check-type element wire-type))
  (%make-wire-type :tuple (copy-list elements)))

(defstruct (wire-field
             (:constructor %make-wire-field (name type))
             (:conc-name %wire-field-))
  (name "" :type string :read-only t)
  (type nil :type wire-type :read-only t))

(defun make-wire-field (name type)
  (check-type type wire-type)
  (%make-wire-field (%require-name name "$.field.name") type))

(defun wire-field-name (field)
  (check-type field wire-field)
  (copy-seq (%wire-field-name field)))

(defun wire-field-type (field)
  (check-type field wire-field)
  (%wire-field-type field))

(defun %named-fields-type (kind fields path)
  (%require-list fields path)
  (dolist (field fields) (check-type field wire-field))
  (%require-unique-names fields #'%wire-field-name path)
  (%make-wire-type
   kind (sort (copy-list fields) #'string< :key #'%wire-field-name)))

(defun record-type (&rest fields)
  (%named-fields-type :record fields "$.type.fields"))

(defun variant-type (&rest cases)
  (%named-fields-type :variant cases "$.type.cases"))

(defparameter +tensor-dtypes+ '(:bool :s64 :u64 :f64))
(defparameter +tensor-layouts+ '(:row-major))
(defparameter +tensor-devices+ '(:host :any))
(defparameter +tensor-mutabilities+ '(:immutable :mutable-borrow))

(defun %keyword-value (value)
  (etypecase value
    (keyword value)
    (symbol (intern (string-upcase (symbol-name value)) :keyword))
    (string (intern (string-upcase value) :keyword))))

(defun tensor-type (dtype dimensions &key (layout :row-major)
                                           (device :any)
                                           (mutability :immutable))
  (%require-list dimensions "$.type.dimensions")
  (let ((dtype (%keyword-value dtype))
        (layout (%keyword-value layout))
        (device (%keyword-value device))
        (mutability (%keyword-value mutability)))
    (unless (member dtype +tensor-dtypes+)
      (%wire-error :invalid-tensor "$.type.dtype" "unsupported dtype ~S" dtype))
    (unless (member layout +tensor-layouts+)
      (%wire-error :invalid-tensor "$.type.layout" "unsupported layout ~S" layout))
    (unless (member device +tensor-devices+)
      (%wire-error :invalid-tensor "$.type.device" "unsupported device ~S" device))
    (unless (member mutability +tensor-mutabilities+)
      (%wire-error :invalid-tensor "$.type.mutability"
                   "unsupported mutability ~S" mutability))
    (dolist (dimension dimensions)
      (unless (or (and (integerp dimension) (not (minusp dimension)))
                  (eq dimension :dynamic)
                  (and (stringp dimension) (plusp (length dimension))))
        (%wire-error :invalid-tensor "$.type.dimensions"
                     "invalid dimension ~S" dimension)))
    (%make-wire-type :tensor
                     (list dtype (mapcar #'%freeze-value dimensions)
                           layout device mutability))))

(defparameter +resource-ownership+ '(:owned :borrowed :shared))

(defun resource-type (name owner ownership)
  (let ((ownership (%keyword-value ownership)))
    (unless (member ownership +resource-ownership+)
      (%wire-error :invalid-resource "$.type.ownership"
                   "unsupported ownership ~S" ownership))
    (%make-wire-type :resource
                     (list (%require-name name "$.type.name")
                           (%require-name owner "$.type.owner")
                           ownership))))

(defstruct (wire-operation
             (:constructor %make-wire-operation (name inputs output))
             (:conc-name %wire-operation-))
  (name "" :type string :read-only t)
  (inputs nil :type list :read-only t)
  (output nil :type wire-type :read-only t))

(defun make-wire-operation (name inputs output)
  (%require-list inputs "$.operation.inputs")
  (dolist (input inputs) (check-type input wire-field))
  (%require-unique-names inputs #'%wire-field-name "$.operation.inputs")
  (check-type output wire-type)
  (%make-wire-operation (%require-name name "$.operation.name")
                        (copy-list inputs) output))

(defun wire-operation-name (operation)
  (copy-seq (%wire-operation-name operation)))
(defun wire-operation-inputs (operation)
  (copy-list (%wire-operation-inputs operation)))
(defun wire-operation-output (operation) (%wire-operation-output operation))

(defstruct (wire-port
             (:constructor %make-wire-port (name operations))
             (:conc-name %wire-port-))
  (name "" :type string :read-only t)
  (operations nil :type list :read-only t))

(defun make-wire-port (name operations)
  (%require-list operations "$.port.operations")
  (dolist (operation operations) (check-type operation wire-operation))
  (%require-unique-names operations #'%wire-operation-name "$.port.operations")
  (%make-wire-port
   (%require-name name "$.port.name")
   (sort (copy-list operations) #'string< :key #'%wire-operation-name)))

(defun wire-port-name (port) (copy-seq (%wire-port-name port)))
(defun wire-port-operations (port) (copy-list (%wire-port-operations port)))

(defstruct (wire-descriptor
             (:constructor %make-wire-descriptor
                 (name version imports exports effects capabilities adapter verifiers))
             (:conc-name %wire-descriptor-))
  (name "" :type string :read-only t)
  (version "" :type string :read-only t)
  (imports nil :type list :read-only t)
  (exports nil :type list :read-only t)
  (effects nil :type list :read-only t)
  (capabilities nil :type list :read-only t)
  (adapter nil :read-only t)
  (verifiers nil :type list :read-only t))

(defparameter +effects+
  '(:pure :state :clock :random :filesystem-read :filesystem-write
    :network-client :network-listen :subprocess :native-ffi :accelerator))

(defun %semantic-version-p (version)
  (let* ((core-end (or (position-if (lambda (character)
                                      (or (char= character #\-)
                                          (char= character #\+)))
                                    version)
                       (length version)))
         (parts (uiop:split-string (subseq version 0 core-end)
                                   :separator '(#\.))))
    (and (= (length parts) 3)
         (every (lambda (part)
                  (and (plusp (length part))
                       (every #'digit-char-p part)
                       (or (string= part "0")
                           (char/= (char part 0) #\0))))
                parts))))

(defun make-wire-descriptor (&key name version imports exports effects
                                  capabilities adapter verifiers)
  (%require-list imports "$.imports")
  (%require-list exports "$.exports")
  (%require-list effects "$.effects")
  (%require-list capabilities "$.capabilities")
  (%require-list verifiers "$.verifiers")
  (dolist (port (append imports exports)) (check-type port wire-port))
  (%require-unique-names imports #'%wire-port-name "$.imports")
  (%require-unique-names exports #'%wire-port-name "$.exports")
  (let ((version (%require-name version "$.version"))
        (effect-values (mapcar #'%keyword-value effects)))
    (unless (%semantic-version-p version)
      (%wire-error :invalid-version "$.version" "expected semantic version, got ~S"
                   version))
    (dolist (effect effect-values)
      (unless (member effect +effects+)
        (%wire-error :unknown-effect "$.effects" "unknown effect ~S" effect)))
    (when (and (member :pure effect-values) (> (length effect-values) 1))
      (%wire-error :effect-conflict "$.effects" "PURE is exclusive"))
    (dolist (capability capabilities)
      (%require-name capability "$.capabilities"))
    (dolist (verifier verifiers)
      (%require-name verifier "$.verifiers"))
    (%make-wire-descriptor
     (%require-name name "$.name") version
     (sort (copy-list imports) #'string< :key #'%wire-port-name)
     (sort (copy-list exports) #'string< :key #'%wire-port-name)
     (sort (remove-duplicates effect-values :test #'eq)
           #'string< :key #'symbol-name)
     (sort (remove-duplicates (mapcar #'copy-seq capabilities) :test #'string=)
           #'string<)
     (%freeze-value (or adapter :empty-object))
     (sort (remove-duplicates (mapcar #'copy-seq verifiers) :test #'string=)
           #'string<))))

(defun wire-descriptor-name (descriptor)
  (copy-seq (%wire-descriptor-name descriptor)))
(defun wire-descriptor-version (descriptor)
  (copy-seq (%wire-descriptor-version descriptor)))
(defun wire-descriptor-imports (descriptor)
  (copy-list (%wire-descriptor-imports descriptor)))
(defun wire-descriptor-exports (descriptor)
  (copy-list (%wire-descriptor-exports descriptor)))
(defun wire-descriptor-effects (descriptor)
  (copy-list (%wire-descriptor-effects descriptor)))
(defun wire-descriptor-capabilities (descriptor)
  (mapcar #'copy-seq (%wire-descriptor-capabilities descriptor)))
(defun wire-descriptor-adapter (descriptor)
  (%freeze-value (%wire-descriptor-adapter descriptor)))
(defun wire-descriptor-verifiers (descriptor)
  (mapcar #'copy-seq (%wire-descriptor-verifiers descriptor)))
