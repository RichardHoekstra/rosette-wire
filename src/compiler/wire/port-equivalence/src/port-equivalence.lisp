;;;; port-equivalence.lisp --- port-equivalence implementation.

(in-package #:port-equivalence)

(define-condition port-equivalence-refusal (error)
  ((code :initarg :code :reader port-equivalence-refusal-code)
   (detail :initarg :detail :reader port-equivalence-refusal-detail))
  (:report (lambda (condition stream)
             (format stream "Port equivalence refused (~A): ~A"
                     (port-equivalence-refusal-code condition)
                     (port-equivalence-refusal-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'port-equivalence-refusal :code code
         :detail (apply #'format nil control arguments)))

(defun %name (value role)
  (unless (and (stringp value) (plusp (length value)))
    (%refuse :invalid-name "~A must be a non-empty string" role))
  (copy-seq value))

(defun %wire-type= (left right)
  (equal (wire-type->value left) (wire-type->value right)))

(defun %member-p (value samples test)
  (not (null (member value samples :test test))))

(defstruct (type-equivalence-certificate
            (:constructor %make-type-equivalence-certificate)
            (:copier nil))
  (name "" :type string :read-only t)
  (source-wire-type nil :read-only t)
  (target-wire-type nil :read-only t)
  (source-samples nil :type list :read-only t)
  (target-samples nil :type list :read-only t)
  (equivalence nil :type hott-equivalence :read-only t)
  (test #'equal :type function :read-only t)
  (id "" :type string :read-only t))

(defun %type-certificate-value (certificate &key include-id)
  (let ((value
          `(("name" . ,(type-equivalence-certificate-name certificate))
            ("schema" . "urn:rosette-wire:schema:type-equivalence:1")
            ("sourceSamples" .
             ,(copy-tree
               (type-equivalence-certificate-source-samples certificate)))
            ("sourceType" .
             ,(wire-type->value
               (type-equivalence-certificate-source-wire-type certificate)))
            ("targetSamples" .
             ,(copy-tree
               (type-equivalence-certificate-target-samples certificate)))
            ("targetType" .
             ,(wire-type->value
               (type-equivalence-certificate-target-wire-type certificate)))
            ("verdict" . "pass"))))
    (if include-id
        (acons "id" (type-equivalence-certificate-id certificate) value)
        value)))

(defun type-equivalence-certificate->value (certificate)
  (check-type certificate type-equivalence-certificate)
  (%type-certificate-value certificate :include-id t))

(defun %type-laws-p (certificate)
  (let* ((equivalence
           (type-equivalence-certificate-equivalence certificate))
         (inverse (inverse-equivalence equivalence))
         (test (type-equivalence-certificate-test certificate))
         (source (type-equivalence-certificate-source-samples certificate))
         (target (type-equivalence-certificate-target-samples certificate)))
    (handler-case
        (and
         (every (lambda (value)
                  (let ((forward (ua-beta equivalence value)))
                    (and (%member-p forward target test)
                         (funcall test (ua-beta inverse forward) value))))
                source)
         (every (lambda (value)
                  (let ((backward (ua-beta inverse value)))
                    (and (%member-p backward source test)
                         (funcall test (ua-beta equivalence backward) value))))
                target))
      (error () nil))))

(defun certify-type-equivalence
    (&key name source-wire-type target-wire-type forward backward
          source-samples target-samples (test #'equal))
  "Certify a finite carrier equivalence and its computational ua beta rule."
  (unless (wire-type-p source-wire-type)
    (%refuse :invalid-wire-type "source-wire-type is not a Rosette Wire type"))
  (unless (wire-type-p target-wire-type)
    (%refuse :invalid-wire-type "target-wire-type is not a Rosette Wire type"))
  (check-type forward function)
  (check-type backward function)
  (check-type test function)
  (unless (and (listp source-samples) source-samples
               (listp target-samples) target-samples)
    (%refuse :empty-carrier "both evidence carriers must be non-empty lists"))
  (let* ((name (%name name "equivalence name"))
         (source-copy (copy-tree source-samples))
         (target-copy (copy-tree target-samples))
         (source-type
           (make-hott-type
            (list :wire-source name)
            :predicate (lambda (value) (%member-p value source-copy test))))
         (target-type
           (make-hott-type
            (list :wire-target name)
            :predicate (lambda (value) (%member-p value target-copy test))))
         (equivalence
           (make-hott-equivalence
            source-type target-type forward backward
            :left-homotopy
            (lambda (value)
              (make-hott-path source-type
                              (funcall backward (funcall forward value)) value
                              :witness :finite-retraction))
            :right-homotopy
            (lambda (value)
              (make-hott-path target-type
                              (funcall forward (funcall backward value)) value
                              :witness :finite-section))))
         (arguments
           (list :name name :source-wire-type source-wire-type
                 :target-wire-type target-wire-type
                 :source-samples source-copy :target-samples target-copy
                 :equivalence equivalence :test test))
         (unsigned (apply #'%make-type-equivalence-certificate arguments)))
    (unless (%type-laws-p unsigned)
      (%refuse :not-an-equivalence
               "~A failed carrier, section, retraction, or cubical ua checks"
               name))
    (let ((id (canonical-id
               (%type-certificate-value unsigned :include-id nil))))
      (apply #'%make-type-equivalence-certificate
             (append arguments (list :id id))))))

(defun verify-type-equivalence-certificate (certificate)
  (and (type-equivalence-certificate-p certificate)
       (%type-laws-p certificate)
       (string=
        (type-equivalence-certificate-id certificate)
        (canonical-id (%type-certificate-value certificate :include-id nil)))))

(defun transport-value-forward (certificate value)
  (unless (verify-type-equivalence-certificate certificate)
    (%refuse :invalid-type-certificate "type certificate does not replay"))
  (ua-beta (type-equivalence-certificate-equivalence certificate) value))

(defun transport-value-backward (certificate value)
  (unless (verify-type-equivalence-certificate certificate)
    (%refuse :invalid-type-certificate "type certificate does not replay"))
  (ua-beta
   (inverse-equivalence
    (type-equivalence-certificate-equivalence certificate))
   value))

(defstruct (input-equivalence
            (:constructor %make-input-equivalence
                (source-field target-field certificate))
            (:copier nil))
  (source-field "" :type string :read-only t)
  (target-field "" :type string :read-only t)
  (certificate nil :type type-equivalence-certificate :read-only t))

(defun make-input-equivalence (source-field target-field certificate)
  (check-type certificate type-equivalence-certificate)
  (%make-input-equivalence (%name source-field "source field")
                           (%name target-field "target field") certificate))

(defstruct (operation-equivalence
            (:constructor %make-operation-equivalence
                (source-operation target-operation inputs output))
            (:copier nil))
  (source-operation "" :type string :read-only t)
  (target-operation "" :type string :read-only t)
  (inputs nil :type list :read-only t)
  (output nil :type type-equivalence-certificate :read-only t))

(defun make-operation-equivalence
    (&key source-operation target-operation inputs output)
  (unless (listp inputs) (%refuse :invalid-input-map "inputs must be a list"))
  (dolist (input inputs) (check-type input input-equivalence))
  (check-type output type-equivalence-certificate)
  (%make-operation-equivalence
   (%name source-operation "source operation")
   (%name target-operation "target operation")
   (copy-list inputs) output))

(defun %operation (port name)
  (find name (port-operations port)
        :test #'string= :key #'component-operation-name))

(defun %field (operation name)
  (find name (component-operation-inputs operation)
        :test #'string= :key #'component-field-name))

(defun %operation-value (operation)
  `(("inputs" .
     ,(mapcar (lambda (field)
                `(("name" . ,(component-field-name field))
                  ("type" . ,(wire-type->value (component-field-type field)))))
              (component-operation-inputs operation)))
    ("name" . ,(component-operation-name operation))
    ("output" . ,(wire-type->value (component-operation-output operation)))))

(defun %port-value (port)
  `(("name" . ,(port-name port))
    ("operations" . ,(mapcar #'%operation-value (port-operations port)))))

(defun %input-equivalence-value (input)
  `(("certificate" . ,(type-equivalence-certificate-id
                         (input-equivalence-certificate input)))
    ("sourceField" . ,(input-equivalence-source-field input))
    ("targetField" . ,(input-equivalence-target-field input))))

(defun %operation-equivalence-value (operation)
  `(("inputs" . ,(mapcar #'%input-equivalence-value
                           (operation-equivalence-inputs operation)))
    ("outputCertificate" .
     ,(type-equivalence-certificate-id
       (operation-equivalence-output operation)))
    ("sourceOperation" .
     ,(operation-equivalence-source-operation operation))
    ("targetOperation" .
     ,(operation-equivalence-target-operation operation))))

(defstruct (port-equivalence-certificate
            (:constructor %make-port-equivalence-certificate)
            (:copier nil))
  (source-port nil :type port :read-only t)
  (target-port nil :type port :read-only t)
  (operations nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %port-certificate-value (certificate &key include-id)
  (let ((value
          `(("operations" .
             ,(mapcar #'%operation-equivalence-value
                      (port-equivalence-certificate-operations certificate)))
            ("schema" . "urn:rosette-wire:schema:port-equivalence:1")
            ("sourcePort" .
             ,(%port-value
               (port-equivalence-certificate-source-port certificate)))
            ("targetPort" .
             ,(%port-value
               (port-equivalence-certificate-target-port certificate)))
            ("verdict" . "pass"))))
    (if include-id
        (acons "id" (port-equivalence-certificate-id certificate) value)
        value)))

(defun port-equivalence-certificate->value (certificate)
  (check-type certificate port-equivalence-certificate)
  (%port-certificate-value certificate :include-id t))

(defun %same-name-set-p (left right)
  (equal (sort (copy-list left) #'string<)
         (sort (copy-list right) #'string<)))

(defun %operation-equivalence-valid-p (source-port target-port mapping)
  (let* ((source (%operation source-port
                             (operation-equivalence-source-operation mapping)))
         (target (%operation target-port
                             (operation-equivalence-target-operation mapping)))
         (inputs (operation-equivalence-inputs mapping))
         (output (operation-equivalence-output mapping)))
    (and source target
         (verify-type-equivalence-certificate output)
         (%wire-type= (component-operation-output source)
                      (type-equivalence-certificate-source-wire-type output))
         (%wire-type= (component-operation-output target)
                      (type-equivalence-certificate-target-wire-type output))
         (%same-name-set-p
          (mapcar #'component-field-name
                  (component-operation-inputs source))
          (mapcar #'input-equivalence-source-field inputs))
         (%same-name-set-p
          (mapcar #'component-field-name
                  (component-operation-inputs target))
          (mapcar #'input-equivalence-target-field inputs))
         (every
          (lambda (input)
            (let ((source-field
                    (%field source (input-equivalence-source-field input)))
                  (target-field
                    (%field target (input-equivalence-target-field input)))
                  (certificate (input-equivalence-certificate input)))
              (and source-field target-field
                   (verify-type-equivalence-certificate certificate)
                   (%wire-type=
                    (component-field-type source-field)
                    (type-equivalence-certificate-source-wire-type certificate))
                   (%wire-type=
                    (component-field-type target-field)
                    (type-equivalence-certificate-target-wire-type certificate)))))
          inputs))))

(defun %port-laws-p (certificate)
  (let ((source (port-equivalence-certificate-source-port certificate))
        (target (port-equivalence-certificate-target-port certificate))
        (mappings (port-equivalence-certificate-operations certificate)))
    (and
     (%same-name-set-p
      (mapcar #'component-operation-name (port-operations source))
      (mapcar #'operation-equivalence-source-operation mappings))
     (%same-name-set-p
      (mapcar #'component-operation-name (port-operations target))
      (mapcar #'operation-equivalence-target-operation mappings))
     (every (lambda (mapping)
              (%operation-equivalence-valid-p source target mapping))
            mappings))))

(defun certify-port-equivalence (source-port target-port operations)
  "Certify a complete operation/field equivalence between two Ports."
  (check-type source-port port)
  (check-type target-port port)
  (unless (and (listp operations) operations)
    (%refuse :empty-port-map "operation equivalences must be non-empty"))
  (dolist (operation operations) (check-type operation operation-equivalence))
  (let* ((arguments (list :source-port source-port :target-port target-port
                          :operations (copy-list operations)))
         (unsigned (apply #'%make-port-equivalence-certificate arguments)))
    (unless (%port-laws-p unsigned)
      (%refuse :invalid-port-equivalence
               "operation maps are incomplete, duplicated, mistyped, or uncertified"))
    (let ((id (canonical-id (%port-certificate-value unsigned :include-id nil))))
      (apply #'%make-port-equivalence-certificate
             (append arguments (list :id id))))))

(defun verify-port-equivalence-certificate (certificate)
  (and (port-equivalence-certificate-p certificate)
       (%port-laws-p certificate)
       (string= (port-equivalence-certificate-id certificate)
                (canonical-id
                 (%port-certificate-value certificate :include-id nil)))))

(defun %mapping-for-target (certificate operation-name)
  (find operation-name (port-equivalence-certificate-operations certificate)
        :test #'string= :key #'operation-equivalence-target-operation))

(defun %alist-value (alist name)
  (let ((entry (and (listp alist) (assoc name alist :test #'string=))))
    (if entry (values (cdr entry) t) (values nil nil))))

(defun transport-operation-handler (certificate target-operation source-handler)
  "Produce the target Wire handler by cubically transporting one source handler."
  (unless (verify-port-equivalence-certificate certificate)
    (%refuse :invalid-port-certificate "Port certificate does not replay"))
  (check-type source-handler function)
  (let ((mapping (%mapping-for-target certificate target-operation)))
    (unless mapping
      (%refuse :unknown-operation "no target operation ~A" target-operation))
    (lambda (target-arguments services context)
      (let ((source-arguments nil))
        (dolist (input (operation-equivalence-inputs mapping))
          (multiple-value-bind (value present-p)
              (%alist-value target-arguments
                            (input-equivalence-target-field input))
            (unless present-p
              (%refuse :missing-argument "missing target argument ~A"
                       (input-equivalence-target-field input)))
            (push
             (cons (input-equivalence-source-field input)
                   (transport-value-backward
                    (input-equivalence-certificate input) value))
             source-arguments)))
        (transport-value-forward
         (operation-equivalence-output mapping)
         (funcall source-handler (nreverse source-arguments)
                  services context))))))

(defstruct (transport-correctness-receipt
            (:constructor %make-transport-correctness-receipt)
            (:copier nil))
  (port-equivalence-id "" :type string :read-only t)
  (operation "" :type string :read-only t)
  (implementation-id "" :type string :read-only t)
  (specification-id "" :type string :read-only t)
  (cases-id "" :type string :read-only t)
  (id "" :type string :read-only t))

(defun %correctness-value (receipt &key include-id)
  (let ((value
          `(("casesId" . ,(transport-correctness-receipt-cases-id receipt))
            ("implementationId" .
             ,(transport-correctness-receipt-implementation-id receipt))
            ("operation" . ,(transport-correctness-receipt-operation receipt))
            ("portEquivalenceId" .
             ,(transport-correctness-receipt-port-equivalence-id receipt))
            ("schema" . "urn:rosette-wire:schema:transport-proof:1")
            ("specificationId" .
             ,(transport-correctness-receipt-specification-id receipt))
            ("theorem" . "transported-extensional-equality")
            ("verdict" . "pass"))))
    (if include-id
        (acons "id" (transport-correctness-receipt-id receipt) value)
        value)))

(defun transport-correctness-receipt->value (receipt)
  (check-type receipt transport-correctness-receipt)
  (%correctness-value receipt :include-id t))

(defun %source-cases (mapping)
  (labels ((product (rows)
             (if (null rows)
                 (list nil)
                 (loop for value in (cdar rows)
                       append
                       (mapcar (lambda (tail)
                                 (acons (caar rows) value tail))
                               (product (cdr rows)))))))
    (product
     (mapcar
      (lambda (input)
        (cons (input-equivalence-source-field input)
              (type-equivalence-certificate-source-samples
               (input-equivalence-certificate input))))
      (operation-equivalence-inputs mapping)))))

(defun %correct-on-cases-p (implementation specification cases)
  (every (lambda (arguments)
           (equal (funcall implementation arguments nil nil)
                  (funcall specification arguments nil nil)))
         cases))

(defun certify-transported-correctness
    (certificate target-operation implementation specification
     &key implementation-id specification-id)
  "Derive equality of transported functions from finite source equality."
  (unless (verify-port-equivalence-certificate certificate)
    (%refuse :invalid-port-certificate "Port certificate does not replay"))
  (check-type implementation function)
  (check-type specification function)
  (let* ((mapping (%mapping-for-target certificate target-operation))
         (cases (and mapping (%source-cases mapping))))
    (unless mapping
      (%refuse :unknown-operation "no target operation ~A" target-operation))
    (unless (%correct-on-cases-p implementation specification cases)
      (%refuse :source-proof-failed
               "implementation and specification differ on the source carrier"))
    (let* ((arguments
             (list :port-equivalence-id
                   (port-equivalence-certificate-id certificate)
                   :operation (%name target-operation "target operation")
                   :implementation-id
                   (%name implementation-id "implementation id")
                   :specification-id
                   (%name specification-id "specification id")
                   :cases-id (canonical-id cases)))
           (unsigned (apply #'%make-transport-correctness-receipt arguments))
           (id (canonical-id (%correctness-value unsigned :include-id nil))))
      (apply #'%make-transport-correctness-receipt
             (append arguments (list :id id))))))

(defun verify-transported-correctness
    (certificate target-operation implementation specification receipt
     &key implementation-id specification-id)
  (and (transport-correctness-receipt-p receipt)
       (handler-case
           (let ((fresh
                   (certify-transported-correctness
                    certificate target-operation implementation specification
                    :implementation-id implementation-id
                    :specification-id specification-id)))
             (equal (transport-correctness-receipt->value fresh)
                    (transport-correctness-receipt->value receipt)))
         (error () nil))))
