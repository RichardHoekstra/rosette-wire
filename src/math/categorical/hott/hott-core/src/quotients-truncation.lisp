;;;; quotients-truncation.lisp --- quotients-truncation vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (hott-quotient
            (:constructor %make-hott-quotient
                (base relation canonicalize witness type)))
  "A set-truncated quotient of BASE by an executable relation."
  (base nil :type hott-type :read-only t)
  (relation nil :type function :read-only t)
  (canonicalize nil :type function :read-only t)
  (witness nil :read-only t)
  (type nil))

(defstruct (quotient-value
            (:constructor %make-quotient-value
                (quotient representative canonical witness)))
  "A quotient inhabitant with original and canonical representatives."
  (quotient nil :type hott-quotient :read-only t)
  (representative nil :read-only t)
  (canonical nil :read-only t)
  (witness nil :read-only t))

(defun quotient-value-in-quotient-p (quotient value)
  "Return true when VALUE is a well-formed inhabitant of QUOTIENT."
  (and (typep value 'quotient-value)
       (eq (quotient-value-quotient value) quotient)
       (in-type-p (hott-quotient-base quotient)
                  (quotient-value-representative value))
       (in-type-p (hott-quotient-base quotient)
                  (quotient-value-canonical value))
       (funcall (hott-quotient-relation quotient)
                (quotient-value-representative value)
                (quotient-value-canonical value))))

(defun make-hott-quotient (base relation &key
                                  canonicalize
                                  witness
                                  (name :quotient))
  "Construct a set-level quotient of BASE by RELATION.

RELATION is an executable equivalence-style predicate over BASE values.
CANONICALIZE maps a representative to its preferred representative; when
omitted it is identity.  The resulting quotient type has truncation +SET+."
  (let* ((canonical (or canonicalize #'identity))
         (quotient (%make-hott-quotient base relation canonical witness nil))
         (type (make-hott-type
                name
                :predicate (lambda (value)
                             (quotient-value-in-quotient-p quotient value))
                :truncation +set+)))
    (setf (hott-quotient-type quotient) type)
    quotient))

(defun make-quotient-value (quotient representative &key witness)
  "Inject REPRESENTATIVE into QUOTIENT."
  (let ((base (hott-quotient-base quotient)))
    (unless (in-type-p base representative)
      (error "Representative ~S is not in quotient base ~S."
             representative (hott-type-name base)))
    (let ((canonical (funcall (hott-quotient-canonicalize quotient)
                              representative)))
      (unless (in-type-p base canonical)
        (error "Canonical representative ~S is not in quotient base ~S."
               canonical (hott-type-name base)))
      (unless (funcall (hott-quotient-relation quotient)
                       representative canonical)
        (error "Representative ~S is not related to canonical value ~S."
               representative canonical))
      (%make-quotient-value quotient representative canonical witness))))

(defun quotient-related-p (quotient left right)
  "Return true when LEFT and RIGHT are related representatives in QUOTIENT."
  (and (quotient-value-in-quotient-p quotient left)
       (quotient-value-in-quotient-p quotient right)
       (funcall (hott-quotient-relation quotient)
                (quotient-value-representative left)
                (quotient-value-representative right))))

(defun quotient-path (quotient left right &key witness)
  "Return a quotient path when LEFT and RIGHT are relation-equivalent."
  (unless (quotient-related-p quotient left right)
    (error "Quotient values are not related: ~S and ~S." left right))
  (make-hott-path (hott-quotient-type quotient)
                  left right
                  :witness (or witness
                               (list :quotient
                                     (hott-quotient-witness quotient)))))

(defun set-truncation (type &key name)
  "Return TYPE viewed through its set truncation."
  (make-hott-type (or name (list :set-truncation (hott-type-name type)))
                  :predicate (hott-type-predicate type)
                  :truncation (if (truncation<= (hott-type-truncation type)
                                                +set+)
                                  (hott-type-truncation type)
                                  +set+)))

(defstruct (trunc-value
            (:constructor %make-trunc-value (base value witness)))
  "An inhabitant of the propositional truncation of BASE."
  (base nil :type hott-type :read-only t)
  (value nil :read-only t)
  (witness nil :read-only t))

(defun trunc-value-in-type-p (base value)
  "Return true when VALUE is a truncation wrapper over BASE."
  (and (typep value 'trunc-value)
       (eq (trunc-value-base value) base)
       (in-type-p base (trunc-value-value value))))

(defun propositional-truncation (base &key name)
  "Return the propositional truncation ||BASE||.

The wrapper retains the underlying witness for computation/debugging, while
the exposed type is a proposition and TRUNC-PATH connects any two inhabitants."
  (make-hott-type (or name (list :prop-truncation (hott-type-name base)))
                  :predicate (lambda (value)
                               (trunc-value-in-type-p base value))
                  :truncation +proposition+))

(defun make-trunc-value (base value &key witness)
  "Inject VALUE : BASE into ||BASE||."
  (unless (in-type-p base value)
    (error "Truncation value ~S is not in base type ~S."
           value (hott-type-name base)))
  (%make-trunc-value base value witness))

(defun trunc-path (trunc-type left right &key witness)
  "Return the propositional path connecting LEFT and RIGHT in TRUNC-TYPE."
  (unless (and (in-type-p trunc-type left)
               (in-type-p trunc-type right))
    (error "Both truncation values must inhabit ~S."
           (hott-type-name trunc-type)))
  (make-hott-path trunc-type left right
                  :witness (or witness :propositional-truncation)))

(defstruct (connected-map-witness
            (:constructor %make-connected-map-witness
                (source target function fiber-witness-fn)))
  "Runtime evidence that FUNCTION has merely inhabited homotopy fibers."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (function nil :type function :read-only t)
  (fiber-witness-fn nil :type function :read-only t))

(defun make-connected-map-witness
    (source target function fiber-witness-fn)
  "Construct evidence that FUNCTION : SOURCE -> TARGET is connected.

FIBER-WITNESS-FN is called on each target value and must return an inhabitant
of the corresponding homotopy fiber.  CONNECTED-FIBER-WITNESS-FOR wraps that
inhabitant in propositional truncation."
  (%make-connected-map-witness source target function fiber-witness-fn))

(defun connected-fiber-type (witness target-value &key name)
  "Return the homotopy-fiber type used by WITNESS over TARGET-VALUE."
  (unless (in-type-p (connected-map-witness-target witness) target-value)
    (error "Connected-map target value ~S is not in target type ~S."
           target-value
           (hott-type-name (connected-map-witness-target witness))))
  (homotopy-fiber-type
   (connected-map-witness-source witness)
   (connected-map-witness-target witness)
   (connected-map-witness-function witness)
   target-value
   :name (or name
             (list :connected-fiber
                   (hott-type-name
                    (connected-map-witness-source witness))
                   target-value))))

(defun connected-fiber-proposition (witness target-value &key name)
  "Return ||fiber FUNCTION TARGET-VALUE|| for WITNESS."
  (let ((fiber-type (connected-fiber-type witness target-value)))
    (make-hott-type
     (or name (list :merely-connected-fiber target-value))
     :predicate (lambda (value)
                  (and (typep value 'trunc-value)
                       (in-type-p fiber-type (trunc-value-value value))))
     :truncation +proposition+)))

(defun connected-fiber-witness-for (witness target-value)
  "Return the truncated fiber inhabitant over TARGET-VALUE."
  (let* ((fiber-type (connected-fiber-type witness target-value))
         (prop (connected-fiber-proposition witness target-value))
         (fiber-value
           (funcall (connected-map-witness-fiber-witness-fn witness)
                    target-value)))
    (unless (in-type-p fiber-type fiber-value)
      (error "Connected fiber witness for ~S is not in fiber type ~S: ~S."
             target-value (hott-type-name fiber-type) fiber-value))
    (let ((out (make-trunc-value fiber-type fiber-value
                                 :witness :connected-fiber)))
      (unless (in-type-p prop out)
        (error "Connected fiber truncation failed for ~S." target-value))
      out)))

(defun connected-on-p (witness samples)
  "Return true when WITNESS supplies truncated fibers for all SAMPLES."
  (every (lambda (sample)
           (typep (connected-fiber-witness-for witness sample)
                  'trunc-value))
         samples))

(defstruct (embedding-map-witness
            (:constructor %make-embedding-map-witness
                (source target function fiber-path-fn)))
  "Runtime evidence that FUNCTION has proposition-like homotopy fibers."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (function nil :type function :read-only t)
  (fiber-path-fn nil :type function :read-only t))

(defun make-embedding-map-witness
    (source target function fiber-path-fn)
  "Construct evidence that FUNCTION : SOURCE -> TARGET is an embedding.

FIBER-PATH-FN is called as (target-value left right fiber-type) and must
return a path LEFT=RIGHT in the homotopy fiber over TARGET-VALUE."
  (%make-embedding-map-witness source target function fiber-path-fn))

(defun embedding-fiber-type (witness target-value &key name)
  "Return the homotopy-fiber type used by embedding WITNESS."
  (unless (in-type-p (embedding-map-witness-target witness) target-value)
    (error "Embedding target value ~S is not in target type ~S."
           target-value
           (hott-type-name (embedding-map-witness-target witness))))
  (homotopy-fiber-type
   (embedding-map-witness-source witness)
   (embedding-map-witness-target witness)
   (embedding-map-witness-function witness)
   target-value
   :name (or name
             (list :embedding-fiber
                   (hott-type-name
                    (embedding-map-witness-source witness))
                   target-value))))

(defun embedding-fiber-path (witness target-value left right)
  "Return the proposition-style fiber path LEFT=RIGHT over TARGET-VALUE."
  (let* ((fiber-type (embedding-fiber-type witness target-value))
         (path (funcall (embedding-map-witness-fiber-path-fn witness)
                        target-value left right fiber-type)))
    (unless (and (in-type-p fiber-type left)
                 (in-type-p fiber-type right))
      (error "Embedding fiber values are not both over ~S." target-value))
    (unless (and (typep path 'hott-path)
                 (eq (hott-path-type path) fiber-type)
                 (equal (hott-path-from path) left)
                 (equal (hott-path-to path) right))
      (error "Embedding fiber path is not coherent over ~S." target-value))
    path))

(defun embedding-on-p (witness target-value fiber-values)
  "Return true when WITNESS connects sampled FIBER-VALUES pairwise."
  (loop for left in fiber-values
        always (loop for right in fiber-values
                     always (typep
                             (embedding-fiber-path witness
                                                   target-value
                                                   left
                                                   right)
                             'hott-path))))

(defun equiv-from-connected-embedding (connected embedding)
  "Build an equivalence from connected and embedding evidence for one map.

CONNECTED supplies a merely inhabited fiber over each target value.  EMBEDDING
turns each fiber into a proposition, so the retained truncated inhabitant is a
contractible center for that fiber."
  (unless (and (eq (connected-map-witness-source connected)
                   (embedding-map-witness-source embedding))
               (eq (connected-map-witness-target connected)
                   (embedding-map-witness-target embedding))
               (eq (connected-map-witness-function connected)
                   (embedding-map-witness-function embedding)))
    (error "Connected and embedding witnesses must describe the same map."))
  (let ((source (connected-map-witness-source connected))
        (target (connected-map-witness-target connected))
        (function (connected-map-witness-function connected)))
    (equiv-from-fiberwise-witness
     (make-fiberwise-equivalence-witness
      source target function
      (lambda (target-value)
        (let* ((fiber-type (embedding-fiber-type embedding target-value))
               (center
                 (trunc-value-value
                  (connected-fiber-witness-for connected target-value))))
          (unless (in-type-p fiber-type center)
            (error "Connected fiber center is not in embedding fiber ~S."
                   (hott-type-name fiber-type)))
          (make-contractible-witness
           fiber-type
           center
           (lambda (value)
             (embedding-fiber-path embedding
                                   target-value
                                   center
                                   value)))))))))

