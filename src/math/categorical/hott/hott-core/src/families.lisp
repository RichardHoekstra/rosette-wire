;;;; families.lisp --- families vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (hott-family
            (:constructor %make-hott-family
                (base fiber transport-fn)))
  "A dependent family of HoTT types over a BASE type.

FIBER maps a base value to a HoTT type descriptor.  TRANSPORT-FN moves a
value from the fiber over PATH's source to the fiber over PATH's target."
  (base nil :type hott-type :read-only t)
  (fiber nil :type function :read-only t)
  (transport-fn nil :type function :read-only t))

(defun make-hott-family (base fiber &key transport)
  "Construct a dependent family over BASE.

When TRANSPORT is omitted, transport is identity on values whose source and
target fibers are the same descriptor.  Supplying TRANSPORT lets callers model
nontrivial reindexing without exposing a proof-assistant-sized API."
  (%make-hott-family
   base
   fiber
   (or transport
       (lambda (path value source-fiber target-fiber)
         (declare (ignore path))
         (unless (eq source-fiber target-fiber)
           (error "No transport supplied between fibers ~S and ~S."
                  (hott-type-name source-fiber)
                  (hott-type-name target-fiber)))
         value))))

(defun family-fiber (family base-value)
  "Return FAMILY's fiber type over BASE-VALUE."
  (unless (in-type-p (hott-family-base family) base-value)
    (error "Base value ~S is not in family base type ~S."
           base-value
           (hott-type-name (hott-family-base family))))
  (let ((fiber (funcall (hott-family-fiber family) base-value)))
    (unless (typep fiber 'hott-type)
      (error "Family fiber for ~S is not a HoTT type: ~S."
             base-value fiber))
    fiber))

(defun transport (family path value)
  "Transport VALUE along PATH in dependent FAMILY.

PATH must live in FAMILY's base type and VALUE must inhabit the source fiber.
The returned value is checked against the target fiber."
  (unless (eq (hott-path-type path) (hott-family-base family))
    (error "Transport path is over ~S, expected family base ~S."
           (hott-type-name (hott-path-type path))
           (hott-type-name (hott-family-base family))))
  (let* ((source-fiber (family-fiber family (hott-path-from path)))
         (target-fiber (family-fiber family (hott-path-to path))))
    (unless (in-type-p source-fiber value)
      (error "Value ~S is not in source fiber ~S."
             value (hott-type-name source-fiber)))
    (let ((out (funcall (hott-family-transport-fn family)
                        path value source-fiber target-fiber)))
      (unless (in-type-p target-fiber out)
        (error "Transport produced ~S outside target fiber ~S."
               out (hott-type-name target-fiber)))
      out)))

(defun transport-refl (family base-value value)
  "Transport VALUE along reflexivity at BASE-VALUE."
  (transport family (refl (hott-family-base family) base-value) value))

(defun transport-compose (family left right value)
  "Transport VALUE along LEFT and then RIGHT."
  (transport family right (transport family left value)))

(defun transport-refl-p (family base-value value &key (test #'equal))
  "Check that transport along reflexivity leaves VALUE unchanged."
  (funcall test (transport-refl family base-value value) value))

(defun transport-compose-p (family left right value &key (test #'equal))
  "Check composed transport against sequential transport."
  (funcall test
           (transport family (path-compose left right) value)
           (transport-compose family left right value)))

(defstruct (dependent-path
            (:constructor %make-dependent-path
                (family base-path from to path)))
  "A pathover in a dependent family.

PATH witnesses transport(FAMILY, BASE-PATH, FROM)=TO in the target fiber."
  (family nil :type hott-family :read-only t)
  (base-path nil :type hott-path :read-only t)
  (from nil :read-only t)
  (to nil :read-only t)
  (path nil :type hott-path :read-only t))

(defun make-dependent-path (family base-path from to &key path)
  "Construct a checked dependent path over BASE-PATH."
  (unless (eq (hott-path-type base-path) (hott-family-base family))
    (error "Dependent path base path is over ~S, expected family base ~S."
           (hott-type-name (hott-path-type base-path))
           (hott-type-name (hott-family-base family))))
  (let* ((source-fiber (family-fiber family (hott-path-from base-path)))
         (target-fiber (family-fiber family (hott-path-to base-path)))
         (transported (transport family base-path from))
         (path-witness (or path
                           (make-hott-path target-fiber transported to
                                           :witness :dependent-path))))
    (unless (in-type-p source-fiber from)
      (error "Dependent path source ~S is not in source fiber ~S."
             from (hott-type-name source-fiber)))
    (unless (in-type-p target-fiber to)
      (error "Dependent path target ~S is not in target fiber ~S."
             to (hott-type-name target-fiber)))
    (unless (and (eq (hott-path-type path-witness) target-fiber)
                 (equal (hott-path-from path-witness) transported)
                 (equal (hott-path-to path-witness) to))
      (error "Dependent path witness must connect transported source to target."))
    (%make-dependent-path family base-path from to path-witness)))

(defun dependent-path-valid-p (pathover)
  "Return true when PATHOVER's stored witness still matches transport."
  (let* ((family (dependent-path-family pathover))
         (base-path (dependent-path-base-path pathover))
         (target-fiber (family-fiber family (hott-path-to base-path)))
         (transported (transport family base-path
                                 (dependent-path-from pathover)))
         (path (dependent-path-path pathover)))
    (and (in-type-p target-fiber (dependent-path-to pathover))
         (eq (hott-path-type path) target-fiber)
         (equal (hott-path-from path) transported)
         (equal (hott-path-to path) (dependent-path-to pathover)))))

(defun path-compose-associative-p (p q r &key (test #'equal))
  "Check the endpoint-level associativity law for path composition."
  (let ((left (path-compose (path-compose p q) r))
        (right (path-compose p (path-compose q r))))
    (and (funcall test (hott-path-from left) (hott-path-from right))
         (funcall test (hott-path-to left) (hott-path-to right))
         (eq (hott-path-type left) (hott-path-type right)))))

(defun path-left-unit-p (path &key (test #'equal))
  "Check the endpoint-level law refl(start) · PATH = PATH."
  (let ((left (path-compose (refl (hott-path-type path)
                                  (hott-path-from path))
                            path)))
    (and (eq (hott-path-type left) (hott-path-type path))
         (funcall test (hott-path-from left) (hott-path-from path))
         (funcall test (hott-path-to left) (hott-path-to path)))))

(defun path-right-unit-p (path &key (test #'equal))
  "Check the endpoint-level law PATH · refl(end) = PATH."
  (let ((right (path-compose path
                             (refl (hott-path-type path)
                                   (hott-path-to path)))))
    (and (eq (hott-path-type right) (hott-path-type path))
         (funcall test (hott-path-from right) (hott-path-from path))
         (funcall test (hott-path-to right) (hott-path-to path)))))

(defun path-inverse-left-p (path &key (test #'equal))
  "Check that inverse(PATH) · PATH is a loop at PATH's target."
  (let ((loop (path-compose (path-inverse path) path)))
    (and (eq (hott-path-type loop) (hott-path-type path))
         (funcall test (hott-path-from loop) (hott-path-to path))
         (funcall test (hott-path-to loop) (hott-path-to path)))))

(defun path-inverse-right-p (path &key (test #'equal))
  "Check that PATH · inverse(PATH) is a loop at PATH's source."
  (let ((loop (path-compose path (path-inverse path))))
    (and (eq (hott-path-type loop) (hott-path-type path))
         (funcall test (hott-path-from loop) (hott-path-from path))
         (funcall test (hott-path-to loop) (hott-path-from path)))))

(defun path-ap-compose-p (f g target-type path &key (test #'equal))
  "Check endpoint coherence for ap(g o f, PATH) versus ap(g, ap(f, PATH))."
  (let* ((mid-from (funcall f (hott-path-from path)))
         (mid-to (funcall f (hott-path-to path)))
         (mid-type (make-hott-type
                    (list :ap-compose-mid (hott-type-name target-type))
                    :predicate (lambda (value)
                                 (or (funcall test value mid-from)
                                     (funcall test value mid-to)))
                    :truncation (hott-type-truncation target-type)))
         (direct (path-ap (lambda (x) (funcall g (funcall f x)))
                          target-type
                          path))
         (staged (path-ap g target-type
                          (path-ap f mid-type path))))
    (and (eq (hott-path-type direct) (hott-path-type staged))
         (funcall test (hott-path-from direct) (hott-path-from staged))
         (funcall test (hott-path-to direct) (hott-path-to staged)))))

