;;;; dependent-types.lisp --- dependent-types vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (sigma-value
            (:constructor %make-sigma-value (base witness fiber-value)))
  "A dependent pair (x, y) where y inhabits the fiber over x."
  (base nil :read-only t)
  (witness nil :read-only t)
  (fiber-value nil :read-only t))

(defun make-sigma-value (family base-value fiber-value &key witness)
  "Construct a dependent pair for FAMILY.

BASE-VALUE must inhabit FAMILY's base type and FIBER-VALUE must inhabit the
fiber selected by BASE-VALUE."
  (let ((fiber (family-fiber family base-value)))
    (unless (in-type-p fiber fiber-value)
      (error "Sigma fiber value ~S is not in fiber ~S over ~S."
             fiber-value (hott-type-name fiber) base-value))
    (%make-sigma-value base-value witness fiber-value)))

(defun sigma-value-in-family-p (family value)
  "Return true when VALUE is a well-formed dependent pair for FAMILY."
  (and (typep value 'sigma-value)
       (in-type-p (hott-family-base family) (sigma-value-base value))
       (in-type-p (family-fiber family (sigma-value-base value))
                  (sigma-value-fiber-value value))))

(defun sigma-type (family &key (name :sigma) (truncation +set+))
  "Return the dependent sum type Σ(x:A).B(x) for FAMILY."
  (make-hott-type name
                  :predicate (lambda (x)
                               (sigma-value-in-family-p family x))
                  :truncation truncation))

(defstruct (pi-function
            (:constructor %make-pi-function (family function checker)))
  "A dependent function whose output type may vary with its input."
  (family nil :type hott-family :read-only t)
  (function nil :type function :read-only t)
  (checker nil :type function :read-only t))

(defun make-pi-function (family function &key checker)
  "Construct a dependent function over FAMILY.

CHECKER, when supplied, is called as (CHECKER FUNCTION BASE-TYPE) and can
perform finite-domain validation.  Pointwise calls are always checked by
PI-APPLY."
  (let ((check (or checker (constantly t))))
    (unless (funcall check function (hott-family-base family))
      (error "Dependent function failed checker for base type ~S."
             (hott-type-name (hott-family-base family))))
    (%make-pi-function family function check)))

(defun pi-apply (pi-function base-value)
  "Apply PI-FUNCTION at BASE-VALUE and check the selected fiber."
  (let* ((family (pi-function-family pi-function))
         (fiber (family-fiber family base-value))
         (out (funcall (pi-function-function pi-function) base-value)))
    (unless (in-type-p fiber out)
      (error "Pi function returned ~S outside fiber ~S over ~S."
             out (hott-type-name fiber) base-value))
    out))

(defun dependent-action-path (pi-function base-path &key path)
  "Return the dependent action on BASE-PATH for a dependent function.

This is the executable shape of `apd`: for f : Π(x:A).P(x), it witnesses
transport P p (f x)=f y."
  (let* ((family (pi-function-family pi-function))
         (from (pi-apply pi-function (hott-path-from base-path)))
         (to (pi-apply pi-function (hott-path-to base-path))))
    (make-dependent-path family base-path from to :path path)))

(defun pi-function-in-family-p (family value)
  "Return true when VALUE is a dependent function over FAMILY."
  (and (typep value 'pi-function)
       (eq (pi-function-family value) family)))

(defun pi-type (family &key (name :pi) (truncation +set+))
  "Return the dependent product type Π(x:A).B(x) for FAMILY."
  (make-hott-type name
                  :predicate (lambda (x)
                               (pi-function-in-family-p family x))
                  :truncation truncation))

(defun pi-extensional-path (type left right samples &key (test #'equal))
  "Build an endpoint path between dependent functions if SAMPLES agree.

This is an executable function-extensionality witness for finite/sample-based
domains: every sampled input must produce equal outputs under TEST."
  (unless (and (in-type-p type left)
               (in-type-p type right))
    (error "Both dependent functions must inhabit pi type ~S."
           (hott-type-name type)))
  (dolist (sample samples)
    (unless (funcall test (pi-apply left sample) (pi-apply right sample))
      (error "Dependent functions differ at sample ~S." sample)))
  (make-hott-path type left right
                  :witness (list :pi-extensional samples)))

