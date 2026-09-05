;;;; core.lisp --- executable HoTT vocabulary.

(in-package #:rosette-hott-core)

(defconstant +contractible+ -2)
(defconstant +proposition+ -1)
(defconstant +set+ 0)
(defconstant +groupoid+ 1)

(defun truncation-level-p (level)
  "Return true when LEVEL is an integer HoTT truncation level."
  (integerp level))

(defun truncation<= (a b)
  "Return true when truncation level A is no higher than B."
  (and (truncation-level-p a)
       (truncation-level-p b)
       (<= a b)))

(defstruct (hott-type
            (:constructor %make-hott-type (name predicate truncation)))
  "A runtime carrier for a HoTT type approximation."
  (name nil :read-only t)
  (predicate (constantly t) :type function :read-only t)
  (truncation +set+ :type integer :read-only t))

(defun make-hott-type (name &key (predicate (constantly t)) (truncation +set+))
  "Construct a HoTT type descriptor.

PREDICATE is the runtime membership predicate.  TRUNCATION follows the
HoTT convention: -2 contractible, -1 proposition, 0 set, 1 groupoid, etc."
  (unless (truncation-level-p truncation)
    (error "Invalid HoTT truncation level: ~S." truncation))
  (%make-hott-type name predicate truncation))

(defun in-type-p (type value)
  "Return true when VALUE is accepted by TYPE's runtime predicate."
  (funcall (hott-type-predicate type) value))

(defun ensure-in-type (type value control &rest args)
  "Signal CONTROL unless VALUE is accepted by TYPE, then return VALUE."
  (unless (in-type-p type value)
    (apply #'error control args))
  value)

(defstruct (hott-path
            (:constructor %make-hott-path (type from to witness)))
  "A path/identity witness between two inhabitants of TYPE."
  (type nil :type hott-type :read-only t)
  (from nil :read-only t)
  (to nil :read-only t)
  (witness nil :read-only t))

(defun refl (type value)
  "Return the reflexivity path VALUE = VALUE in TYPE."
  (unless (in-type-p type value)
    (error "Value ~S is not in HoTT type ~S." value (hott-type-name type)))
  (%make-hott-path type value value :refl))

(defun make-hott-path (type from to &key witness)
  "Construct an explicit path witness FROM = TO in TYPE."
  (unless (and (in-type-p type from)
               (in-type-p type to))
    (error "Path endpoints ~S and ~S are not both in HoTT type ~S."
           from to (hott-type-name type)))
  (%make-hott-path type from to (or witness :given)))

(defstruct (pointed-type
            (:constructor %make-pointed-type (type basepoint)))
  "A HoTT type equipped with a distinguished basepoint."
  (type nil :type hott-type :read-only t)
  (basepoint nil :read-only t))

(defun make-pointed-type (type basepoint)
  "Construct a pointed type (TYPE, BASEPOINT)."
  (unless (in-type-p type basepoint)
    (error "Basepoint ~S is not in HoTT type ~S."
           basepoint (hott-type-name type)))
  (%make-pointed-type type basepoint))

(defstruct (pointed-map
            (:constructor %make-pointed-map
                (source target function basepoint-path)))
  "A basepoint-preserving map between pointed types.

BASEPOINT-PATH witnesses f(source.basepoint) = target.basepoint."
  (source nil :type pointed-type :read-only t)
  (target nil :type pointed-type :read-only t)
  (function nil :type function :read-only t)
  (basepoint-path nil :type hott-path :read-only t))

(defun make-pointed-map (source target function &key basepoint-path)
  "Construct a checked pointed map SOURCE -> TARGET."
  (let* ((source-point (pointed-type-basepoint source))
         (target-point (pointed-type-basepoint target))
         (image (funcall function source-point))
         (target-type (pointed-type-type target))
         (path (or basepoint-path
                   (make-hott-path target-type image target-point
                                   :witness :pointed-map-basepoint))))
    (unless (in-type-p target-type image)
      (error "Pointed map sends basepoint to ~S outside target ~S."
             image (hott-type-name target-type)))
    (unless (and (eq (hott-path-type path) target-type)
                 (equalp (hott-path-from path) image)
                 (equalp (hott-path-to path) target-point))
      (error "Pointed map basepoint path must witness f(base)=target base."))
    (%make-pointed-map source target function path)))

