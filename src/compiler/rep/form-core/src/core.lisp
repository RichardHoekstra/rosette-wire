;;;; core.lisp --- content-addressed Form primitives.

(in-package #:rosette-form-core)

(defstruct (form
            (:constructor %make-form (value address))
            (:copier nil))
  value
  address)

(defvar *form-table* (make-hash-table :test #'equal)
  "Live hash-cons table keyed by normalized Form value.")

(defconstant +fnv64-offset-basis+ 14695981039346656037)
(defconstant +fnv64-prime+ 1099511628211)
(defconstant +fnv64-mask+ #xffffffffffffffff)

(declaim (inline %hash-byte))
(declaim
 (ftype (function ((unsigned-byte 64) (unsigned-byte 8))
                  (unsigned-byte 64))
        %hash-byte))

(defun %hash-byte (hash byte)
  (declare (type (unsigned-byte 64) hash)
           (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 1)))
  (logand +fnv64-mask+
          (* (logxor hash byte) +fnv64-prime+)))

(defun %hash-string (hash string)
  (declare (type (unsigned-byte 64) hash)
           (type string string)
           (optimize (speed 3) (safety 1)))
  (let ((state hash))
    (declare (type (unsigned-byte 64) state))
    (loop for ch across string
          for code = (char-code ch)
          do (setf state (%hash-byte state (ldb (byte 8 0) code)))
             (setf state (%hash-byte state (ldb (byte 8 8) code)))
             (setf state (%hash-byte state (ldb (byte 8 16) code)))
             (setf state (%hash-byte state (ldb (byte 8 24) code))))
    state))

(defun %hash-atom (hash tag value)
  (%hash-string (%hash-byte hash tag) (prin1-to-string value)))

(defun %structural-hash (hash value)
  (cond
    ((null value) (%hash-byte hash 0))
    ((consp value)
     (%structural-hash
      (%structural-hash (%hash-byte hash 1) (car value))
      (cdr value)))
    ((symbolp value)
     (%hash-string
      (%hash-string (%hash-byte hash 2)
                    (or (package-name (symbol-package value)) ""))
      (symbol-name value)))
    ((stringp value) (%hash-string (%hash-byte hash 3) value))
    ((characterp value) (%hash-atom hash 4 value))
    ((numberp value) (%hash-atom hash 5 value))
    ((arrayp value)
     (loop with state = (%hash-atom hash 6 (array-dimensions value))
           for index below (array-total-size value)
           for element = (row-major-aref value index)
           do (setf state (%structural-hash state element))
           finally (return state)))
    (t (%hash-atom hash 7 value))))

(defun form-address-of (value)
  "Return a deterministic structural address for VALUE."
  (%structural-hash +fnv64-offset-basis+ value))

(defun %hash-byte-many (states byte)
  (dotimes (index (length states) states)
    (setf (aref states index) (%hash-byte (aref states index) byte))))

(declaim (inline %hash-byte-four))
(declaim
 (ftype (function ((simple-array (unsigned-byte 64) (4)) (unsigned-byte 8))
                  (simple-array (unsigned-byte 64) (4)))
        %hash-byte-four))

(defun %hash-byte-four (states byte)
  "Advance the canonical four content-id lanes without a dynamic inner loop."
  (declare (type (simple-array (unsigned-byte 64) (4)) states)
           (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 1)))
  (setf (aref states 0) (%hash-byte (aref states 0) byte)
        (aref states 1) (%hash-byte (aref states 1) byte)
        (aref states 2) (%hash-byte (aref states 2) byte)
        (aref states 3) (%hash-byte (aref states 3) byte))
  states)

(defun %hash-string-four (states string)
  (declare (type (simple-array (unsigned-byte 64) (4)) states)
           (type string string)
           (optimize (speed 3) (safety 1)))
  (loop for ch across string
        for code = (char-code ch)
        do (%hash-byte-four states (ldb (byte 8 0) code))
           (%hash-byte-four states (ldb (byte 8 8) code))
           (%hash-byte-four states (ldb (byte 8 16) code))
           (%hash-byte-four states (ldb (byte 8 24) code)))
  states)

(defun %hash-atom-four (states tag value)
  (%hash-string-four (%hash-byte-four states tag) (prin1-to-string value)))

(defun %structural-hash-four (states value)
  "Traverse VALUE once while advancing exactly four unboxed hash lanes."
  (declare (type (simple-array (unsigned-byte 64) (4)) states)
           (optimize (speed 3) (safety 1)))
  (cond
    ((null value) (%hash-byte-four states 0))
    ((consp value)
     (%structural-hash-four
      (%structural-hash-four (%hash-byte-four states 1) (car value))
      (cdr value)))
    ((symbolp value)
     (%hash-string-four
      (%hash-string-four (%hash-byte-four states 2)
                         (or (package-name (symbol-package value)) ""))
      (symbol-name value)))
    ((stringp value) (%hash-string-four (%hash-byte-four states 3) value))
    ((characterp value) (%hash-atom-four states 4 value))
    ((numberp value) (%hash-atom-four states 5 value))
    ((arrayp value)
     (let ((next (%hash-atom-four states 6 (array-dimensions value))))
       (loop for index below (array-total-size value)
             do (%structural-hash-four next (row-major-aref value index)))
       next))
    (t (%hash-atom-four states 7 value))))

(defun %hash-string-many (states string)
  "Hash STRING once through every independent state lane."
  (loop for ch across string
        for code = (char-code ch)
        do (%hash-byte-many states (ldb (byte 8 0) code))
           (%hash-byte-many states (ldb (byte 8 8) code))
           (%hash-byte-many states (ldb (byte 8 16) code))
           (%hash-byte-many states (ldb (byte 8 24) code)))
  states)

(defun %hash-atom-many (states tag value)
  (%hash-string-many (%hash-byte-many states tag) (prin1-to-string value)))

(defun %structural-hash-many (states value)
  "Traverse VALUE once while advancing every structural-hash state lane."
  (cond
    ((null value) (%hash-byte-many states 0))
    ((consp value)
     (%structural-hash-many
      (%structural-hash-many (%hash-byte-many states 1) (car value))
      (cdr value)))
    ((symbolp value)
     (%hash-string-many
      (%hash-string-many (%hash-byte-many states 2)
                         (or (package-name (symbol-package value)) ""))
      (symbol-name value)))
    ((stringp value) (%hash-string-many (%hash-byte-many states 3) value))
    ((characterp value) (%hash-atom-many states 4 value))
    ((numberp value) (%hash-atom-many states 5 value))
    ((arrayp value)
     (let ((next (%hash-atom-many states 6 (array-dimensions value))))
       (loop for index below (array-total-size value)
             do (%structural-hash-many
                 next (row-major-aref value index)))
       next))
    (t (%hash-atom-many states 7 value))))

(defun %domain-prefix-state (domain)
  "Return the hash state immediately before DOMAIN's final value cell."
  (let ((state +fnv64-offset-basis+))
    (dolist (element domain)
      (setf state (%structural-hash (%hash-byte state 1) element)))
    (%hash-byte state 1)))

(defun form-addresses-of-domain-separated (value domains)
  "Return addresses of (APPEND DOMAIN (LIST VALUE)) in one VALUE traversal.

DOMAINS is a non-empty list of proper-list prefixes. The result is bit-exactly
equivalent to calling FORM-ADDRESS-OF on each framed value independently."
  (unless (and (consp domains) (every #'listp domains))
    (error "Structural hash domains must be a non-empty list of lists."))
  (let ((states
          (make-array (length domains) :element-type '(unsigned-byte 64)
                      :initial-contents
                      (mapcar #'%domain-prefix-state domains))))
    (if (= 4 (length domains))
        (%structural-hash-four states value)
        (%structural-hash-many states value))
    ;; Close the final value cell's cdr with NIL, matching ordinary list hash.
    (if (= 4 (length domains))
        (%hash-byte-four states 0)
        (%hash-byte-many states 0))
    (coerce states 'list)))

(defun make-form (value)
  "Create a non-interned Form handle for VALUE."
  (%make-form value (form-address-of value)))

(defun intern-form (value)
  "Return the canonical live Form handle for VALUE."
  (or (gethash value *form-table*)
      (setf (gethash value *form-table*) (make-form value))))

(defun walk-form (fn form-or-value)
  "Preorder-walk FORM-OR-VALUE, calling FN on every node value."
  (labels ((walk (value)
             (funcall fn value)
             (when (consp value)
               (walk (car value))
               (walk (cdr value)))))
    (walk (if (form-p form-or-value)
              (form-value form-or-value)
              form-or-value)))
  form-or-value)

(defun pprint-form (form-or-value &optional (stream *standard-output*))
  "Pretty-print FORM-OR-VALUE's structural value."
  (pprint (if (form-p form-or-value)
              (form-value form-or-value)
              form-or-value)
          stream))
