;;;; frozen.lisp --- Explicit immutable hash-cons arenas for s-expressions.

(in-package #:rosette-form-core)

(defstruct (form-arena
            (:constructor %make-form-arena (table))
            (:copier nil))
  (table nil :read-only t))

(defstruct (frozen-form
            (:constructor %make-frozen-form
                (arena kind payload children address))
            (:copier nil))
  (arena nil :read-only t)
  (kind nil :read-only t)
  (payload nil :read-only t)
  (children nil :read-only t)
  (address nil :read-only t))

(defun make-form-arena ()
  "Return an empty explicit lifetime for immutable hash-consed forms."
  (%make-form-arena (make-hash-table :test #'equal)))

(defun form-arena-node-count (arena)
  "Return the number of structurally distinct nodes retained by ARENA."
  (check-type arena form-arena)
  (hash-table-count (form-arena-table arena)))

(defun %exact-number-key (number)
  (cond
    ((integerp number) (list :integer number))
    ((rationalp number)
     (list :ratio (numerator number) (denominator number)))
    ((floatp number)
     (handler-case
         (multiple-value-bind (significand exponent sign)
             (integer-decode-float number)
           ;; FLOAT-SIGN preserves the otherwise invisible sign of zero.
           (list :float (type-of number) significand exponent sign
                 (minusp (float-sign number))))
       (error ()
         (error "Unsupported non-finite float in frozen form: ~S" number))))
    ((complexp number)
     (list :complex (%exact-number-key (realpart number))
                    (%exact-number-key (imagpart number))))
    (t (error "Unsupported numeric atom in frozen form: ~S" number))))

(defun %copy-type-specifier (specifier)
  (labels ((copy (value)
             (cond ((consp value) (cons (copy (car value)) (copy (cdr value))))
                   ((or (symbolp value) (numberp value)) value)
                   (t (error "Unsupported array element-type component: ~S"
                             value)))))
    (copy specifier)))

(defun intern-frozen-form (arena value)
  "Freeze VALUE into ARENA and return its canonical immutable DAG root.

Composite caller objects are never retained.  Structural keys, rather than the
64-bit diagnostic address, authorize interning.  A frozen root from another
arena is imported structurally.  Cycles and unsupported mutable atoms fail
closed."
  (check-type arena form-arena)
  (when (and (frozen-form-p value)
             (eq arena (frozen-form-arena value)))
    (return-from intern-frozen-form value))
  (when (frozen-form-p value)
    (setf value (thaw-frozen-form value)))
  ;; Default EQL is object identity for the composite objects and structures
  ;; stored in these private traversal caches.
  (let ((active (make-hash-table))
        (objects (make-hash-table))
        (table (form-arena-table arena)))
    (labels
        ((intern-node (key kind payload children source)
           (multiple-value-bind (present found-p) (gethash key table)
             (if found-p present
                 (setf (gethash key table)
                       (%make-frozen-form arena kind payload children
                                          (form-address-of source))))))
         (composite (object thunk)
           (multiple-value-bind (present found-p) (gethash object objects)
           (when found-p (return-from composite present)))
           (when (gethash object active)
             (error "Cyclic structure cannot be frozen"))
           (setf (gethash object active) t)
           (unwind-protect
                (let ((result (funcall thunk)))
                  (setf (gethash object objects) result)
                  result)
             (remhash object active)))
         (freeze (node)
           (cond
             ((null node)
              (intern-node '(:null) :null nil nil node))
             ((consp node)
              (composite
               node
               (lambda ()
                 (let* ((left (freeze (car node)))
                        (right (freeze (cdr node)))
                        (children (list left right)))
                   (intern-node (list :cons left right)
                                :cons nil children node)))))
             ((symbolp node)
              (unless (symbol-package node)
                (error "Uninterned symbols have no durable frozen identity: ~S"
                       node))
              (intern-node (list :symbol
                                 (package-name (symbol-package node))
                                 (symbol-name node))
                           :symbol node nil node))
             ((stringp node)
              (let ((copy (copy-seq node)))
                (intern-node (list :string copy) :string copy nil node)))
             ((characterp node)
              (intern-node (list :character (char-code node))
                           :character node nil node))
             ((numberp node)
              (intern-node (%exact-number-key node) :number node nil node))
             ((arrayp node)
              (composite
               node
               (lambda ()
                 (let* ((dimensions (copy-list (array-dimensions node)))
                        (element-type
                          (%copy-type-specifier (array-element-type node)))
                        (adjustable-p (adjustable-array-p node))
                        (fill-pointer-p (array-has-fill-pointer-p node))
                        (fill-pointer (and fill-pointer-p (fill-pointer node)))
                        (children
                          (loop for index below (array-total-size node)
                                collect (freeze (row-major-aref node index))))
                        (metadata
                          (list :dimensions dimensions
                                :element-type element-type
                                :adjustable-p adjustable-p
                                :fill-pointer-p fill-pointer-p
                                :fill-pointer fill-pointer)))
                   (intern-node (list :array metadata children)
                                :array metadata children node)))))
             (t (error "Unsupported mutable atom in frozen form: ~S" node)))))
      (freeze value))))

(defun thaw-frozen-form (form)
  "Return a fresh mutable structural value represented by FORM.

No composite storage owned by the arena is exposed, and repeated DAG paths are
expanded independently so mutation of the result cannot affect another path."
  (check-type form frozen-form)
  (labels ((thaw (node)
             (case (frozen-form-kind node)
               (:null nil)
               (:cons
                (cons (thaw (first (frozen-form-children node)))
                      (thaw (second (frozen-form-children node)))))
               (:symbol (frozen-form-payload node))
               (:string (copy-seq (frozen-form-payload node)))
               (:character (frozen-form-payload node))
               (:number (frozen-form-payload node))
               (:array
                (let* ((metadata (frozen-form-payload node))
                       (arguments
                         (append
                          (list :element-type
                                (copy-tree (getf metadata :element-type))
                                :adjustable (getf metadata :adjustable-p))
                          (when (getf metadata :fill-pointer-p)
                            (list :fill-pointer
                                  (getf metadata :fill-pointer)))))
                       (array
                         (apply #'make-array
                                (cons (copy-list (getf metadata :dimensions))
                                      arguments))))
                  (loop for child in (frozen-form-children node)
                        for index from 0
                        do (setf (row-major-aref array index) (thaw child)))
                  array))
               (otherwise
                (error "Unknown frozen form node kind: ~S"
                       (frozen-form-kind node))))))
    (thaw form)))

(defun fold-frozen-form (function form)
  "Bottom-up fold the immutable DAG rooted at FORM exactly once per node.

FUNCTION receives (KIND SAFE-PAYLOAD CHILD-RESULTS).  Mutable payloads are
copied before the call.  CHILD-RESULTS follows structural order."
  (check-type form frozen-form)
  (let ((memo (make-hash-table)))
    (labels ((fold (node)
               (multiple-value-bind (present found-p) (gethash node memo)
                 (if found-p present
                     (let* ((payload
                              (case (frozen-form-kind node)
                                (:string
                                 (copy-seq (frozen-form-payload node)))
                                (:array
                                 (copy-tree (frozen-form-payload node)))
                                (otherwise (frozen-form-payload node))))
                            (children
                              (mapcar #'fold (frozen-form-children node)))
                            (result
                              (funcall function (frozen-form-kind node)
                                       payload children)))
                       (setf (gethash node memo) result)
                       result)))))
      (fold form))))
