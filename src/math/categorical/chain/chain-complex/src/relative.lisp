;;;; relative.lisp --- coordinate subcomplex quotients with a closure receipt.

(in-package #:rosette-chain-complex)

(define-condition relative-chain-error (error)
  ((dimension :initarg :dimension :reader relative-chain-error-dimension)
   (row :initarg :row :initform nil :reader relative-chain-error-row)
   (column :initarg :column :initform nil :reader relative-chain-error-column)
   (value :initarg :value :initform nil :reader relative-chain-error-value)
   (message :initarg :message :reader %relative-chain-error-message))
  (:report (lambda (condition stream)
             (format stream "Relative-chain obstruction in degree ~D: ~A"
                     (relative-chain-error-dimension condition)
                     (%relative-chain-error-message condition)))))

(defstruct (relative-chain-complex
            (:constructor %make-relative-chain-complex)
            (:conc-name %relative-)
            (:copier nil))
  ambient
  (subspace-indices nil :type list :read-only t)
  quotient
  (receipt nil :type list :read-only t))

(defun relative-chain-complex-ambient (relative)
  (%relative-ambient relative))

(defun relative-chain-complex-subspace-indices (relative)
  (copy-tree (%relative-subspace-indices relative)))

(defun relative-chain-complex-quotient (relative)
  (%relative-quotient relative))

(defun relative-chain-complex-receipt (relative)
  (copy-tree (%relative-receipt relative)))

(defun %relative-fail (dimension message &key row column value)
  (error 'relative-chain-error :dimension dimension :message message
         :row row :column column :value value))

(defun %canonical-coordinate-set (indices dimension degree)
  (unless (listp indices)
    (%relative-fail degree "subspace coordinates must be a proper list"))
  (let ((copy (sort (copy-list indices) #'<)))
    (loop for tail on copy
          for index = (first tail) do
      (unless (and (integerp index) (<= 0 index) (< index dimension))
        (%relative-fail degree
                        (format nil "coordinate ~S is outside [0,~D)"
                                index dimension)
                        :column index))
      (when (and (rest tail) (= index (second tail)))
        (%relative-fail degree
                        (format nil "coordinate ~D is duplicated" index)
                        :column index)))
    copy))

(defun %coordinate-complement (dimension removed)
  (loop for index below dimension
        unless (member index removed :test #'=)
          collect index))

(defun %relative-matrix-entry (matrix row column)
  (if matrix (nth column (nth row matrix)) 0))

(defun %check-coordinate-subcomplex (ambient selected)
  "Prove d(A_n) is contained in A_(n-1) for every coordinate generator."
  (loop for degree from 1 to (chain-complex-top ambient)
        for boundary = (boundary-matrix ambient degree)
        for lower-selected = (nth (1- degree) selected)
        for upper-selected = (nth degree selected)
        for lower-dimension = (nth (1- degree)
                                   (chain-complex-dims ambient)) do
    (dolist (column upper-selected)
      (dotimes (row lower-dimension)
        (unless (or (member row lower-selected :test #'=)
                    (zerop (%relative-matrix-entry boundary row column)))
          (%relative-fail
           degree
           "the selected coordinates are not closed under the boundary"
           :row row :column column
           :value (%relative-matrix-entry boundary row column))))))
  t)

(defun %quotient-boundary (boundary kept-rows kept-columns)
  (when (and boundary kept-rows kept-columns)
    (loop for row in kept-rows
          collect (loop for column in kept-columns
                        collect (%relative-matrix-entry boundary row column)))))

(defun make-relative-chain-complex (ambient subspace-indices)
  "Construct the coordinate quotient C_*(AMBIENT)/C_*(A).

SUBSPACE-INDICES contains one coordinate-index list per chain degree.  The
constructor first proves that those basis vectors form a subcomplex, then
deletes the selected rows and columns from every boundary map."
  (unless (chain-complex-p ambient)
    (error 'type-error :datum ambient :expected-type 'chain-complex))
  (let ((dimensions (copy-list (chain-complex-dims ambient))))
    (unless (= (length subspace-indices) (length dimensions))
      (%relative-fail
       0 (format nil "expected ~D coordinate sets, received ~D"
                 (length dimensions) (length subspace-indices))))
    (let* ((selected
             (loop for indices in subspace-indices
                   for dimension in dimensions
                   for degree from 0
                   collect (%canonical-coordinate-set indices dimension degree)))
           (kept
             (loop for dimension in dimensions
                   for indices in selected
                   collect (%coordinate-complement dimension indices))))
      (%check-coordinate-subcomplex ambient selected)
      (let* ((ambient-copy
               (make-chain-complex
                (copy-tree (chain-complex-boundaries ambient)) dimensions))
             (quotient-boundaries
               (loop for degree from 1 to (chain-complex-top ambient)
                     collect (%quotient-boundary
                              (boundary-matrix ambient degree)
                              (nth (1- degree) kept)
                              (nth degree kept))))
             (quotient-dimensions (mapcar #'length kept))
             (quotient (make-chain-complex quotient-boundaries
                                           quotient-dimensions))
             (receipt
               (list :version 1
                     :construction :coordinate-chain-quotient
                     :ambient-dimensions dimensions
                     :subspace-indices (copy-tree selected)
                     :quotient-dimensions quotient-dimensions
                     :subcomplex-closed-p t
                     :d-squared-zero-p (d-squared-zero-p quotient)
                     :boundary-residuals
                     (copy-tree (d-squared-residual quotient)))))
        (%make-relative-chain-complex
         :ambient ambient-copy :subspace-indices selected
         :quotient quotient :receipt receipt)))))

(defun relative-chain-complex-valid-p (relative)
  "Replay the closure and quotient boundary laws of RELATIVE."
  (and (relative-chain-complex-p relative)
       (handler-case
           (let* ((ambient (%relative-ambient relative))
                  (selected (%relative-subspace-indices relative))
                  (replayed (make-relative-chain-complex ambient selected))
                  (left (%relative-quotient relative))
                  (right (%relative-quotient replayed)))
             (and (equal (chain-complex-dims left)
                         (chain-complex-dims right))
                  (equal (chain-complex-boundaries left)
                         (chain-complex-boundaries right))
                  (d-squared-zero-p left)
                  (equal (%relative-receipt relative)
                         (%relative-receipt replayed))))
         (error () nil))))
