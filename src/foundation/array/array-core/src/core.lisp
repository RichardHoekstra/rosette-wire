;;;; core.lisp --- Typed array primitives.

(in-package #:rosette-array-core)

(deftype f-scalar (kind)
  "Scalar float type selected by KIND, currently :F64 or :F32."
  (float-kind-type kind))

(deftype f-array (kind &optional (dimensions '*))
  "A simple float array selected by KIND with DIMENSIONS."
  `(simple-array ,(float-kind-type kind) ,dimensions))

(deftype f-vector (kind &optional (length '*))
  "A simple one-dimensional float array selected by KIND with LENGTH."
  `(f-array ,kind (,length)))

(deftype f64-array (&optional (dimensions '*))
  "A simple array specialized to DOUBLE-FLOAT with DIMENSIONS."
  `(f-array :f64 ,dimensions))

(deftype f32-array (&optional (dimensions '*))
  "A simple array specialized to SINGLE-FLOAT with DIMENSIONS."
  `(f-array :f32 ,dimensions))

(declaim (inline shape3))
(defun shape3 (nx ny nz)
  "Return a 3D array dimension list in canonical (NX NY NZ) order."
  (list nx ny nz))

(defun %dimension-total-size (dimensions)
  (etypecase dimensions
    ((integer 0 *) dimensions)
    (list (reduce #'* dimensions :initial-value 1))))

(defun f-array-storage-bytes (kind dimensions)
  "Return the storage byte count for a float array of KIND and DIMENSIONS."
  (* (%dimension-total-size dimensions)
     (float-kind-bytes kind)))

(defun make-f-array
    (kind dimensions &key initial-element
                       (initial-contents nil initial-contents-p))
  "Allocate a simple float array of KIND with DIMENSIONS."
  (ecase kind
    (:f64
     (if initial-contents-p
         (make-array dimensions
                     :element-type 'double-float
                     :initial-contents initial-contents)
         (make-array dimensions
                     :element-type 'double-float
                     :initial-element (if initial-element
                                          (as-float-kind :f64 initial-element)
                                          0d0))))
    (:f32
     (if initial-contents-p
         (make-array dimensions
                     :element-type 'single-float
                     :initial-contents initial-contents)
         (make-array dimensions
                     :element-type 'single-float
                     :initial-element (if initial-element
                                          (as-float-kind :f32 initial-element)
                                          0f0))))))

(defun make-double-float-array
    (dimensions &key (initial-element 0d0)
                     (initial-contents nil initial-contents-p))
  "Allocate a double-float array with DIMENSIONS."
  (if initial-contents-p
      (make-f-array :f64 dimensions :initial-contents initial-contents)
      (make-f-array :f64 dimensions :initial-element initial-element)))

(defun make-single-float-array
    (dimensions &key (initial-element 0f0)
                     (initial-contents nil initial-contents-p))
  "Allocate a single-float array with DIMENSIONS."
  (if initial-contents-p
      (make-f-array :f32 dimensions :initial-contents initial-contents)
      (make-f-array :f32 dimensions :initial-element initial-element)))

(defun copy-array (array &key element-type)
  "Return a same-shaped array copy, preserving ARRAY's element type by default."
  (let ((out (make-array (array-dimensions array)
                         :element-type (or element-type
                                           (array-element-type array)))))
    (if (= (array-rank array) 1)
        (replace out array)
        (dotimes (i (array-total-size array) out)
          (setf (row-major-aref out i) (row-major-aref array i))))))

(defun array-same-dimensions-p (a b)
  "Return T iff A and B have identical rank and dimensions."
  (let ((rank (array-rank a)))
    (and (= rank (array-rank b))
         (loop for axis fixnum below rank
               always (= (array-dimension a axis)
                         (array-dimension b axis))))))
