;;;; bundled-kernels.lisp --- Worked example kernels used by tests + docs
;;;;
;;;; Two canonical examples:
;;;;
;;;;   VECTOR-ADD  : c[i] = a[i] + b[i]      (CUDA-by-Example chapter 4)
;;;;   SAXPY       : y[i] = alpha * x[i] + y[i]  (BLAS Level-1)
;;;;
;;;; Both are constructed via DEFKERNEL at load time and bound to top-
;;;; level constants for ease of inspection.  Each ships with a thin
;;;; convenience wrapper (the function VECTOR-ADD / SAXPY) that builds
;;;; an args list and calls LAUNCH-KERNEL with a default block size of
;;;; 256 and the ceiling grid size for n elements.

(in-package #:rosette-gpu-kernel-dsl)

;;; --- vector-add ------------------------------------------------------

(defkernel vector-add-kernel (a :const-float* b :const-float*
                              c :float* n :int)
  (for-grid-stride (i n)
    (setq c[i] (+ a[i] b[i]))))

(defparameter *vector-add-spec* (find-kernel 'vector-add-kernel)
  "Cached KERNEL-SPEC for the vector-add kernel.")

(defun vector-add-spec ()
  "Return the registered KERNEL-SPEC for vector-add."
  *vector-add-spec*)

(defun %ceil-div (a b)
  (declare (type integer a b))
  (floor (+ a b -1) b))

(defun vector-add (a b c n &key (block-x 256))
  "Launch the vector-add kernel.  Computes c[i] = a[i] + b[i] for
   0 <= i < N on the device.

   A, B, C : ROSETTE-GPU-TENSORs (or any value the bridge accepts).
   N       : positive integer, total element count.
   :BLOCK-X: threads per block on the X axis (default 256).

   Returns the rc of rosette-gpu-bridge:NVRTC-LAUNCH.

   Signals rosette-gpu-bridge:GPU-NOT-AVAILABLE on a CPU-only host."
  (declare (type integer n))
  (let ((grid (list (%ceil-div n block-x) 1 1))
        (block (list block-x 1 1)))
    (launch-kernel *vector-add-spec* grid block a b c n)))

;;; --- saxpy -----------------------------------------------------------

(defkernel saxpy-kernel (alpha :float
                         x :const-float*
                         y :float*
                         n :int)
  (for-grid-stride (i n)
    (setq y[i] (+ (* alpha x[i]) y[i]))))

(defparameter *saxpy-spec* (find-kernel 'saxpy-kernel)
  "Cached KERNEL-SPEC for the saxpy kernel.")

(defun saxpy-spec ()
  "Return the registered KERNEL-SPEC for saxpy."
  *saxpy-spec*)

(defun saxpy (alpha x y n &key (block-x 256))
  "Launch the saxpy (Single-precision A·X Plus Y) kernel.  Computes
   y[i] = alpha * x[i] + y[i] for 0 <= i < N on the device.

   ALPHA : single-float scalar.
   X, Y  : ROSETTE-GPU-TENSORs.
   N     : positive integer, total element count.
   :BLOCK-X : threads per block on the X axis (default 256).

   Returns the rc of rosette-gpu-bridge:NVRTC-LAUNCH.

   Signals rosette-gpu-bridge:GPU-NOT-AVAILABLE on a CPU-only host."
  (declare (type integer n))
  (let ((grid (list (%ceil-div n block-x) 1 1))
        (block (list block-x 1 1)))
    (launch-kernel *saxpy-spec* grid block
                   (coerce alpha 'single-float) x y n)))
