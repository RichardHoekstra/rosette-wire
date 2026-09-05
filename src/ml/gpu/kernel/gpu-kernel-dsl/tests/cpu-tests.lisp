;;;; tests/cpu-tests.lisp --- CPU interpreter test suite.
;;;;
;;;; Proves the "one source, two backends" property: the SAME KERNEL-SPECs
;;;; the codegen lowers to CUDA-C are also executed on the host and produce
;;;; the expected results, exercising grid-stride, setq, let, if, call, and
;;;; atomic-add.

(defpackage #:rosette-gpu-kernel-dsl/cpu/tests
  (:use #:cl #:rosette-gpu-kernel-dsl #:rosette-gpu-kernel-dsl/cpu)
  (:export #:run-all-tests))

(in-package #:rosette-gpu-kernel-dsl/cpu/tests)

(defvar *test-count* 0)
(defvar *failure-count* 0)

(defmacro is (form &optional (message "assertion failed"))
  `(progn
     (incf *test-count*)
     (unless ,form
       (incf *failure-count*)
       (format t "  FAIL: ~A~%       form: ~S~%" ,message ',form))))

(defmacro test-case (name &body body)
  `(progn (format t "~&~A~%" ,name) ,@body))

(defun vec= (a b &optional (tol 1d-6))
  (and (= (length a) (length b))
       (every (lambda (x y) (<= (abs (- x y)) tol)) a b)))

;;; --- bundled kernels run on the CPU --------------------------------------

(defun test-vector-add ()
  (test-case "vector-add-kernel executes on CPU"
    (let ((a (vector 1d0 2d0 3d0 4d0))
          (b (vector 10d0 20d0 30d0 40d0))
          (c (make-array 4 :initial-element 0d0)))
      (interpret-kernel (vector-add-spec) a b c 4)
      (is (vec= c #(11d0 22d0 33d0 44d0)) "c = a + b"))))

(defun test-saxpy ()
  (test-case "saxpy-kernel executes on CPU"
    (let ((x (vector 1d0 2d0 3d0))
          (y (vector 10d0 20d0 30d0)))
      (interpret-kernel (saxpy-spec) 2d0 x y 3)
      (is (vec= y #(12d0 24d0 36d0)) "y = 2*x + y"))))

;;; --- a kernel exercising let / if / call ---------------------------------

(defkernel relu-sqrt-kernel (x :const-float* y :float* n :int)
  (for-grid-stride (i n)
    (let ((v x[i]))
      (if (< v 0d0)
          (setq y[i] 0d0)
          (setq y[i] (call sqrtf v))))))

(defun test-let-if-call ()
  (test-case "let + if + call (relu then sqrt)"
    (let ((x (vector -4d0 4d0 9d0 0d0))
          (y (make-array 4 :initial-element -1d0)))
      (interpret-kernel (find-kernel 'relu-sqrt-kernel) x y 4)
      (is (vec= y #(0d0 2d0 3d0 0d0)) "relu then sqrt"))))

;;; --- atomic-add as a serial reduction ------------------------------------

(defkernel sum-kernel (x :const-float* acc :float* n :int)
  (for-grid-stride (i n)
    (atomic-add acc[0] x[i])))

(defun test-atomic-add ()
  (test-case "atomic-add accumulates"
    (let ((x (vector 1d0 2d0 3d0 4d0 5d0))
          (acc (make-array 1 :initial-element 0d0)))
      (interpret-kernel (find-kernel 'sum-kernel) x acc 5)
      (is (= 15d0 (aref acc 0)) "sum = 15"))))

;;; --- one source, two backends: still lowers to CUDA-C --------------------

(defun test-dual-backend-invariant ()
  (test-case "the interpreted spec still compiles to CUDA-C source"
    (let ((src (compile-kernel-spec (vector-add-spec))))
      (is (search "__global__" src) "CUDA-C source emitted")
      (is (search "grid-stride" src) "grid-stride idiom present"))))

;;; --- binary16 carrier -----------------------------------------------------

(defkernel encode-known-halves
    (input :const-float* bytes :uint8* n :int)
  (for-grid-stride (i n)
    (let ((bits :int (float-to-half input[i]))
          (base :int (* i 2))
          (next-byte :int (+ base 1)))
      (setq bytes[base] (band bits 255))
      (setq bytes[next-byte] (band (shr bits 8) 255)))))

(defun test-float-to-half-carrier ()
  (test-case "float-to-half is RNE binary16 with byte-store parity"
    (let ((input #(0.0f0 -0.0f0 1.0f0 -2.0f0 0.5f0 65504.0f0))
          (bytes (make-array 12 :element-type '(unsigned-byte 8)
                               :initial-element 0)))
      (interpret-kernel-spec (find-kernel 'encode-known-halves)
                             (list input bytes (length input)))
      (is (equalp bytes #(0 0 0 128 0 60 0 192 0 56 255 123))
          "known values encode as little-endian IEEE binary16"))
    (let ((wide-byte (make-array 1 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
      (interpret-kernel-spec
       (make-kernel-spec :name 'byte-low-bits
                         :params '(out :uint8*)
                         :body '(setq out[0] 511))
       (list wide-byte))
      (is (= 255 (aref wide-byte 0))
          "mutable byte stores truncate to the low eight bits"))))

;;; --- CUDA builtins in the serial CPU witness -----------------------------

(defkernel builtin-witness-kernel (x :const-int* y :int* n :int)
  (for-grid-stride (i n)
    (setq y[i]
          (+ x[i]
             (thread-idx-x) (thread-idx-y) (thread-idx-z)
             (block-idx-x) (block-idx-y) (block-idx-z)
             (block-dim-x) (block-dim-y) (block-dim-z)
             (grid-dim-x) (grid-dim-y) (grid-dim-z)))))

(defun test-cuda-builtin-witness-values ()
  (test-case "CUDA builtins evaluate in the serial CPU witness"
    (let ((x (vector 1 2 3 4))
          (y (make-array 4 :initial-element 0)))
      (interpret-kernel (find-kernel 'builtin-witness-kernel) x y 4)
      (is (equalp y #(7 8 9 10))
          "thread/block indexes are 0 and dimensions are 1"))))

;;; --- runner ---------------------------------------------------------------

(defun run-all-tests ()
  (setf *test-count* 0 *failure-count* 0)
  (test-vector-add)
  (test-saxpy)
  (test-let-if-call)
  (test-atomic-add)
  (test-dual-backend-invariant)
  (test-float-to-half-carrier)
  (test-cuda-builtin-witness-values)
  (format t "~&~%rosette-gpu-kernel-dsl/cpu: ~D assertions, ~D failures~%"
          *test-count* *failure-count*)
  (when (plusp *failure-count*)
    (error "rosette-gpu-kernel-dsl/cpu: ~D test failure(s)" *failure-count*))
  t)
