;;;; launch-base.lisp --- Validated base launch API.

(in-package #:rosette-gpu-kernel-dsl)

(defun normalize-launch-dim (dim name)
  "Return DIM as a canonical three-integer CUDA launch dimension.

DIM may be a positive integer, interpreted as `(DIM 1 1)', or a list of
one to three positive integers.  Short lists are padded with ones."
  (labels ((positive-int-p (x)
             (and (integerp x) (plusp x))))
    (cond
      ((positive-int-p dim)
       (list dim 1 1))
      ((and (listp dim)
            (<= 1 (length dim) 3)
            (every #'positive-int-p dim))
       (append dim (make-list (- 3 (length dim)) :initial-element 1)))
      (t
       (error 'dsl-syntax-error
              :form dim
              :reason (format nil "~A must be a positive integer or 1-3 positive integers"
                              name))))))

(defun validate-launch-args (spec args)
  "Validate ARGS against SPEC's parameter count and return ARGS.

The base system intentionally validates only arity and launch geometry.
Device-pointer/scalar ABI validation belongs to the optional bridge,
because it has access to `rosette-gpu-bridge' tensor objects."
  (unless (kernel-spec-p spec)
    (error 'dsl-syntax-error :form spec :reason "launch spec must be a KERNEL-SPEC"))
  (let ((expected (length (kernel-spec-params spec)))
        (actual (length args)))
    (unless (= expected actual)
      (error 'dsl-syntax-error
             :form args
             :reason (format nil "kernel ~S expects ~D args, got ~D"
                             (kernel-spec-name spec) expected actual))))
  args)

(defun %bridge-unavailable (spec)
  (error 'gpu-kernel-bridge-unavailable :spec spec))

(defun launch-kernel (spec grid-dim block-dim &rest args)
  "Validate a launch request, then signal that the NVRTC bridge is absent.

The base `rosette-gpu-kernel-dsl' system is pure code generation and does
not depend on CUDA libraries.  Load `rosette-gpu-kernel-dsl/bridge' to
replace this function with the NVRTC-backed implementation."
  (normalize-launch-dim grid-dim "GRID-DIM")
  (normalize-launch-dim block-dim "BLOCK-DIM")
  (validate-launch-args spec args)
  (%bridge-unavailable spec))

(defmacro with-compiled-kernel ((var spec) &body body)
  "Validate SPEC, then signal that the NVRTC bridge is absent.

This macro keeps the base API deterministic on CPU-only hosts while the
optional bridge system provides the compile-and-cleanup implementation."
  (declare (ignore var body))
  `(progn
     (unless (kernel-spec-p ,spec)
       (error 'dsl-syntax-error :form ,spec :reason "compile spec must be a KERNEL-SPEC"))
     (%bridge-unavailable ,spec)))
