;;;; package.lisp --- Public API surface of rosette-gpu-kernel-dsl
;;;;
;;;; Discipline (mirrors rosette-gpu-bridge):
;;;;   - All exported names are also reachable via the :rosette-kdsl
;;;;     nickname.
;;;;   - Codegen (VALID-DSL-FORM-P, EMIT-CUDA-DECL, EMIT-CUDA-STMT,
;;;;     EMIT-CUDA-EXPR, COMPILE-KERNEL-SPEC) is pure string work — it
;;;;     has no GPU dependency and runs on CPU-only hosts.
;;;;   - LAUNCH-KERNEL and WITH-COMPILED-KERNEL are provided by the
;;;;     base system with validation and a typed unavailable condition.
;;;;     The optional rosette-gpu-kernel-dsl/bridge system replaces them with
;;;;     NVRTC-backed implementations.

(in-package #:cl-user)

(defpackage #:rosette-gpu-kernel-dsl
  (:nicknames #:rosette-kdsl)
  (:use #:cl)
  (:export
   ;; --- Conditions ---------------------------------------------------
   #:dsl-syntax-error
   #:dsl-syntax-error-form
   #:dsl-syntax-error-reason
   #:gpu-kernel-bridge-unavailable
   #:gpu-kernel-bridge-unavailable-spec
   #:gpu-kernel-bridge-unavailable-remedy

   ;; --- IR struct + registry ----------------------------------------
   #:kernel-spec
   #:kernel-spec-p
   #:kernel-spec-name
   #:kernel-spec-params
   #:kernel-spec-body
   #:kernel-spec-schedule-hints
   #:make-kernel-spec
   #:*kernel-registry*
   #:find-kernel
   #:list-kernels

   ;; --- Schedule form composition ----------------------------------
   #:make-warp-sum-tree
   #:warp-sum-tree-form
   #:make-block-segmented-sum-tree
   #:block-segmented-sum-tree-declarations
   #:block-segmented-sum-tree-form
   #:make-register-fragment
   #:register-fragment-bindings
   #:register-fragment-form
   #:make-online-softmax-moment
   #:online-softmax-moment-step-form
   #:online-softmax-moment-finish-form
   #:online-softmax-moment-coefficients-form
   #:online-softmax-moment-distributed-update-form
   #:make-memory-schedule
   #:memory-schedule-traffic
   #:verify-memory-schedule-traffic

   ;; --- defkernel macro ---------------------------------------------
   #:defkernel

   ;; --- Validation --------------------------------------------------
   #:valid-dsl-form-p
   #:valid-dsl-stmt-p
   #:valid-dsl-expr-p
   #:dsl-operator-p

   ;; --- Codegen -----------------------------------------------------
   #:emit-cuda-decl
   #:emit-cuda-stmt
   #:emit-cuda-expr
   #:emit-cuda-source
   #:compile-kernel-spec
   #:kernel-c-name
   #:param-c-type

   ;; --- Launch ------------------------------------------------------
   #:normalize-launch-dim
   #:validate-launch-args
   #:launch-kernel
   #:with-compiled-kernel

   ;; --- Bundled example kernels -------------------------------------
   #:vector-add
   #:saxpy
   #:vector-add-spec
   #:saxpy-spec))
