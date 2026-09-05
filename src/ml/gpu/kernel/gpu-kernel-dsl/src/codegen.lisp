;;;; codegen.lisp --- DSL -> CUDA-C source emission
;;;;
;;;; A small recursive emitter.  Three layers:
;;;;
;;;;   EMIT-CUDA-DECL  — produces the extern "C" __global__ void NAME(...)
;;;;                     signature line from KERNEL-SPEC params.
;;;;   EMIT-CUDA-STMT  — emits a statement and the semicolon (or block).
;;;;   EMIT-CUDA-EXPR  — emits a balanced parenthesised expression.
;;;;
;;;; COMPILE-KERNEL-SPEC concatenates header comment + decl + open
;;;; brace + body + close brace.  No optimisation, no register
;;;; allocation, no common-subexpression elimination — NVRTC handles
;;;; that downstream (NVRTC delegates to LLVM/PTXAS).
;;;;
;;;; Discipline:
;;;;
;;;;   * Indented two spaces per nesting level for human-readability.
;;;;   * For-grid-stride loops emit the canonical CUDA idiom (CUDA C++
;;;;     Programming Guide §B.3): one thread iterates from
;;;;     blockIdx.x*blockDim.x + threadIdx.x by gridDim.x*blockDim.x.
;;;;   * For-range emits a local counted int loop.  It is intentionally
;;;;     boring and exists for tiny reductions inside a grid-stride body.
;;;;   * Floats use the f suffix on every literal so NVRTC does not
;;;;     promote to double silently.
;;;;
;;;; Implemented structured statements include if/else, cond, case, and
;;;; CUDA atomicAdd/atomicCAS emission.  GPU-only runtime support remains
;;;; in rosette-gpu-bridge.

(in-package #:rosette-gpu-kernel-dsl)

;;; --- emit-cuda-stmt --------------------------------------------------

(defvar *cuda-type-environment* nil
  "Dynamically bound internal type map used by the CUDA-C statement emitter.")

(defvar *cuda-shuffle-analysis* nil
  "Lexical per-form shuffle receipt for the CUDA-C kernel being emitted.")

(defun %cuda-expression-type (form)
  (if *cuda-type-environment*
      (infer-dsl-type form *cuda-type-environment*)
      :i32))

(defun %cuda-type-environment (analysis)
  ;; Non-collective CUDA details still use the legacy map.  Warp policy itself
  ;; was already proved against lexical source-form identity in ANALYSIS.
  (kernel-shuffle-analysis-legacy-type-environment analysis))

(defun %emit-cuda-stmt-sequence (forms stream level)
  (dolist (sub forms)
    (princ (emit-cuda-stmt sub :level level) stream)))

(defun %emit-close-brace (stream level)
  "Emit a closing brace at indentation LEVEL."
  (format stream "~A}~%" (%indent level)))

(defun %emit-loop-block (stream level body-forms emit-header)
  (%with-indent (indent level body-level)
    (funcall emit-header stream indent)
    (%emit-cuda-stmt-sequence body-forms stream body-level)
    (%emit-close-brace stream level)))

(defun %emit-expr-stmt (form level)
  "Emit FORM as an expression statement at indentation LEVEL."
  (format nil "~A~A;~%" (%indent level) (emit-cuda-expr form)))

(defun emit-cuda-stmt (form &key (level 1))
  "Recursively emit FORM as a CUDA-C statement string at LEVEL nesting.

   Each emitted statement ends in a newline; nested blocks indent +1."
  (cond
    ((not (consp form))
     (%emit-expr-stmt form level))
    (t
     (let ((head (first form)))
       (cond
         ;; (setq LVAL EXPR)
         ((%sym-name= head 'setq)
          (%with-indent (indent level)
            (format nil "~A~A = ~A;~%"
                    indent
                    (%emit-lval (second form))
                    (emit-cuda-expr (third form)))))
         ;; (for-grid-stride (VAR LIMIT-EXPR) STMT ...)
         ((%sym-name= head 'for-grid-stride)
          (let* ((header (second form))
                 (var (first header))
                 (var-c (%sym-c var))
                 (limit-c (emit-cuda-expr (second header)))
                 (wide-p (eq (%cuda-expression-type (second header)) :i64))
                 (loop-type (if wide-p "long long" "int")))
            (with-output-to-string (s)
              (%emit-loop-block
               s level (cddr form)
               (lambda (stream indent)
                 (format stream "~A// grid-stride loop (CUDA C++ Programming Guide §B.3)~%"
                         indent)
                 (if wide-p
                     (format stream "~Afor (~A ~A = (long long)blockIdx.x * (long long)blockDim.x + (long long)threadIdx.x;~%"
                             indent loop-type var-c)
                     (format stream "~Afor (~A ~A = blockIdx.x * blockDim.x + threadIdx.x;~%"
                             indent loop-type var-c))
                 (format stream "~A     ~A < ~A;~%"
                         indent var-c limit-c)
                 (if wide-p
                     (format stream "~A     ~A += (long long)blockDim.x * (long long)gridDim.x) {~%"
                             indent var-c)
                     (format stream "~A     ~A += blockDim.x * gridDim.x) {~%"
                             indent var-c)))))))
         ;; (for-range (VAR LIMIT-EXPR) STMT ...)
         ((%sym-name= head 'for-range)
          (let* ((header (second form))
                 (var (first header))
                 (var-c (%sym-c var))
                 (limit-c (emit-cuda-expr (second header)))
                 (loop-type (if (eq (%cuda-expression-type (second header)) :i64)
                                "long long" "int")))
            (with-output-to-string (s)
              (%emit-loop-block
               s level (cddr form)
               (lambda (stream indent)
                 (format stream "~Afor (~A ~A = 0; ~A < ~A; ++~A) {~%"
                         indent loop-type var-c var-c limit-c var-c))))))
         ;; (while CONDITION STMT ...) -> a regular CUDA pre-test loop.
         ;; The validator, CPU/LLVM interpreters, and NVPTX lowering already
         ;; admit WHILE; keeping it in the CUDA gauge prevents a kernel from
         ;; becoming backend-dependent merely because a reduction uses a
         ;; dynamic trip count.
         ((%sym-name= head 'while)
          (with-output-to-string (s)
            (%with-indent (indent level body-level)
              (format s "~Awhile (~A) {~%"
                      indent (emit-cuda-expr (second form)))
              (%emit-cuda-stmt-sequence (cddr form) s body-level)
              (%emit-close-brace s level))))
         ;; (progn STMT ...)
         ((%sym-name= head 'progn)
          (with-output-to-string (s)
            (%emit-cuda-stmt-sequence (rest form) s level)))
         ;; (let ((VAR EXPR) ...) STMT ...)
         ;; (let ((VAR :TYPE EXPR) ...) STMT ...)  -- typed form, since 0.2.0
         ((%sym-name= head 'let)
          (with-output-to-string (s)
            (%with-indent (indent level body-level)
              (format s "~A{~%" indent)
              (let ((body-indent (%indent body-level)))
                (dolist (b (second form))
                  (let* ((typed-p (= 3 (length b)))
                         (var (first b))
                         (typ (if typed-p (second b) :float))
                         (expr (if typed-p (third b) (second b)))
                         (type-c (ecase typ
                                   (:float  "float")
                                   (:int    "int")
                                   (:uint32 "unsigned int")
                                   (:int64  "long long")
                                   (:bool   "bool")))
                         (label (if typed-p
                                    (format nil "let-binding (~A)" typ)
                                    "let-binding (auto-typed float)")))
                    (format s "~A// ~A~%" body-indent label)
                    (format s "~A~A ~A = ~A;~%"
                            body-indent type-c (%sym-c var)
                            (emit-cuda-expr expr)))))
              (%emit-cuda-stmt-sequence (cddr form) s body-level)
              (%emit-close-brace s level))))
         ;; (if EXPR STMT [STMT])
         ((%sym-name= head 'if)
          (with-output-to-string (s)
            (%with-indent (indent level body-level)
              (format s "~Aif (~A) {~%"
                      indent
                      (emit-cuda-expr (second form)))
              (princ (emit-cuda-stmt (third form) :level body-level) s)
              (cond
                ((= 4 (length form))
                 (format s "~A} else {~%" indent)
                 (princ (emit-cuda-stmt (fourth form) :level body-level) s)
                 (%emit-close-brace s level))
                (t (%emit-close-brace s level))))))
         ;; (cond (TEST STMT...) ... [(t STMT...)])
         ((%sym-name= head 'cond)
          (with-output-to-string (s)
            (%with-indent (indent level body-level)
              (loop for clause in (rest form)
                    for first-clause = t then nil
                    for test = (first clause)
                    do (cond
                         ((eql test t)
                          (format s "~Aelse {~%" indent))
                         (first-clause
                          (format s "~Aif (~A) {~%"
                                  indent (emit-cuda-expr test)))
                         (t
                          (format s "~Aelse if (~A) {~%"
                                  indent (emit-cuda-expr test))))
                       (%emit-cuda-stmt-sequence (rest clause) s body-level)
                       (%emit-close-brace s level)))))
         ;; (case EXPR (KEY STMT...) ... [(otherwise STMT...)])
         ((%sym-name= head 'case)
          (with-output-to-string (s)
            (%with-indent (indent level body-level)
              (let* ((case-level body-level)
                     (case-indent (%indent case-level))
                     (case-body-level (1+ case-level))
                     (case-body-indent (%indent case-body-level)))
                (format s "~Aswitch (~A) {~%" indent (emit-cuda-expr (second form)))
                (dolist (clause (cddr form))
                  (let ((key (first clause)))
                    (if (member key '(otherwise t) :test #'%sym-name=)
                        (format s "~Adefault:~%" case-indent)
                        (format s "~Acase ~A:~%" case-indent
                                (if (symbolp key) (%sym-c key) key)))
                    (%emit-cuda-stmt-sequence (rest clause) s case-body-level)
                    (format s "~Abreak;~%" case-body-indent)))
                (%emit-close-brace s level)))))
         ((%sym-name= head 'atomic-add)
          (%with-indent (indent level)
            (format nil "~AatomicAdd(&~A, ~A);~%"
                    indent
                    (%emit-lval (second form))
                    (emit-cuda-expr (third form)))))
         ((%sym-name= head 'atomic-cas)
          (%with-indent (indent level)
            (format nil "~AatomicCAS(&~A, ~A, ~A);~%"
                    indent
                    (%emit-lval (second form))
                    (emit-cuda-expr (third form))
                    (emit-cuda-expr (fourth form)))))
         ;; (shared-array NAME :TYPE SIZE) -> __shared__ <ctype> name[SIZE];
         ;; SIZE must be a compile-time integer literal (CUDA static shared mem).
         ;; Enables block-cooperative kernels (e.g. a staged radix FFT).
         ((%sym-name= head 'shared-array)
          (%with-indent (indent level)
            (format nil "~A__shared__ ~A ~A[~A];~%"
                    indent
                    (ecase (third form) (:float "float") (:int "int")
                                        (:uint32 "unsigned int") (:double "double"))
                    (%sym-c (second form))
                    (the integer (fourth form)))))
         ;; (sync) -> __syncthreads();  block-wide barrier.
         ((%sym-name= head 'sync)
          (%with-indent (indent level)
            (format nil "~A__syncthreads();~%" indent)))
         ;; Otherwise: expression-statement.
         (t (%emit-expr-stmt form level)))))))

;;; --- emit-cuda-source / compile-kernel-spec --------------------------

(defun emit-cuda-source (spec)
  "Emit the full CUDA-C source string for SPEC: header comment, decl,
   body, closing brace.  Pure; no GPU dependency.

   The first line is a /* ... */ comment with the kernel name and the
   declared parameter list, useful for debugging compile errors that
   NVRTC reports against a temporary file name."
  (declare (type kernel-spec spec))
  (let* ((*cuda-shuffle-analysis* (validate-kernel-shuffle-types spec))
         (cname (kernel-c-name spec))
         (decl (emit-cuda-decl (kernel-spec-params spec) :name cname))
         (*cuda-type-environment*
           (%cuda-type-environment *cuda-shuffle-analysis*)))
    (with-output-to-string (s)
      (format s "/* rosette-gpu-kernel-dsl emit: ~A */~%" cname)
      (format s "/* params: ~A */~%"
              (or (kernel-spec-params spec) "<none>"))
      (format s "~A {~%" decl)
      (princ (emit-cuda-stmt (kernel-spec-body spec) :level 1) s)
      (format s "}~%"))))

(defun compile-kernel-spec (spec)
  "End-to-end pure-codegen entry point.  Returns the CUDA-C source
   string for SPEC.  Does **not** invoke NVRTC; that lives in
   LAUNCH-KERNEL / WITH-COMPILED-KERNEL.  Pure on a CPU-only host.

   Errors as DSL-SYNTAX-ERROR if SPEC's body fails the grammar
   (this is normally caught earlier at MAKE-KERNEL-SPEC time)."
  (declare (type kernel-spec spec))
  (unless (valid-dsl-form-p (kernel-spec-body spec))
    (error 'dsl-syntax-error
           :form (kernel-spec-body spec)
           :reason "kernel body fails VALID-DSL-FORM-P at compile time"))
  (emit-cuda-source spec))
