;;;; codegen-expr.lisp --- CUDA-C declaration and expression emission.

(in-package #:rosette-gpu-kernel-dsl)

(defun %symbol-c-name (sym)
  "Map SYM to a C-safe identifier name."
  (substitute #\_ #\- (string-downcase (symbol-name sym))))

(defun kernel-c-name (spec-or-name)
  "Map a kernel spec or symbol name to a C-safe identifier.  Lowercase,
   hyphens become underscores."
  (let ((sym (etypecase spec-or-name
               (kernel-spec (kernel-spec-name spec-or-name))
               (symbol spec-or-name))))
    (%symbol-c-name sym)))

(defun param-c-type (type-keyword)
  "Map a DSL parameter type keyword to a CUDA-C type string."
  (case type-keyword
    (:int           "int")
    (:int64         "long long")
    (:float         "float")
    (:float*        "float * __restrict__")
    (:int*          "int * __restrict__")
    (:int64*        "long long * __restrict__")
    (:const-float*  "const float * __restrict__")
    (:const-int*    "const int * __restrict__")
    (:const-int64*  "const long long * __restrict__")
    (t (error 'dsl-syntax-error
              :form type-keyword
              :reason (format nil "unsupported param type ~S in v0.1.0"
                              type-keyword)))))

(defun %sym-c (sym)
  "Lower-case SYM into a CUDA-C identifier."
  (%symbol-c-name sym))

(defun %indent (level)
  (make-string (* 2 level) :initial-element #\Space))

(defmacro %with-indent ((indent level &optional body-level) &body body)
  "Bind INDENT for LEVEL, optionally BODY-LEVEL for nested statements."
  `(let ((,indent (%indent ,level))
         ,@(when body-level
             `((,body-level (1+ ,level)))))
     ,@body))

(defun %emit-separated (stream items separator emit-item)
  "Emit ITEMS to STREAM with SEPARATOR between elements."
  (loop for item in items
        for first-iter = t then nil
        do (unless first-iter
             (format stream "~A" separator))
           (funcall emit-item stream item)))

(defun emit-cuda-decl (params &key (name "kernel"))
  "Emit the extern \"C\" __global__ void NAME(...) signature line.

   PARAMS is a list of (NAME :TYPE T) plists in the canonical form.
   Returns a single CUDA-C string that ends with a closing paren but
   no trailing semicolon (the caller appends `{` for the body)."
  (with-output-to-string (s)
    (format s "extern \"C\" __global__ void ~A(" name)
    (when params
      (%emit-separated
       s params ", "
       (lambda (stream entry)
         (let ((pname (first entry))
               (ptype (getf (rest entry) :type)))
           (format stream "~A ~A"
                   (param-c-type ptype)
                   (%sym-c pname))))))
    (format s ")")))

(defun %lookup-symbol-name (sym entries)
  "Return the mapped string for SYM in ENTRIES using DSL symbol matching."
  (cdr (find-if (lambda (entry)
                  (%sym-name= sym (car entry)))
                entries)))

(defun %op-c-string (sym)
  "Map a DSL operator symbol to its CUDA-C printable form."
        (or (%lookup-symbol-name
       sym
       '((+ . "+")
         (- . "-")
         (* . "*")
         (/ . "/")
         (% . "%")
         (< . "<")
         (> . ">")
         (<= . "<=")
         (>= . ">=")
         (= . "==")
         (/= . "!=")))
      (error 'dsl-syntax-error
             :form sym :reason "operator not in CUDA-C catalog")))

(defun %builtin-c-string (sym)
  "Map a CUDA built-in DSL symbol to its CUDA-C source spelling."
  (or (%lookup-symbol-name
       sym
       '((thread-idx-x . "threadIdx.x")
         (thread-idx-y . "threadIdx.y")
         (thread-idx-z . "threadIdx.z")
         (block-idx-x . "blockIdx.x")
         (block-idx-y . "blockIdx.y")
         (block-idx-z . "blockIdx.z")
         (block-dim-x . "blockDim.x")
         (block-dim-y . "blockDim.y")
         (block-dim-z . "blockDim.z")
         (grid-dim-x . "gridDim.x")
         (grid-dim-y . "gridDim.y")
         (grid-dim-z . "gridDim.z")))
      (error 'dsl-syntax-error
             :form sym :reason "unknown CUDA built-in")))

(defun %emit-indexed-symbol (sym)
  "Emit indexed DSL symbol SYM as BASE[INDEX]."
  (multiple-value-bind (base idx) (%decode-indexed-symbol sym)
    (format nil "~A[~A]" base (emit-cuda-expr idx))))

(defun %emit-cuda-expr-item (stream arg)
  "Emit ARG as a CUDA expression to STREAM."
  (format stream "~A" (emit-cuda-expr arg)))

(defun emit-cuda-expr (form)
  "Recursively emit FORM as a balanced CUDA-C expression string.

   Top-level expressions are not parenthesised; nested operator
   sub-expressions are."
  (cond
    ((integerp form) (format nil "~D" form))
    ((floatp form) (format nil "~Af" (coerce form 'single-float)))
    ((null form) (error 'dsl-syntax-error :form nil :reason "NIL not allowed in expr"))
    ((eql form t) (error 'dsl-syntax-error :form t :reason "T not allowed in expr"))
    ((keywordp form)
     (error 'dsl-syntax-error :form form :reason "keyword not allowed in expr"))
    ((and (symbolp form) (%indexed-symbol-p form))
     (%emit-indexed-symbol form))
    ((symbolp form) (%sym-c form))
    ((not (consp form))
     (error 'dsl-syntax-error :form form :reason "unrecognised expression atom"))
    (t
     (let ((head (first form)))
       (cond
         ((member head *dsl-cuda-builtins* :test #'%sym-name=)
          (%builtin-c-string head))
         ((member head *dsl-arithmetic-operators* :test #'%sym-name=)
          (let ((op (%op-c-string head))
                (args (rest form)))
            (cond
              ((and (%sym-name= head '-) (= 1 (length args)))
               (format nil "(-~A)" (emit-cuda-expr (first args))))
              (t
               (with-output-to-string (s)
                 (format s "(")
                 (%emit-separated
                  s args (format nil " ~A " op)
                  #'%emit-cuda-expr-item)
                 (format s ")"))))))
         ((member head *dsl-comparison-operators* :test #'%sym-name=)
          (format nil "(~A ~A ~A)"
                  (emit-cuda-expr (second form))
                  (%op-c-string head)
                  (emit-cuda-expr (third form))))
         ((%sym-name= head 'shfl-down)
          (format nil "__shfl_down_sync(0xffffffffu, ~A, ~A, 32)"
                  (emit-cuda-expr (second form))
                  (emit-cuda-expr (third form))))
         ((%sym-name= head 'shfl-index)
          (format nil "__shfl_sync(0xffffffffu, ~A, ~A, 32)"
                  (emit-cuda-expr (second form))
                  (emit-cuda-expr (third form))))
         ((%sym-name= head 'call)
          (with-output-to-string (s)
            (format s "~A(" (%sym-c (second form)))
            (%emit-separated
             s (cddr form) ", "
             #'%emit-cuda-expr-item)
            (format s ")")))
         (t (error 'dsl-syntax-error
                   :form form
                   :reason "expression head not in DSL catalog")))))))

(defun %emit-lval (lval)
  "Emit a CUDA-C l-value: a plain identifier or an a[i] indexed form."
  (cond
    ((symbolp lval)
     (cond
       ((%indexed-symbol-p lval)
        (%emit-indexed-symbol lval))
       (t (%sym-c lval))))
    (t (error 'dsl-syntax-error :form lval
              :reason "l-value must be a symbol or indexed symbol"))))
