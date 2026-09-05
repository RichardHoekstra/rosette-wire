;;;; spec.lisp --- KERNEL-SPEC IR struct + DEFKERNEL macro
;;;;
;;;; KERNEL-SPEC is the intermediate representation: a name, a
;;;; parameter list, a body (DSL s-expression), and a schedule-hints
;;;; alist.  Following Halide (Ragan-Kelley 2013), the body specifies
;;;; the *algorithm* and the schedule-hints alist is reserved for
;;;; *schedule* directives (tile sizes, block dim, vectorization)
;;;; that v0.2+ will eventually consume during lowering.  v0.1.0
;;;; codegen emits a single grid-stride loop and ignores the slot
;;;; beyond storing the alist.
;;;;
;;;; PARAM-DECLS shape:
;;;;
;;;;   (NAME-SYMBOL :type-keyword [:role <:in :out :inout :scalar>])
;;;;
;;;; Type keywords accepted in v0.1.0:
;;;;
;;;;   :int        -> int
;;;;   :float      -> float (scalar)
;;;;   :float*     -> float* (pointer to device array)
;;;;   :int*       -> int*
;;;;   :const-float* -> const float * __restrict__
;;;;
;;;; The DEFKERNEL macro accepts a more compact (NAME :TYPE) flat
;;;; pair-list to keep call sites concise:
;;;;
;;;;   (defkernel vector-add (a :const-float* b :const-float*
;;;;                          c :float* n :int)
;;;;     (for-grid-stride (i n)
;;;;       (setq c[i] (+ a[i] b[i]))))

(in-package #:rosette-gpu-kernel-dsl)

;;; --- Registry --------------------------------------------------------

(defparameter *kernel-registry* (make-hash-table :test #'eq)
  "Hash-table NAME-SYMBOL -> KERNEL-SPEC of every kernel registered by
   DEFKERNEL.  Look-up is via FIND-KERNEL.")

(defun find-kernel (name)
  "Return the registered KERNEL-SPEC for NAME, or NIL if absent."
  (declare (type symbol name))
  (gethash name *kernel-registry*))

(defun list-kernels ()
  "Return a fresh list of every registered kernel name."
  (loop for k being the hash-keys of *kernel-registry* collect k))

;;; --- Struct ----------------------------------------------------------

(defstruct (kernel-spec
            (:constructor %make-kernel-spec
                          (name params body schedule-hints))
            (:copier nil)
            (:predicate kernel-spec-p))
  "Validated kernel IR.

   NAME            : symbol — kernel name (used as C entry point).
   PARAMS          : list of (param-name :type-keyword [:role role])
                     plists, in declaration order.
   BODY            : DSL s-expression that VALID-DSL-FORM-P accepts.
   SCHEDULE-HINTS  : alist — caller-provided hints for v0.2 schedule
                     lowering (e.g. (:block-x . 128) (:tile-m . 32)).
                     v0.1.0 codegen ignores this beyond storage.

   Construct via MAKE-KERNEL-SPEC, never the private %MAKE-KERNEL-SPEC."
  name
  params
  body
  schedule-hints)

;;; --- Param-decl normalisation ---------------------------------------

(defparameter *valid-param-types*
  '(:int :float :float* :int* :const-float* :const-int*
    :int64 :int64* :const-int64*
    :uint8* :const-uint8*)
  "DSL parameter type keywords accepted in v0.1.0.
:int64 / :int64* / :const-int64* admit 64-bit values and payloads.  Integer
value width, loop/address width, and pointer element width are inferred
independently: an i64 index may address four-byte :int* payloads, while a typed
i64 local can widen a composite address without changing an all-i32 launch ABI.
Wide grid/address domains are for non-negative counts and indices.
:uint8* / :const-uint8* are byte (unsigned char) arrays: an indexed load reads
one byte and zero-extends into the i32 value domain -- the quantized-weight
substrate (q8_0/q2_k packed blocks + fp16 scales decoded via HALF-TO-FLOAT).")

(defun %check-param-name (name)
  (unless (and (symbolp name) (not (keywordp name)))
    (error 'dsl-syntax-error
           :form name
           :reason "param name must be a non-keyword symbol"))
  name)

(defun %check-param-type (type)
  (unless (member type *valid-param-types*)
    (error 'dsl-syntax-error
           :form type
           :reason (format nil
                           "param type ~S not in ~S"
                           type *valid-param-types*)))
  type)

(defun %normalise-flat-params (flat)
  "Convert a flat pair-list (a :type b :type ...) into the canonical
   list-of-plists shape.  Used by DEFKERNEL.

   Each pair becomes (NAME :TYPE keyword)."
  (let ((out '()))
    (loop for (name type) on flat by #'cddr
          do (%check-param-name name)
             (%check-param-type type)
             (push (list name :type type) out))
    (nreverse out)))

(defun %normalise-param-list (params)
  "Normalise PARAMS into the canonical (NAME :TYPE T [:ROLE R]) plist
   form.  Accepts either:

     - flat pair list: (a :float* b :int)
     - canonical form: ((a :type :float*) (b :type :int))"
  (cond
    ((null params) nil)
    ((and (consp (first params))
          (>= (length (first params)) 2)
          (symbolp (first (first params))))
     ;; canonical (NAME :type T ...) plist form already.
     (loop for entry in params
           for name = (first entry)
           for plist = (rest entry)
           for type = (getf plist :type)
           do (%check-param-name name)
              (%check-param-type type)
           collect (list name :type type)))
    (t
     (%normalise-flat-params params))))

;;; --- Constructor -----------------------------------------------------

(defun make-kernel-spec (&key name params body schedule-hints)
  "Construct a validated KERNEL-SPEC.  Validates the body against
   VALID-DSL-FORM-P, normalises the param-list shape, and asserts NAME
   is a symbol.  Does **not** register the result; callers that want
   that go through DEFKERNEL.

   Signals DSL-SYNTAX-ERROR if BODY fails grammar or if a param-decl
   is malformed."
  (declare (type symbol name))
  (unless (symbolp name)
    (error 'dsl-syntax-error :form name :reason "kernel name must be a symbol"))
  (let ((normalised-params (%normalise-param-list params))
        ;; Canonicalise the body's symbols into the rosette-gpu-kernel-dsl
        ;; package so the stored IR is package-independent: backends that
        ;; key on symbol identity (EQ/CASE/ECASE) see the same operator
        ;; symbol no matter which package the caller wrote the DSL in.
        ;; Param names are canonicalised too so identifier references in the
        ;; body and the param list share one package (single source of EQ).
        (canonical-body (%canonicalize-dsl-form body)))
    (unless (valid-dsl-form-p canonical-body)
      (error 'dsl-syntax-error :form body
             :reason "kernel body fails VALID-DSL-FORM-P"))
    (%make-kernel-spec name
                       (%canonicalize-param-names normalised-params)
                       canonical-body
                       schedule-hints)))

(defun %canonicalize-param-names (params)
  "Re-intern each param NAME into the rosette-gpu-kernel-dsl package so a body
   identifier reference and its param declaration are the SAME symbol.
   Type/role keywords in the plist are already canonical and untouched."
  (loop for (name . plist) in params
        collect (cons (%canonical-symbol name) plist)))

;;; --- DEFKERNEL macro -------------------------------------------------

(defmacro defkernel (name (&rest param-decls) &body body)
  "Define a GPU kernel and register it in *KERNEL-REGISTRY*.

   Syntax:

     (defkernel NAME (PARAM :TYPE PARAM :TYPE ...)
       BODY-STMT)

   PARAM-DECLS is a flat pair list of (name type-keyword), with the
   types limited to *VALID-PARAM-TYPES*.  BODY may be one or more
   DSL statements; multiple statements are wrapped implicitly in
   PROGN.

   Example:

     (defkernel vector-add (a :const-float* b :const-float*
                            c :float* n :int)
       (for-grid-stride (i n)
         (setq c[i] (+ a[i] b[i]))))

   The macro expands at load-time to a call that builds and registers
   the KERNEL-SPEC; the BODY forms are kept as data, not evaluated."
  (let* ((wrapped-body (cond ((null body) (error 'dsl-syntax-error
                                                  :form nil
                                                  :reason "empty kernel body"))
                             ((null (rest body)) (first body))
                             (t (cons 'progn body)))))
    `(let ((spec (make-kernel-spec :name ',name
                                   :params ',param-decls
                                   :body ',wrapped-body
                                   :schedule-hints nil)))
       (setf (gethash ',name *kernel-registry*) spec)
       ',name)))
