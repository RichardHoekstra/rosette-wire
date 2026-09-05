;;;; register-fragment.lisp --- named lexical register lifetimes.

(in-package #:rosette-gpu-kernel-dsl)

(defun make-register-fragment
    (&key slots (carrier :float) (lifetime :thread)
          (layout :contiguous) offsets)
  "Return a canonical identity for a fixed lexical register fragment.

The identity records schedule intent; REGISTER-FRAGMENT-FORM supplies the
actual expressions and scope.  No alias or invariance theorem is inferred."
  (unless (and (integerp slots) (plusp slots))
    (error 'dsl-syntax-error :form slots
           :reason "register fragment slots must be a positive integer"))
  (unless (member carrier '(:float :int :uint32 :bool :int64) :test #'eq)
    (error 'dsl-syntax-error :form carrier
           :reason "register fragment carrier is not a typed LET carrier"))
  (unless (keywordp lifetime)
    (error 'dsl-syntax-error :form lifetime
           :reason "register fragment lifetime must be a keyword identity"))
  (unless (keywordp layout)
    (error 'dsl-syntax-error :form layout
           :reason "register fragment layout must be a keyword identity"))
  (let ((resolved (or offsets (loop for slot below slots collect slot))))
    (unless (and (listp resolved) (= (length resolved) slots)
                 (every #'integerp resolved))
      (error 'dsl-syntax-error :form resolved
             :reason "register fragment offsets must contain one integer per slot"))
    (list :schema :rosette-register-fragment/v1
          :slots slots :carrier carrier :lifetime lifetime
          :layout layout :offsets resolved)))

(defun %validated-register-fragment (fragment)
  (unless (and (listp fragment)
               (eq (getf fragment :schema) :rosette-register-fragment/v1))
    (error 'dsl-syntax-error :form fragment
           :reason "not an :ROSETTE-REGISTER-FRAGMENT/V1 identity"))
  (let ((canonical
          (make-register-fragment
           :slots (getf fragment :slots)
           :carrier (getf fragment :carrier)
           :lifetime (getf fragment :lifetime)
           :layout (getf fragment :layout)
           :offsets (getf fragment :offsets))))
    (unless (equal fragment canonical)
      (error 'dsl-syntax-error :form fragment
             :reason "register fragment identity is non-canonical"))
    canonical))

(defun register-fragment-bindings (fragment bindings)
  "Validate BINDINGS and return their ordinary typed DSL LET bindings."
  (let* ((canonical (%validated-register-fragment fragment))
         (slots (getf canonical :slots))
         (carrier (getf canonical :carrier)))
    (unless (and (listp bindings) (= (length bindings) slots)
                 (every (lambda (binding)
                          (and (listp binding) (= (length binding) 2)
                               (symbolp (first binding))
                               (not (keywordp (first binding)))))
                        bindings)
                 (= (length (remove-duplicates (mapcar #'first bindings))) slots))
      (error 'dsl-syntax-error :form bindings
             :reason "register fragment requires unique (REGISTER EXPRESSION) pairs"))
    (loop for (register expression) in bindings
          collect `(,register ,carrier ,expression))))

(defun register-fragment-form (fragment bindings &rest body)
  "Wrap BODY in typed lexical BINDINGS governed by FRAGMENT.

BINDINGS is a list of (REGISTER EXPRESSION) pairs.  The explicit caller owns
the proof that each expression is safe for the declared lifetime."
  `(let ,(register-fragment-bindings fragment bindings)
     ,@body))
