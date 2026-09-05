;;;; package.lisp --- public API for rosette-lisp-codegen.

(defpackage #:rosette-lisp-codegen
  (:use #:cl)
  (:import-from #:rosette-lisp-objects #:mk-fixnum #:fixnum-val)
  (:export
   ;; the bytecode ISA (opcodes) -- shared by the reference and native VMs
   #:+op-halt+ #:+op-pushc+ #:+op-loadl+ #:+op-add+ #:+op-sub+ #:+op-mul+
   #:+op-div+ #:+op-mod+ #:+op-lt+ #:+op-gt+ #:+op-eq+ #:+op-jmp+ #:+op-jz+
   #:+op-call+ #:+op-ret+
   ;; the compiled artefact
   #:program #:program-p #:program-code #:program-entry
   #:program-fn-entry #:program-fn-arity #:program-names
   ;; compiler + reference VM
   #:compile-program         ; (compile-program defs main-expr) -> program
   #:run                     ; (run program) -> integer result
   #:disassemble-program))   ; human-readable listing
