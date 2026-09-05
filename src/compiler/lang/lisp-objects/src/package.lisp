;;;; package.lisp --- public API for rosette-lisp-objects.

(defpackage #:rosette-lisp-objects
  (:use #:cl)
  (:export
   ;; tag constants + the singletons
   #:+tag-fixnum+ #:+tag-cons+ #:+tag-char+ #:+tag-singleton+ #:+tag-closure+
   #:+nil+ #:+true+
   ;; tag introspection
   #:tag-of                  ; low 3 bits of a word
   #:objtype                 ; keyword: :fixnum :cons :char :singleton :closure
   #:pointerp                ; does this word point into the heap (cons/closure)?
   ;; immediates
   #:mk-fixnum #:fixnum-val #:fixnump
   #:mk-char   #:char-code-of #:charp
   #:mk-singleton #:singletonp #:nilp #:truep
   ;; the heap
   #:make-heap #:heap #:heap-p #:heap-word-count
   ;; cons cells
   #:heap-cons #:consp* #:obj-car #:obj-cdr #:obj-set-car #:obj-set-cdr
   ;; flat closures: [code-id, ncaps, cap0 ...]
   #:heap-closure #:closurep #:closure-code #:closure-ncaps #:closure-cap
   ;; precise garbage collection
   #:heap-trace              ; -> hash-table of live object base indices
   #:heap-live-count         ; # live objects reachable from roots
   #:heap-gc))               ; copying compaction -> (values new-heap forwarded-roots)
