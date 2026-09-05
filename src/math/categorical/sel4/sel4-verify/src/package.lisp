;;;; package.lisp --- public API for rosette-sel4-verify.

(defpackage #:rosette-sel4-verify
  (:use #:cl)
  (:export
   ;; the system model
   #:system #:make-system #:system-p
   #:system-pds #:system-edges #:system-levels
   #:pd-priority #:add-pd #:add-ppcall
   #:parse-system-file #:parse-system-string
   ;; the two invariants (one shape: does a global potential exist?)
   #:progress-potential        ; topological order of the ppcall graph, or NIL (deadlock)
   #:betti1                     ; homological shadow b_1 (via rosette-chain-complex)
   #:priority-monotone-p        ; Microkit rule: every ppcall edge rises in priority
   #:deadlock-free-p            ; (values bool order b1)
   #:info-flow-secure-p         ; security-level labelling never drops along a flow
   ;; the verdict
   #:verdict #:verdict-p #:verdict-deadlock-free #:verdict-info-flow-secure
   #:verdict-b1 #:verdict-order #:verdict-violations
   #:referee                    ; system -> verdict
   #:report))                   ; printed referee report
