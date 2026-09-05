;;;; package.lisp --- deliberately small public surface.

(in-package #:cl-user)

(defpackage #:rosette-quantum-ir
  (:use #:cl)
  (:export
   ;; The carrier remains rosette-program-ir:PROGRAM.  These functions admit only
   ;; the quantum dialect projected from that shared carrier.
   #:make-quantum-program
   #:quantum-program-p
   #:program-form
   #:check-program
   #:program-valid-p
   #:certify-program
   #:verify-certified-program
   #:certified-program-resource-certificates
   #:elaborate-legacy-qprogram

   ;; Deterministic, immutable semantic tape view over an admitted ProgramIR.
   #:lower-quantum-tape
   #:quantum-tape
   #:quantum-tape-p
   #:quantum-tape-entry
   #:quantum-tape-parameters
   #:quantum-tape-result-types
   #:quantum-tape-operations
   #:quantum-tape-subroutines
   #:quantum-tape-signatures
   #:quantum-tape-fingerprint
   #:quantum-tape-op
   #:quantum-tape-op-p
   #:quantum-tape-op-opcode
   #:quantum-tape-op-path
   #:quantum-tape-op-operands
   #:quantum-tape-op-blocks

   ;; Lowering refuses invalid source and ambiguous/missing entries with a
   ;; typed obstruction rather than a generic implementation error.
   #:quantum-tape-compilation-obstruction
   #:quantum-tape-obstruction-kind
   #:quantum-tape-obstruction-diagnostics
   #:quantum-tape-obstruction-details

   ;; Non-signalling admission report.
   #:program-report
   #:program-report-p
   #:program-report-ok-p
   #:program-report-kind
   #:program-report-definitions
   #:program-report-programs
   #:program-report-resources
   #:program-report-errors

   ;; Per-value proof-carrying structural assignments.
   #:resource-certificate
   #:resource-certificate-p
   #:resource-certificate-name
   #:resource-certificate-type
   #:resource-certificate-profile
   #:resource-certificate-structural-rules
   #:resource-certificate-defined-at
   #:resource-certificate-transition
   #:resource-certificate-derivations
   #:verify-resource-certificate

   ;; Stable, machine-readable diagnostics.
   #:diagnostic
   #:diagnostic-p
   #:diagnostic-kind
   #:diagnostic-path
   #:diagnostic-message))
