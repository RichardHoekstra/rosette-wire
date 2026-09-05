;;;; package.lisp --- small public surface for the quantum tape VM.

(in-package #:cl-user)

(defpackage #:rosette-quantum-vm
  (:use #:cl)
  (:export
   ;; Located refusal, never an implicit approximation.
   #:quantum-vm-obstruction
   #:quantum-vm-obstruction-kind
   #:quantum-vm-obstruction-path
   #:quantum-vm-obstruction-details

   ;; Dense split-complex state projection.  All array readers copy.
   #:quantum-state
   #:quantum-state-p
   #:quantum-state-n-qubits
   #:quantum-state-values
   #:quantum-state-real-parts
   #:quantum-state-imag-parts
   #:quantum-state-norm-squared

   ;; Pauli sums use big-endian/MSB qubit strings over I, X, Y, Z.
   #:pauli-sum
   #:pauli-sum-p
   #:make-pauli-sum
   #:pauli-sum-terms
   #:pauli-sum-n-qubits
   #:pauli-sum-fingerprint
   #:observable-expectation

   ;; Primal tape execution and immutable receipt.
   #:execution-result
   #:execution-result-p
   #:execute-quantum-tape
   #:execution-result-state
   #:execution-result-gate-count
   #:execution-result-parameter-occurrences
   #:execution-result-tape-fingerprint
   #:execution-result-bindings
   #:execution-result-receipt
   #:execution-result-fingerprint

   ;; State-free partial evaluation to the existing qIR ProgramIR carrier.
   #:specialize-quantum-tape

   ;; Three exact first-derivative routes and exact hyper-dual Hessians.
   #:gradient-result
   #:gradient-result-p
   #:value-and-gradient
   #:hyperdual-gradient-hessian
   #:hessian-vector-product
   #:gradient-result-method
   #:gradient-result-value
   #:gradient-result-parameters
   #:gradient-result-gradient
   #:gradient-result-hessian
   #:gradient-result-receipt
   #:gradient-result-fingerprint

   ;; State-level differential maps.
   #:state-jvp
   #:state-vjp

   ;; Validated data-only production seam.
   #:circuit-value-gradient
   #:circuit-grad-main

   ;; Recompute, compare semantic content, and reject altered receipts.
   #:verify-execution-result
   #:verify-gradient-result))
