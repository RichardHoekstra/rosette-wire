;;;; crossfloor.lisp --- CERTIFY (stage 5b): floor === floor, the deployment moat.
;;;;
;;;; parity.lisp certifies floor === in-tree: a deliverable reproduces the
;;;; witness. This layer certifies the stronger, deployment-native claim: every
;;;; DELIVERY floor computes the same thing as every other. It is the shipping
;;;; analog of the substrate's one-IR-many-silicon-floors oracle -- there the
;;;; gauge is CPU/PTX/WASM/WGSL/Verilog and the check is bit-equivalence of the
;;;; emitted text; here the gauge is :bin/:jit/:oci/:mcp and the check is
;;;; byte-equivalence of what the realised artifacts print. Proving floor===floor
;;;; (not merely floor===in-tree) is what lets a shipment migrate delivery
;;;; formats without re-earning trust -- the moat.
;;;;
;;;; Pure adjudication, no I/O: the CLI realises each floor and captures its
;;;; stdout; these functions only compare the captured strings.

(in-package #:rosette-ship)

(defun floors-agree-p (floor-outputs)
  "FLOOR-OUTPUTS is an alist ((:bin . \"out\") (:jit . \"out\") ...) of a floor
keyword to that floor's captured stdout (or NIL when a floor was not realised).
Return T iff at least one output is present and every present (non-NIL) output
is byte-identical (STRING=). Return NIL when no floor produced output -- there
is nothing to agree on."
  (let ((present (loop for (nil . out) in floor-outputs
                       when out collect out)))
    (and present
         (let ((first (first present)))
           (every (lambda (o) (string= o first)) (rest present)))
         t)))

(defun cross-floor-report (floor-outputs)
  "A plist summary of the cross-floor gauge-check over FLOOR-OUTPUTS (an alist
of floor keyword -> captured stdout). :FLOORS is the list of floor keywords
compared, :AGREE the FLOORS-AGREE-P verdict, :OUTPUTS the raw alist. Report the
residue; the caller decides what a disagreement means."
  (list :floors (mapcar #'car floor-outputs)
        :agree (floors-agree-p floor-outputs)
        :outputs floor-outputs))
