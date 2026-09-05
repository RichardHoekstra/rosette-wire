;;;; package.lisp --- public API for rosette-claim-referee.

(in-package #:cl-user)

(defpackage #:rosette-claim-referee
  (:use #:cl)
  (:import-from #:rosette-scalar-core
                #:approx=
                #:+default-tolerance+)
  (:export
   ;; --- Verdict taxonomy ----
   #:verdict-keywords           ; the canonical (:confirmed :refuted :partial :untestable)
   #:verdict-keyword-p          ; T if a keyword is in the taxonomy
   ;; --- Claim constructors ----
   #:point-claim                ; claimed value +/- tol, measured by a thunk
   #:lower-bound-claim          ; measured >= claimed (">= 10x speedup")
   #:upper-bound-claim          ; measured <= claimed
   #:composite-claim            ; a claim with sub-claims
   #:permutation-control-claim  ; negative control: effect must beat its label-shuffle null
   ;; --- Claim accessors ----
   #:claim-name
   #:claim-kind                 ; :point / :lower-bound / :upper-bound / :composite
   #:claim-claimed              ; the claimed value (nil for composite)
   #:claim-tolerance
   #:claim-subclaims            ; list of sub-claims (composite only)
   ;; --- The referee ----
   #:adjudicate                 ; (claim) -> verdict
   ;; --- Verdict accessors ----
   #:verdict-p
   #:verdict-name
   #:verdict-status             ; one of verdict-keywords
   #:verdict-claimed
   #:verdict-measured
   #:verdict-margin             ; signed: measured - claimed (point/bound)
   #:verdict-ratio              ; measured / claimed (the "10x -> 2x" factor)
   #:verdict-reason             ; human-readable string
   #:verdict-subverdicts        ; list of sub-verdicts (composite only)
   #:verdict-holding-count      ; # of sub-verdicts that are :confirmed
   ;; --- Rendering ----
   #:verdict-row-string         ; one-line rendering of a single verdict
   #:verdict-table-string))     ; multi-line rendering (composite-aware)

(in-package #:rosette-claim-referee)
