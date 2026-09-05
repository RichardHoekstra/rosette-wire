;;;; package.lisp --- rosette-smith-normal-form package definition.

(defpackage #:rosette-smith-normal-form
  (:use #:cl)
  (:nicknames #:rosette-snf)
  (:export
   ;; --- exact integer Smith normal form ---
   #:smith-normal-form          ; full D = U A V with transforms
   #:invariant-factors          ; just the d_1 | d_2 | ... | d_r list
   #:snf-d                      ; accessors on the result struct
   #:snf-u
   #:snf-v
   #:snf-rank
   #:snf-invariant-factors
   #:snf-p
   ;; --- matrix helpers (exact integer) ---
   #:mat-mul
   #:mat-det
   #:unimodular-p
   #:diagonal-p
   #:divisibility-chain-p
   ;; --- integer homology with torsion ---
   #:integer-homology           ; H_n from a list of boundary matrices
   #:homology-group             ; one H_n: (free-rank . torsion-list)
   #:hom-free-rank
   #:hom-torsion
   #:hom-betti
   #:format-homology-group))
