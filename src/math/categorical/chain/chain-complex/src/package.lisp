;;;; package.lisp --- rosette-chain-complex package.

(defpackage #:rosette-chain-complex
  (:use #:cl)
  (:import-from #:rosette-smith-normal-form
                #:integer-homology
                #:hom-free-rank
                #:hom-torsion
                #:hom-betti
                #:format-homology-group)
  (:import-from #:rosette-exact-linear-subquotient
                #:make-linear-subquotient
                #:linear-subquotient-dimension
                #:linear-subquotient-representatives
                #:linear-subquotient-class-coordinates
                #:linear-subquotient-receipt
                #:linear-subquotient-valid-p
                #:descend-linear-map
                #:linear-map-descent-matrix
                #:linear-map-descent-receipt
                #:linear-map-descent-valid-p)
  (:export
   ;; --- exact rational rank / nullity over a field (rank-nullity) ---
   #:field-rank
   #:field-nullity
   ;; --- the chain complex object ---
   #:make-chain-complex
   #:chain-complex
   #:chain-complex-p
   #:chain-complex-boundaries     ; (d_1 d_2 ... d_N)
   #:chain-complex-dims           ; (dim C_0 ... dim C_N)
   #:chain-complex-top
   #:boundary-matrix              ; d_n accessor
   ;; --- the defining law d^2 = 0 ---
   #:d-squared-zero-p             ; d_{n-1} d_n = 0 for all n
   #:d-squared-residual           ; the actual product matrices (witness)
   ;; --- relative complexes C_*(X,A) = C_*(X) / C_*(A) ---
   #:make-relative-chain-complex
   #:relative-chain-complex
   #:relative-chain-complex-p
   #:relative-chain-complex-ambient
   #:relative-chain-complex-subspace-indices
   #:relative-chain-complex-quotient
   #:relative-chain-complex-receipt
   #:relative-chain-complex-valid-p
   #:relative-chain-error
   #:relative-chain-error-dimension
   #:relative-chain-error-row
   #:relative-chain-error-column
   #:relative-chain-error-value
   ;; --- (co)homology ---
   #:betti                        ; b_n over a field (rank-nullity)
   #:betti-numbers                ; (b_0 b_1 ... b_N)
   #:homology                     ; H_n over Z (free rank + torsion), via SNF
   #:cohomology                   ; H^n of the dual cochain complex (transpose d)
   #:torsion-coefficients         ; the t_i > 1 in H_n
   #:dim-h                        ; dim H_n over a field (= betti)
   ;; --- exact rational cohomology, with representatives ---
   #:make-rational-cohomology-space
   #:rational-cohomology-space
   #:rational-cohomology-space-p
   #:rational-cohomology-space-complex
   #:rational-cohomology-space-degree
   #:rational-cohomology-space-dimension
   #:rational-cohomology-space-representatives
   #:rational-cohomology-class-coordinates
   #:rational-cohomology-space-receipt
   #:rational-cohomology-space-valid-p
   ;; --- chain maps and their contravariant action on cohomology ---
   #:make-chain-map
   #:chain-map
   #:chain-map-p
   #:chain-map-source
   #:chain-map-target
   #:chain-map-matrices
   #:chain-map-matrix
   #:chain-map-receipt
   #:chain-map-valid-p
   #:identity-chain-map
   #:compose-chain-maps
   #:induced-cohomology-map
   #:induced-cohomology-map-p
   #:induced-cohomology-map-domain
   #:induced-cohomology-map-codomain
   #:induced-cohomology-map-degree
   #:induced-cohomology-map-matrix
   #:induced-cohomology-map-receipt
   #:induced-cohomology-map-valid-p
   ;; --- the Euler characteristic ---
   #:euler-characteristic         ; chi = sum (-1)^n dim C_n
   #:euler-from-betti             ; sum (-1)^n b_n  (must equal the above)
   ;; --- the cochain complex (dual) ---
   #:cochain-complex              ; the transpose-dual complex
   ;; --- builders: spaces become chain complexes ---
   #:simplicial-complex           ; from a set of simplices (vertex tuples)
   #:graph-complex                ; a graph: C_0 vertices, C_1 edges -> b_0,b_1
   ;; --- instance adapters (the unification) ---
   #:cech-1-cochain-complex       ; a Cech 1-skeleton (V,E,triangles) as a complex
   #:report                       ; a printed homology report (Betti + torsion + chi)
   #:homology-report
   #:homology-report-p
   #:homology-report-betti
   #:homology-report-torsion
   #:homology-report-euler
   ;; --- the CUP PRODUCT: cohomology as a graded ring (GF(2)) ---
   #:simplicial-cochains          ; oriented complex retaining ordered simplices
   #:simplicial-cochains-p
   #:sc-complex                   ; the embedded chain-complex
   #:sc-n-simplices               ; number of k-simplices
   #:sc-simplex-col               ; column of a sorted k-simplex tuple
   #:gf2-betti                    ; dim H^k over GF(2)
   #:cohomology-generators        ; representative k-cocycles, a basis of H^k
   #:cochain-cup                  ; the cochain-level Alexander-Whitney product
   #:cochain-cocycle-p            ; is a k-cochain a cocycle?
   #:cochain-coboundary-p         ; is a k-cochain a coboundary?
   #:cohomology-nonzero-p         ; does a k-cochain represent a nonzero class?
   #:cohomology-cohomologous-p    ; do two k-cochains represent the same class?
   #:cohomology-cup               ; the induced ring product on classes (+ nonzero?)
   #:cohomology-ring              ; the graded GF(2) cohomology ring
   #:cohomology-ring-p
   #:cohomology-ring-cochains
   #:cohomology-ring-dimensions   ; (dim H^0 ... dim H^N) over GF(2)
   #:cohomology-ring-generators
   #:cohomology-ring-h1-cup-form  ; M[i][j] = [g_i cup g_j] nonzero in H^2
   #:cohomology-ring-generators-in-degree
   #:cup-form-nonzero-p))
