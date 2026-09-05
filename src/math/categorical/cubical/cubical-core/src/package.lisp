;;;; package.lisp --- package definition for rosette-cubical-core.

(defpackage #:rosette-cubical-core
  (:use #:cl)
  (:import-from #:rosette-hott-core
                #:hott-type #:make-hott-type #:hott-type-name #:in-type-p
                #:hott-type-truncation
                #:hott-equivalence #:make-hott-equivalence
                #:hott-equivalence-source #:hott-equivalence-target
                #:hott-equivalence-forward #:hott-equivalence-backward
                #:equiv-forward #:equiv-backward
                #:equiv->path #:univalence-transport
                #:circle-type #:circle-base #:circle-loop)
  (:export
   ;; The interval I as a De Morgan algebra of dimension terms
   #:i0 #:i1 #:idim #:ineg #:imeet #:ijoin
   #:interval-term-p #:eval-interval #:interval-endpoint-p

   ;; Paths as functions I -> A
   #:cpath #:cpath-type #:cpath-function #:make-cpath
   #:path-app #:cpath-i0 #:cpath-i1 #:crefl #:cpath-from #:cpath-to

   ;; Lines of types  A : I -> U  and the Kan operations
   #:type-line #:type-line-tag #:type-line-at #:type-line-at-fn
   #:type-line-transp-fn
   #:make-const-line #:make-glue-line #:make-type-line
   #:transp #:transp-const-id-p
   #:hcomp #:hcomp-cap

   ;; Computational univalence
   #:ua #:ua-beta #:ua-line
   #:bool-type #:bool-not-equivalence
   #:int-type #:int-succ-equivalence

   ;; The 2-cell layer: squares (2-paths), faces, filling
   #:csquare #:csquare-p #:csquare-type #:csquare-function #:make-csquare #:square-app
   #:csquare-face-r0 #:csquare-face-r1 #:csquare-face-s0 #:csquare-face-s1
   #:square-boundary-coherent-p
   #:refl-square #:hrefl-square #:vrefl-square
   #:commuting-square #:path-compose-cubical #:associator-square
   #:hfill #:hfill-lid

   ;; PathP (heterogeneous paths) and the unified Kan comp
   #:cpathp #:cpathp-p #:cpathp-line #:cpathp-function #:make-cpathp
   #:pathp-app #:cpathp-i0 #:cpathp-i1 #:cpath->pathp #:pathp-heterogeneous-p
   #:transp-fill #:comp #:comp-fill
   #:bit-type #:bool-bit-equivalence

   ;; Dependent circle-induction and pi_1(S^1) = Z as an equivalence
   #:helix-family #:circle-section #:circle-section-p #:circle-ind
   #:circle-section-base-value #:circle-section-loop-pathp
   #:circle-section-at-base #:circle-section-loop-transport
   #:circle-encode #:circle-decode
   #:circle-pi1-encode-decode-id-p #:circle-pi1-decode-encode-id-p

   ;; n-truncation, the pi_n functor, and Eckmann-Hilton (pi_n abelian, n>=2)
   #:loop-space-class #:set-truncate-class #:pi-n #:group-class-is-set-p
   #:circle-aspherical-p #:set-higher-homotopy-trivial-p #:sphere-pi-n-stalls-p
   #:interchange-holds-p #:eckmann-hilton-commutative-p #:pi-n-abelian-for-n>=2-p

   ;; The face lattice (cofibrations), systems of partial elements, comp-sys
   #:f-top #:f-bot #:f-eq0 #:f-eq1 #:f-and #:f-or #:face-p #:eval-face
   #:face-contradictory-p
   #:system #:system-p #:system-clauses #:make-system
   #:system-domain-holds-p #:system-value #:system-compatible-p
   #:comp-sys

   ;; Connection squares (the De Morgan structure builds the unit laws)
   #:connection-and-square #:connection-or-square
   #:connection-and-left-unit-p #:connection-or-right-unit-p

   ;; Kan comp for the type-formers: Sigma / Pi / Path
   #:sigma-type #:make-sigma-line
   #:pi-type #:make-pi-line
   #:path-type #:make-path-line

   ;; Contrast with book-HoTT
   #:book-transport-stuck-p

   ;; Toward pi_1(S^1) = Z
   #:winding-of-loop-power #:circle-encode-decode-roundtrip-p

   ;; Glue types and the Glue comp over a cofibration (increment 8)
   #:gtype #:gtype-p #:gtype-phi #:gtype-equiv #:gtype-base #:gtype-htype
   #:glue-value #:glue-value-p #:glue-value-t-part #:glue-value-a-part
   #:make-gtype #:glue-type-at #:glue-total-env
   #:unglue #:glue
   #:glue-unglue-beta-p #:unglue-glue-base-beta-p
   #:glue-line #:glue-line-p #:glue-line-phi
   #:make-glue-type-line #:glue-line-gtype-at #:ua-glue-line
   #:comp-glue #:comp-glue-total-reduces-to-ua-p

   ;; Higher (2-dimensional) hcomp and S^2 as a HIT (increment 9)
   #:hcomp2 #:hcomp2-degenerate-is-cap-p #:hcomp2-lid-boundary-coherent-p
   #:sphere2 #:sphere2-p #:make-sphere2
   #:sphere2-base #:sphere2-surf #:sphere2-type
   #:sphere2-surf-boundary-refl-p
   #:sphere2-rec #:sphere2-rec-p #:sphere2-rec-target
   #:sphere2-rec-base-image #:sphere2-rec-surf-image
   #:sphere2-rec-app-base #:sphere2-rec-app-surf
   #:sphere2-rec-point-beta-p #:sphere2-rec-surf-is-2cell-p
   #:sphere2-omega2-generator #:sphere2-surf-nontrivial-p #:sphere2-not-a-set-p
   #:sphere2-pi2-has-nontrivial-element-p #:sphere2-pi2-is-Z-p
   #:sphere2-pi3-hopf-is-Z-p #:brunerie-pi4-s3-stalled-p

   ;; The suspension HIT, connectivity, and the Freudenthal range (increment 10)
   #:suspension #:suspension-p #:make-suspension
   #:suspension-base-type #:suspension-carrier
   #:suspension-north #:suspension-south #:suspension-merid-fn
   #:suspension-meridian #:suspension-meridian-endpoints-p
   #:susp-rec #:susp-rec-p #:susp-rec-target
   #:susp-rec-north-image #:susp-rec-south-image #:susp-rec-merid-image-fn
   #:susp-rec-app-north #:susp-rec-app-south #:susp-rec-app-merid
   #:susp-rec-point-beta-p
   #:s0-type #:suspension-of-s0-is-circle-p
   #:suspension-connectivity #:sphere-connectivity
   #:sphere-connectivity-by-iterated-suspension-p
   #:freudenthal-iso-p #:freudenthal-surj-p #:sphere-suspension-iso-p
   #:sphere-diagonal-step-justified-p #:sphere-diagonal-cyclic-p
   #:degree-suspension-invariant-p #:degree-surjective-p
   #:pi-sphere-diagonal-is-Z-p
   #:sphere-below-diagonal-trivial-p #:sphere-above-diagonal-stalled-p
   #:pi4-s3-cyclic-by-freudenthal-p

   ;; The Hopf fibration, its long exact sequence, and pi_3(S^2)=Z (increment 11)
   #:fibration #:fibration-p #:make-hopf-fibration
   #:fibration-name #:fibration-fibre #:fibration-total #:fibration-base
   #:les-pi #:les-mid-map-iso-p #:les-segment-exact-p
   #:hopf-derived-pi3-base #:hopf-pi3-s2-is-Z-p
   #:hopf-family-fibre-at-base-p #:hopf-surf-action-ua-computes-p
   #:hopf-les-relates-pi4-s3-s2-p

   ;; The flattening lemma: int helix contractible, sharpening the Hopf wall (inc12)
   #:cover-point #:cover-point-p #:cover-point-fibre
   #:helix-loop-transport #:helix-loop-transport-inv
   #:cover-loop-lift #:cover-loop-lift-computes-p
   #:cover-centre #:cover-retract-to-centre
   #:cover-point-retracts-p #:cover-reachable-from-centre-p
   #:cover-total-space-contractible-p
   #:flattening-lemma-cover-instance-p #:flattening-reduces-hopf-wall-to-hspace-p

   ;; The S^1 h-space: multiplication + the genuine rotation equivalence (inc13)
   #:circle-mult-winding #:circle-mult-unit-left-p #:circle-mult-unit-right-p
   #:circle-mult-associative-on-pi1-p #:circle-mult-commutative-on-pi1-p
   #:circle-rotation-equiv #:circle-rotation-ua-computes-p
   #:circle-rotation-is-equivalence-p #:circle-rotation-unit-is-identity-p
   #:circle-rotation-composes-additively-p
   #:hopf-fibre-automorphism-is-genuine-rotation-p
   #:circle-hspace-pi1-structure-computes-p
   #:circle-hspace-higher-coherence-residual-p

   ;; The h-space coherence: associator/unitor 2-cells, pentagon, triangle (inc14)
   #:circle-bracket-left #:circle-bracket-right
   #:circle-associator-square #:circle-associator-coherent-p
   #:circle-unitor-square #:circle-unitor-coherent-p
   #:circle-five-bracketings #:circle-pentagon-closes-p
   #:circle-triangle-coherent-p
   #:circle-hspace-coherence-computes-p #:hspace-coherence-residual-p

   ;; Where the 2 comes from: the Brunerie number as a cup square (inc15)
   #:cohomology-class #:cohomology-class-p #:make-cohomology-class
   #:cohomology-class-c1 #:cohomology-class-ca #:cohomology-class-cb #:cohomology-class-cab
   #:cohomology-cup #:cohomology-a+b
   #:whitehead-square-hopf-invariant #:classical-brunerie-number
   #:classical-brunerie-number-is-2-p
   #:even-sphere-whitehead-square-hopf-invariant #:even-sphere-hopf-invariant-always-2-p
   #:hopf-invariant-of-eta #:whitehead-square-as-multiple-of-eta
   #:whitehead-square-is-2-eta-p #:brunerie-2-located-and-cubical-stalls-p

   ;; The classical group pi_4(S^3) = Z/2, assembled; cubical route still stalls (inc16)
   #:cp2-class #:cp2-class-p #:make-cp2-class
   #:cp2-class-c0 #:cp2-class-cu #:cp2-class-cuu
   #:cp2-cup #:cp2-sq2-of-u #:eta-stably-nontrivial-p
   #:whitehead-product-suspends-to-zero-p #:pi4-s3-generator-order-divides-2-p
   #:pi4-s3-cyclic-p #:pi4-s3-classical-order
   #:pi4-s3-is-Z-mod-2-classically-p #:pi4-s3-classical-computed-cubical-stalls-p

   ;; The join HIT A*B: first piece of the cubical pi_4(S^3) normalisation (inc17)
   #:cjoin #:cjoin-p #:make-cjoin
   #:cjoin-base-a #:cjoin-base-b #:cjoin-carrier
   #:cjoin-inl #:cjoin-inr #:cjoin-push #:cjoin-push-endpoints-p
   #:cjoin-rec #:cjoin-rec-p #:cjoin-rec-target
   #:cjoin-rec-app-inl #:cjoin-rec-app-inr #:cjoin-rec-app-push #:cjoin-rec-point-beta-p
   #:unit-type #:cone-of #:cone-apex #:cone-contracts-point-p #:cone-contractible-p
   #:cjoin-connectivity #:sphere-join-dimension #:sphere-join-connectivity-matches-p
   #:s1-join-s1-is-s3-connectivity-p #:join-s1-s1-typed-equiv-to-s3-stalls-p

   ;; The Hopf construction: eta : S^3 -> S^2 as a cubical term (inc18)
   #:hopf-construction-map #:hopf-construction-well-formed-p #:hopf-construction-point-beta-p
   #:circle-mult-first-degree #:circle-mult-second-degree
   #:circle-hopf-construction-bidegree #:circle-hopf-invariant-via-bidegree
   #:cubical-hopf-map-invariant-is-1-p
   #:circle-point-type #:circle-mult-on-points #:cubical-hopf-map-on-circle
   #:cubical-hopf-map-well-formed-p #:cubical-hopf-construction-residual-p

   ;; The Moore space: pi_1(M(Z/2,1))=Z/2 computed cubically (inc19)
   #:moore-attaching-degree #:moore-sheet->z2 #:moore-cover-transport
   #:moore-encode #:moore-decode #:moore-encode-decode-id-p #:moore-encode-is-mod-2-p
   #:moore-loop-order #:moore-pi1-is-z2-p #:moore-z2-order-is-attaching-degree-p
   #:moore-is-brunerie-shape-one-dim-p

   ;; The set-truncation || - ||_0 as an operation, grounding pi_n=||Omega^n||_0 (inc20)
   #:trunc0 #:trunc0-p #:set-truncate #:trunc0-source-name
   #:trunc0-incl #:trunc0-equal-p #:trunc0-rec
   #:pi0-circle-is-point-p #:pi0-s0-is-two-points-p
   #:omega-s1-truncation #:pi1-s1-via-truncation-is-Z-p
   #:omega-moore-truncation #:pi1-moore-via-truncation-is-Z2-p
   #:pi4-s3-via-truncation-stalls-p #:truncation-grounds-computing-pi-n-p

   ;; The uniform engine: pi_1(M(Z/k,1))=Z/k for all k via order-k monodromy (inc21)
   #:zk-type #:cyclic-equiv #:cyclic-equiv-has-order-k-p
   #:cyclic-encode #:cyclic-decode #:cyclic-encode-is-mod-k-p #:cyclic-loop-order
   #:cyclic-pi1-is-Zk-p #:moore-Zk-for-all-k-p #:brunerie-2-is-k2-instance-p

   ;; A computing encode at pi_4: pi_4(S^3) -> Z/2 = (Hopf invariant) mod 2 (inc22)
   #:pi3-s2-hopf-invariant #:pi4-s3-suspension-kernel-generator
   #:pi4-s3-encode #:pi4-s3-encode-eta3-is-1-p #:pi4-s3-encode-2eta3-is-0-p
   #:pi4-s3-encode-computes-p #:pi4-s3-encode-iso-given-ehp-p
   #:pi4-s3-cubical-encode-residual-p

   ;; The encode/decode equivalence at pi_4, and the honest ceiling (inc23)
   #:pi4-s3-decode #:pi4-s3-encode-decode-id-p #:pi4-s3-order-2-relation-holds-p
   #:pi4-s3-decode-encode-id-p #:pi4-s3-is-Z2-encode-decode-p
   #:pi4-s3-computed-at-presentation-level-p
   #:pi4-s3-loop-transport-normalisation-is-the-ceiling-p))
