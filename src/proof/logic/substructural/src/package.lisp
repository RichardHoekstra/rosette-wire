;;;; rosette-substructural/src/package.lisp --- Public API.

(defpackage #:rosette-substructural
  (:use #:cl)
  (:export
   ;; logics and the structural-rule lattice
   #:*logics*
   #:admissible-structural-rules
   #:logic<=
   #:logic-provability-order
   #:structural-profile
   #:structural-profile-p
   #:make-structural-profile
   #:structural-profile-exchange-p
   #:structural-profile-weakening-p
   #:structural-profile-contraction-p
   #:structural-profile-rules
   #:structural-profile-name
   #:logic-structural-profile
   ;; formulae helpers
   #:atomic-p
   #:formula-size
   ;; derivations
   #:deriv
   #:deriv-p
   #:deriv-rule
   #:deriv-gamma
   #:deriv-goal
   #:deriv-premises
   #:deriv-tags
   #:derivation-struct-tags
   #:derivation-size
   #:derivation-end-sequent
   #:cut-free-p
   ;; the engine
   #:prove
   #:provable-p
   #:provable-logics
   #:legal-in-p
   ;; cut and its admissibility
   #:prove-with-cut
   #:eliminate-cut
   #:*max-depth*))
