;;;; package.lisp --- rosette-hyper-dual package definition.
;;;;
;;;; Hyper-dual numbers (Fike & Alonso 2011): exact 1st + 2nd derivatives
;;;; in one forward pass.  jet3 extends to 3rd-order univariate jets.

(defpackage #:rosette-hyper-dual
  (:use #:cl)
  (:nicknames #:hd)
  (:import-from #:rosette-scalar-core
                #:as-f64)
  (:import-from #:rosette-probability-core
                #:normal-pdf
                #:normal-cdf
                #:inverse-normal-cdf
                #:black-scholes-d1-drift-term
                #:inverse-gaussian-pdf
                #:inverse-gaussian-cdf
                #:inverse-gaussian-quantile)
  (:export
   ;; --- hyper-dual struct + constructors ----
   #:hyper-dual
   #:hyper-dual-p
   #:make-hd                  ; canonical constructor (real e1 e2 e12)
   #:hd-make                  ; alias kept from upstream
   #:hd-from-real
   #:hd-deriv-seed

   ;; --- accessors ----
   #:hd-value                 ; real part / f(x_0)
   #:hd-d1                    ; ε₁ part = f'(x_0)
   #:hd-d2                    ; ε₂ part = f'(x_0) (under deriv-seed)
   #:hd-d12                   ; ε₁ε₂ part = f''(x_0)
   #:hd-first-derivative
   #:hd-second-derivative
   #:hyper-dual-real
   #:hyper-dual-e1
   #:hyper-dual-e2
   #:hyper-dual-e12

   ;; --- ring operators ----
   #:hd-add
   #:hd-sub
   #:hd-neg
   #:hd-mul
   #:hd-recip
   #:hd-div

   ;; --- transcendental / analytic ----
   #:hd-exp
   #:hd-log
   #:hd-sqrt
   #:hd-sin
   #:hd-cos
   #:hd-power
   #:hd-univariate
   #:hd-normal-cdf
   #:hd-normal-pdf
   #:hd-inverse-normal-cdf
   #:hd-inverse-gaussian-quantile

   ;; --- convenience ----
   #:hd-derivatives
   #:hd-bs-call

   ;; --- multivariate gradient & Hessian (hyper-dual seed batching) ----
   #:hd-gradient
   #:hd-grad-hessian

   ;; --- multivariate Taylor jets (mvjet) ----
   #:mvjet
   #:mvjet-p
   #:ensure-mvjet-layout
   #:mvjet-from-real
   #:mvjet-num-vars
   #:mvjet-order
   #:mvjet-seeds
   #:mvjet-derivatives
   #:mvjet-grad-hessian
   #:mvjet-value
   #:mvjet-coeff
   #:mvjet-partial
   #:mvjet-gradient
   #:mvjet-hessian
   #:mvjet-add
   #:mvjet-sub
   #:mvjet-neg
   #:mvjet-scale
   #:mvjet-mul
   #:mvjet-recip
   #:mvjet-div
   #:mvjet-exp
   #:mvjet-log
   #:mvjet-sqrt
   #:mvjet-power

   ;; --- jet3 ----
   #:jet3
   #:jet3-p
   #:make-jet3                ; canonical constructor (a0 a1 a2 a3)
   #:j3                       ; upstream alias
   #:jet3-from-real
   #:jet3-deriv-seed
   #:jet3-value
   #:jet3-d1
   #:jet3-d2
   #:jet3-d3
   #:jet3-a0
   #:jet3-a1
   #:jet3-a2
   #:jet3-a3
   #:jet3-add
   #:jet3-sub
   #:jet3-neg
   #:jet3-mul
   #:jet3-recip
   #:jet3-div
   #:jet3-exp
   #:jet3-log
   #:jet3-sqrt
   #:jet3-derivatives

   ;; --- jet4 ----
   #:jet4
   #:jet4-p
   #:make-jet4
   #:j4
   #:jet4-from-real
   #:jet4-deriv-seed
   #:jet4-value
   #:jet4-d1
   #:jet4-d2
   #:jet4-d3
   #:jet4-d4
   #:jet4-a0
   #:jet4-a1
   #:jet4-a2
   #:jet4-a3
   #:jet4-a4
   #:jet4-add
   #:jet4-sub
   #:jet4-neg
   #:jet4-mul
   #:jet4-recip
   #:jet4-div
   #:jet4-exp
   #:jet4-log
   #:jet4-sqrt
   #:jet4-derivatives

   ;; --- jet-n (arbitrary-order univariate) ----
   #:jetn
   #:jetn-p
   #:jetn-coeffs
   #:make-jetn
   #:jetn-from-real
   #:jetn-deriv-seed
   #:jetn-order
   #:jetn-coeff
   #:jetn-value
   #:jetn-derivative
   #:jetn-derivatives
   #:jetn-add
   #:jetn-sub
   #:jetn-neg
   #:jetn-scale
   #:jetn-mul
   #:jetn-recip
   #:jetn-div
   #:jetn-exp
   #:jetn-log
   #:jetn-sqrt
   #:jetn-power

   ;; --- scalar-tolerance <-> jet bridge ----
   #:jet-tolerance-certificate
   #:jet-tolerance-certificate-p
   #:jet-tolerance-certificate-budgets
   #:jet-tolerance-certificate-residuals
   #:jet-tolerance-certificate-normalized-residuals
   #:jet-tolerance-certificate-max-normalized-residual
   #:jet-tolerance-certificate-passed-p
   #:jet-derivative-vector
   #:jet-tolerance-profile
   #:certify-jet-tolerance

   ;; --- exact-jet (arbitrary-order, EXACT rational carrier) ----
   #:exact-jet
   #:exact-jet-p
   #:exact-jet-coeffs
   #:make-exact-jet
   #:exact-jet-from-rational
   #:exact-jet-deriv-seed
   #:exact-jet-order
   #:exact-jet-coeff
   #:exact-jet-value
   #:exact-jet-derivative
   #:exact-jet-add
   #:exact-jet-sub
   #:exact-jet-neg
   #:exact-jet-mul
   #:exact-jet-recip
   #:exact-jet-div
   #:exact-jet-expt
   #:taylor-exact
   #:derivative-n-exact

   ;; --- utility re-exports (used by hd-normal-cdf etc.) ----
   #:normal-pdf
   #:normal-cdf
   #:inverse-normal-cdf
   #:black-scholes-d1-drift-term
   #:inverse-gaussian-pdf
   #:inverse-gaussian-cdf
   #:inverse-gaussian-quantile))
