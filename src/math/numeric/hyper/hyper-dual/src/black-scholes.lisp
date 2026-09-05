;;;; black-scholes.lisp --- hyper-dual Black-Scholes examples/primitives.

(in-package #:rosette-hyper-dual)

(defun hd-bs-call (s-hd &key (strike 100d0)
                             (rate 0.02d0)
                             (dividend 0d0)
                             (volatility 0.2d0)
                             (maturity 1d0))
  "Return the Black-Scholes call price as a hyper-dual function of spot S-HD.

Other inputs are real double-float parameters.  Seeding S-HD with
HD-DERIV-SEED makes price, delta, and gamma appear as value, e1, and
e12 components of the result."
  (let* ((vol-rt (* volatility (sqrt maturity)))
         (d1 (hd-div (hd-add (hd-sub (hd-log s-hd) (log strike))
                             (black-scholes-d1-drift-term
                              rate dividend volatility maturity))
                     vol-rt))
         (d2 (hd-sub d1 vol-rt))
         (df-rate (exp (- (* rate maturity))))
         (df-div  (exp (- (* dividend maturity)))))
    (hd-sub (hd-mul df-div (hd-mul s-hd (hd-normal-cdf d1)))
            (hd-mul (* strike df-rate) (hd-normal-cdf d2)))))
