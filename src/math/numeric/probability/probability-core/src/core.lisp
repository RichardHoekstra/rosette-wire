;;;; core.lisp --- scalar probability special functions.

(in-package #:rosette-probability-core)

(defparameter +sqrt-two-pi+ (sqrt (* 2d0 pi)))

(declaim (inline normal-pdf))
(defun normal-pdf (x)
  "Standard normal density phi(x) = (2*pi)^-1/2 exp(-x^2/2)."
  (let ((xd (as-f64 x)))
    (/ (exp (* -0.5d0 xd xd)) +sqrt-two-pi+)))

(defun normal-cdf (x)
  "Standard normal CDF. Abramowitz-Stegun 7.1.26 approximation."
  (let* ((xd (as-f64 x))
         (z (abs xd))
         (tt (/ 1d0 (+ 1d0 (* 0.2316419d0 z))))
         (poly (+ (* 0.319381530d0 tt)
                  (* -0.356563782d0 tt tt)
                  (* 1.781477937d0 tt tt tt)
                  (* -1.821255978d0 tt tt tt tt)
                  (* 1.330274429d0 tt tt tt tt tt)))
         (cdf (- 1d0 (* (normal-pdf z) poly))))
    (if (< xd 0d0) (- 1d0 cdf) cdf)))

(defun rational-polynomial (coefficients x)
  (let ((acc 0d0))
    (dolist (coefficient coefficients acc)
      (setf acc (+ (* acc x) coefficient)))))

(defun %midpoint (lo hi)
  (* 0.5d0 (+ lo hi)))

(defun %acklam-denominator (coefficients q)
  (+ (* (rational-polynomial coefficients q) q) 1d0))

(defun %acklam-tail-ratio (numerator denominator q)
  (/ (rational-polynomial numerator q)
     (%acklam-denominator denominator q)))

(defun inverse-normal-cdf (p)
  "Standard normal quantile. Peter J. Acklam's rational approximation."
  (let ((pd (as-f64 p)))
    (unless (< 0d0 pd 1d0)
      (error "Inverse normal CDF requires p in (0,1), got ~A." p))
    (let* ((a '(-3.969683028665376d1
                2.209460984245205d2
                -2.759285104469687d2
                1.383577518672690d2
                -3.066479806614716d1
                2.506628277459239d0))
           (b '(-5.447609879822406d1
                1.615858368580409d2
                -1.556989798598866d2
                6.680131188771972d1
                -1.328068155288572d1))
           (c '(-7.784894002430293d-3
                -3.223964580411365d-1
                -2.400758277161838d0
                -2.549732539343734d0
                4.374664141464968d0
                2.938163982698783d0))
           (d '(7.784695709041462d-3
                3.224671290700398d-1
                2.445134137142996d0
                3.754408661907416d0))
           (plow 0.02425d0)
           (phigh (- 1d0 plow)))
      (cond
        ((< pd plow)
         (let ((q (sqrt (* -2d0 (log pd)))))
           (%acklam-tail-ratio c d q)))
        ((> pd phigh)
         (let ((q (sqrt (* -2d0 (log (- 1d0 pd))))))
           (- (%acklam-tail-ratio c d q))))
        (t
         (let* ((q (- pd 0.5d0))
                (r (* q q)))
           (/ (* q (rational-polynomial a r))
              (%acklam-denominator b r))))))))

(defun black-scholes-d1-drift-term (rate dividend volatility maturity)
  "Return (r - q + sigma^2/2) T, the real drift term in Black-Scholes d1."
  (let ((r (as-f64 rate))
        (q (as-f64 dividend))
        (sigma (as-f64 volatility))
        (tt (as-f64 maturity)))
    (* (+ (- r q) (* 0.5d0 sigma sigma)) tt)))

(defun %positive-inverse-gaussian-args (x mu lambda context)
  (let ((xd (as-f64 x))
        (mud (as-f64 mu))
        (lambdad (as-f64 lambda)))
    (unless (and (> xd 0d0) (> mud 0d0) (> lambdad 0d0))
      (error "~A requires positive x, mu, lambda." context))
    (values xd mud lambdad)))

(defun %inverse-gaussian-quantile-args (p mu lambda)
  (let ((pd (as-f64 p))
        (mud (as-f64 mu))
        (lambdad (as-f64 lambda)))
    (unless (< 0d0 pd 1d0)
      (error "Inverse Gaussian quantile requires p in (0,1), got ~A." p))
    (unless (and (> mud 0d0) (> lambdad 0d0))
      (error "Inverse Gaussian quantile requires positive mu, lambda."))
    (values pd mud lambdad)))

(defun %residual-bracket (lo hi x residual)
  (if (> residual 0d0)
      (values lo x)
      (values x hi)))

(defun %safeguarded-newton-step (lo hi x residual pdf-x)
  (let* ((newton (if (> pdf-x 1d-300)
                     (- x (/ residual pdf-x))
                     x))
         (half-bracket (* 0.5d0 (- hi lo))))
    (if (and (< lo newton hi)
             (< (abs (- newton x)) half-bracket))
        newton
        (%midpoint lo hi))))

(defun inverse-gaussian-pdf (x mu lambda)
  "Inverse-Gaussian density f(x; mu, lambda). Arguments must be positive."
  (multiple-value-bind (xd mud lambdad)
      (%positive-inverse-gaussian-args x mu lambda "Inverse Gaussian PDF")
    (* (sqrt (/ lambdad (* 2d0 pi xd xd xd)))
       (exp (/ (* -1d0 lambdad (- xd mud) (- xd mud))
               (* 2d0 mud mud xd))))))

(defun inverse-gaussian-cdf (x mu lambda)
  "Inverse-Gaussian CDF F(x; mu, lambda). Arguments must be positive."
  (multiple-value-bind (xd mud lambdad)
      (%positive-inverse-gaussian-args x mu lambda "Inverse Gaussian CDF")
    (let* ((root (sqrt (/ lambdad xd)))
           (a (* root (- (/ xd mud) 1d0)))
           (b (- (* root (+ (/ xd mud) 1d0)))))
      (+ (normal-cdf a)
         (* (exp (/ (* 2d0 lambdad) mud))
            (normal-cdf b))))))

(defun inverse-gaussian-quantile
    (p mu lambda &key (tolerance 1d-12) (max-iterations 60))
  "Inverse-Gaussian quantile via safeguarded Newton on the IG CDF."
  (multiple-value-bind (pd mud lambdad)
      (%inverse-gaussian-quantile-args p mu lambda)
    (let ((lo least-positive-double-float)
          (hi (max 1d0 (* 2d0 mud))))
      (loop while (< (inverse-gaussian-cdf hi mud lambdad) pd)
            do (setf hi (* 2d0 hi)))
      (let ((x (%midpoint lo hi)))
        (dotimes (i max-iterations (%midpoint lo hi))
          (when (< (- hi lo) tolerance)
            (return (%midpoint lo hi)))
          (let* ((cdf-x (inverse-gaussian-cdf x mud lambdad))
                 (residual (- cdf-x pd)))
            (multiple-value-setq (lo hi)
              (%residual-bracket lo hi x residual))
            (let ((pdf-x (inverse-gaussian-pdf x mud lambdad)))
              (setf x (%safeguarded-newton-step
                       lo hi x residual pdf-x)))))))))
