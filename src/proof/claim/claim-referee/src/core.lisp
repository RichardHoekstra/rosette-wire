;;;; core.lisp --- the falsifiable-claim referee.
;;;;
;;;; A claim is data: a name, a kind, a claimed value (or nil for a
;;;; composite), a tolerance, an optional measurement thunk, and -- for a
;;;; composite -- a list of sub-claims.  The referee runs the measurement
;;;; and emits a structured verdict.
;;;;
;;;; The taxonomy is deliberately small and orthogonal to the obligation
;;;; taxonomy of rosette-theory-verdict (which classifies *proof* obligations:
;;;; closed/open/blocked).  This one classifies *empirical* claims by what a
;;;; measurement does to them:
;;;;
;;;;   :confirmed   measurement upholds the claim (within tol, or >= / <=)
;;;;   :refuted     measurement contradicts the claim, with the actual number
;;;;   :partial     a composite where some sub-claims hold and others fail
;;;;   :untestable  measurement errored or is missing -- never :confirmed
;;;;
;;;; The skeptical default is the whole point: an unmeasurable claim is
;;;; :untestable, not :confirmed.  Extraordinary claims earn their verdict
;;;; from a number, or they do not get one.

(in-package #:rosette-claim-referee)

;;; ----------------------------------------------------------------------
;;; Verdict taxonomy
;;; ----------------------------------------------------------------------

(defparameter +verdict-keywords+
  '(:confirmed :refuted :partial :untestable)
  "The canonical, closed taxonomy of claim verdicts.")

(defun verdict-keywords ()
  "Return the canonical list of verdict status keywords."
  (copy-list +verdict-keywords+))

(defun verdict-keyword-p (k)
  "T iff K is a member of the canonical verdict taxonomy."
  (and (member k +verdict-keywords+) t))

;;; ----------------------------------------------------------------------
;;; Claim records
;;; ----------------------------------------------------------------------

(defstruct (claim (:constructor %make-claim))
  "An empirical, falsifiable claim modelled as data.
KIND is one of :point :lower-bound :upper-bound :composite.
MEASURE is a thunk returning the measured value (nil for :composite).
SUBCLAIMS is a list of CLAIM structs (:composite only)."
  (name "" :type string)
  (kind :point :type keyword)
  (claimed nil)
  (tolerance +default-tolerance+)
  (measure nil :type (or null function))
  (subclaims nil :type list))

(defun point-claim (name claimed measure &key (tolerance +default-tolerance+))
  "A claim that the measured value equals CLAIMED within TOLERANCE.
MEASURE is a zero-argument function returning the measured value."
  (check-type name string)
  (check-type measure function)
  (%make-claim :name name :kind :point :claimed claimed
               :tolerance tolerance :measure measure))

(defun lower-bound-claim (name claimed measure)
  "A claim that the measured value is >= CLAIMED (e.g. 'speedup >= 10x').
Boundary convention: measured == claimed CONFIRMS (documented >=)."
  (check-type name string)
  (check-type measure function)
  (%make-claim :name name :kind :lower-bound :claimed claimed :measure measure))

(defun upper-bound-claim (name claimed measure)
  "A claim that the measured value is <= CLAIMED.
Boundary convention: measured == claimed CONFIRMS (documented <=)."
  (check-type name string)
  (check-type measure function)
  (%make-claim :name name :kind :upper-bound :claimed claimed :measure measure))

(defun composite-claim (name subclaims)
  "A claim composed of SUBCLAIMS.  The verdict separates which parts hold:
all confirmed -> :confirmed, none -> :refuted, mixed -> :partial.  This is
the fusion 'ignition != Q>1 != plant-positive' decomposition as a kernel."
  (check-type name string)
  (assert (and subclaims (every #'claim-p subclaims)) (subclaims)
          "composite-claim needs a non-empty list of CLAIM structs.")
  (%make-claim :name name :kind :composite :subclaims (copy-list subclaims)))

(defun permutation-control-claim (name effect-fn resample-fn
                                  &key (statistic :z) (perms 200) (threshold 2d0))
  "A NEGATIVE-CONTROL claim: the observed effect must stand out from its own
label-shuffle null.  This is the permutation control that experiments carry as a
first-class claim -- the discipline that separates a real effect from a
selection / batch / instrument artifact.

EFFECT-FN is a zero-argument thunk returning the observed effect on the TRUE
labelling.  RESAMPLE-FN is a zero-argument thunk returning the effect under ONE
random RE-labelling; the caller owns the shuffle, so the kernel stays
domain-agnostic (genotype labels, variant labels, condition labels, ...).

STATISTIC :z     -> the claim is (obs - mean_null) / sd_null >= THRESHOLD
                    (default 2 sigma above the shuffle null);
STATISTIC :ratio -> the claim is obs / |mean_null| >= THRESHOLD.

Returns a :lower-bound claim on that statistic, so ADJUDICATE confirms iff the
real effect beats the shuffled null and refutes when an artifact survives
relabelling.  A non-numeric or erroring measurement defaults to :untestable via
the usual skeptical gate."
  (check-type name string)
  (check-type effect-fn function)
  (check-type resample-fn function)
  (check-type perms (integer 1))
  (lower-bound-claim
   name threshold
   (lambda ()
     (let* ((obs (funcall effect-fn))
            (nulls (loop repeat perms collect (funcall resample-fn)))
            (mu (/ (reduce #'+ nulls) perms)))
       (ecase statistic
         (:ratio (/ (float obs 1d0) (max 1d-12 (abs (float mu 1d0)))))
         (:z (let ((sd (sqrt (/ (loop for e in nulls sum (expt (- e mu) 2))
                                (max 1 (1- perms))))))
               (/ (- (float obs 1d0) (float mu 1d0)) (max 1d-12 sd)))))))))

;;; ----------------------------------------------------------------------
;;; Verdict records
;;; ----------------------------------------------------------------------

(defstruct (verdict (:constructor %make-verdict))
  "The referee's structured judgement of a claim."
  (name "" :type string)
  (status :untestable :type keyword)
  (claimed nil)
  (measured nil)
  (margin nil)        ; signed measured - claimed (point / bound claims)
  (ratio nil)         ; measured / claimed -- the "10x -> 2x" factor
  (reason "" :type string)
  (subverdicts nil :type list))

(defun verdict-holding-count (v)
  "Number of immediate sub-verdicts that are :confirmed (0 for leaf claims)."
  (count :confirmed (verdict-subverdicts v) :key #'verdict-status))

;;; ----------------------------------------------------------------------
;;; Measurement: the skeptical gate
;;; ----------------------------------------------------------------------

(defun %safe-measure (thunk)
  "Run THUNK, returning (values OK VALUE).  Any error -> (values nil nil).
A non-number result is also treated as a measurement failure: the referee
adjudicates numbers, and refuses to confirm on anything else."
  (handler-case
      (let ((v (funcall thunk)))
        (if (realp v)
            (values t v)
            (values nil nil)))
    (error () (values nil nil))))

(defun %ratio (measured claimed)
  "measured / claimed, or NIL when claimed is zero (ratio undefined)."
  (if (and (realp measured) (realp claimed) (not (zerop claimed)))
      (/ (float measured 1d0) (float claimed 1d0))
      nil))

;;; ----------------------------------------------------------------------
;;; The referee
;;; ----------------------------------------------------------------------

(defun adjudicate (claim)
  "Run the measurement(s) behind CLAIM and emit a VERDICT.
Never signals on a faulty measurement: a thunk that errors or returns a
non-number yields :untestable."
  (check-type claim claim)
  (ecase (claim-kind claim)
    (:point        (%adjudicate-point claim))
    (:lower-bound  (%adjudicate-bound claim :lower))
    (:upper-bound  (%adjudicate-bound claim :upper))
    (:composite    (%adjudicate-composite claim))))

(defun %adjudicate-point (claim)
  (multiple-value-bind (ok measured) (%safe-measure (claim-measure claim))
    (if (not ok)
        (%make-verdict
         :name (claim-name claim) :status :untestable
         :claimed (claim-claimed claim) :measured nil
         :reason "measurement errored or returned a non-number; untestable.")
        (let* ((claimed (claim-claimed claim))
               (tol (claim-tolerance claim))
               (margin (- measured claimed))
               (ratio (%ratio measured claimed))
               (hit (approx= measured claimed tol)))
          (%make-verdict
           :name (claim-name claim)
           :status (if hit :confirmed :refuted)
           :claimed claimed :measured measured :margin margin :ratio ratio
           :reason (if hit
                       (format nil "measured ~G within tol ~G of claimed ~G."
                               measured tol claimed)
                       (format nil "measured ~G off claimed ~G by ~G (tol ~G)."
                               measured claimed margin tol)))))))

(defun %adjudicate-bound (claim side)
  (multiple-value-bind (ok measured) (%safe-measure (claim-measure claim))
    (if (not ok)
        (%make-verdict
         :name (claim-name claim) :status :untestable
         :claimed (claim-claimed claim) :measured nil
         :reason "measurement errored or returned a non-number; untestable.")
        (let* ((claimed (claim-claimed claim))
               (margin (- measured claimed))
               (ratio (%ratio measured claimed))
               ;; Boundary convention: >= and <= both CONFIRM at equality.
               (hit (ecase side
                      (:lower (>= measured claimed))
                      (:upper (<= measured claimed))))
               (rel (ecase side (:lower ">=") (:upper "<="))))
          (%make-verdict
           :name (claim-name claim)
           :status (if hit :confirmed :refuted)
           :claimed claimed :measured measured :margin margin :ratio ratio
           :reason (if hit
                       (format nil "measured ~G ~A claimed ~G (margin ~G)."
                               measured rel claimed margin)
                       ;; The GeoRefine / over-optimism row: report the factor.
                       (format nil "claimed ~A ~G but measured ~G~@[ (factor ~,2F)~]."
                               rel claimed measured ratio)))))))

(defun %adjudicate-composite (claim)
  (let* ((subs (mapcar #'adjudicate (claim-subclaims claim)))
         (n (length subs))
         (held (count :confirmed subs :key #'verdict-status))
         (testable (count :untestable subs :key #'verdict-status :test-not #'eql))
         (status (cond
                   ;; Nothing could be measured at all -> untestable.
                   ((zerop testable) :untestable)
                   ;; Every (testable) sub-claim confirmed, none failed.
                   ((= held n) :confirmed)
                   ;; Some held, some did not (failed or untestable) -> partial.
                   ((plusp held) :partial)
                   ;; None held, but some were testable -> refuted.
                   (t :refuted)))
         (failing (remove :confirmed subs :key #'verdict-status)))
    (%make-verdict
     :name (claim-name claim)
     :status status
     :claimed nil :measured nil
     :subverdicts subs
     :reason (format nil "~D/~D sub-claims confirmed~@[; not holding: ~{~A~^, ~}~]."
                     held n
                     (when failing (mapcar #'verdict-name failing))))))

;;; ----------------------------------------------------------------------
;;; Rendering
;;; ----------------------------------------------------------------------

(defun %status-tag (status)
  (ecase status
    (:confirmed  "CONFIRMED ")
    (:refuted    "REFUTED   ")
    (:partial    "PARTIAL   ")
    (:untestable "UNTESTABLE")))

(defun verdict-row-string (v)
  "One-line rendering of a single (leaf) verdict."
  (format nil "[~A] ~A -- ~A"
          (%status-tag (verdict-status v))
          (verdict-name v)
          (verdict-reason v)))

(defun verdict-table-string (v)
  "Multi-line rendering of V; indents sub-verdicts of a composite claim."
  (with-output-to-string (s)
    (format s "~A~%" (verdict-row-string v))
    (dolist (sub (verdict-subverdicts v))
      (format s "    ~A~%" (verdict-row-string sub)))))
