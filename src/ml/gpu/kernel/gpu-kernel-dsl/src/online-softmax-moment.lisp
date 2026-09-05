;;;; online-softmax-moment.lisp --- stable streaming normalized moments.

(in-package #:rosette-gpu-kernel-dsl)

(defun make-online-softmax-moment
    (&key (moments 1) (carrier :float) (order :token)
          (seed :first-item))
  "Return the canonical identity of a stable online softmax moment algebra."
  (unless (and (integerp moments) (plusp moments))
    (error 'dsl-syntax-error :form moments
           :reason "online softmax moment count must be positive"))
  (unless (eq carrier :float)
    (error 'dsl-syntax-error :form carrier
           :reason "online softmax moments currently require an f32 carrier"))
  (unless (keywordp order)
    (error 'dsl-syntax-error :form order
           :reason "online softmax order must be a keyword identity"))
  (unless (eq seed :first-item)
    (error 'dsl-syntax-error :form seed
           :reason "online softmax currently requires a nonempty first-item seed"))
  (list :schema :rosette-online-softmax-moment/v1
        :moments moments :carrier carrier :order order :seed seed
        :merge :directional-singleton))

(defun %validated-online-softmax-moment (algebra)
  (unless (and (listp algebra)
               (eq (getf algebra :schema) :rosette-online-softmax-moment/v1))
    (error 'dsl-syntax-error :form algebra
           :reason "not an :ROSETTE-ONLINE-SOFTMAX-MOMENT/V1 identity"))
  (let ((canonical
          (make-online-softmax-moment
           :moments (getf algebra :moments)
           :carrier (getf algebra :carrier)
           :order (getf algebra :order)
           :seed (getf algebra :seed))))
    (unless (equal algebra canonical)
      (error 'dsl-syntax-error :form algebra
             :reason "online softmax moment identity is non-canonical"))
    canonical))

(defun %online-moment-state-pairs (algebra weighted-moments)
  (unless (and (listp weighted-moments)
               (= (length weighted-moments) (getf algebra :moments))
               (every (lambda (pair)
                        (and (listp pair) (= (length pair) 2)
                             (symbolp (first pair))
                             (not (keywordp (first pair)))))
                      weighted-moments)
               (= (length weighted-moments)
                  (length (remove-duplicates (mapcar #'first weighted-moments)))))
    (error 'dsl-syntax-error :form weighted-moments
           :reason "online softmax requires unique (STATE VALUE) moment pairs"))
  weighted-moments)

(defun online-softmax-moment-step-form
    (algebra index score maximum mass weighted-moments
     &key (correction 'online-correction) (weight 'online-weight))
  "Compose one stable ordered singleton merge into (MAXIMUM,MASS,MOMENTS...).

INDEX zero seeds the nonempty stream. Every later exponential argument is
non-positive. WEIGHTED-MOMENTS is a list of (NUMERATOR VALUE) pairs."
  (let* ((canonical (%validated-online-softmax-moment algebra))
         (pairs (%online-moment-state-pairs canonical weighted-moments)))
    (unless (and (symbolp maximum) (symbolp mass)
                 (symbolp correction) (symbolp weight)
                 (notany #'keywordp (list maximum mass correction weight)))
      (error 'dsl-syntax-error
             :form (list maximum mass correction weight)
             :reason "online softmax state and scratch names must be symbols"))
    `(if (= ,index 0)
         (progn
           (setq ,maximum ,score)
           (setq ,mass 1.0)
           ,@(loop for (state value) in pairs
                   collect `(setq ,state ,value)))
         (if (> ,score ,maximum)
             (let ((,correction (call expf (- ,maximum ,score))))
               (setq ,mass (+ (* ,mass ,correction) 1.0))
               ,@(loop for (state value) in pairs
                       collect `(setq ,state (+ (* ,state ,correction) ,value)))
               (setq ,maximum ,score))
             (let ((,weight (call expf (- ,score ,maximum))))
               (setq ,mass (+ ,mass ,weight))
               ,@(loop for (state value) in pairs
                       collect `(setq ,state (+ ,state (* ,value ,weight)))))))))

(defun online-softmax-moment-finish-form
    (algebra mass weighted-moments)
  "Invert MASS in place and normalize every moment numerator in place."
  (let* ((canonical (%validated-online-softmax-moment algebra))
         (pairs (%online-moment-state-pairs canonical weighted-moments)))
    `(progn
       (setq ,mass (/ 1.0 ,mass))
       ,@(loop for pair in pairs
               for state = (first pair)
               collect `(setq ,state (* ,state ,mass))))))

(defun online-softmax-moment-coefficients-form
    (algebra index score maximum mass factor renormalize)
  "Compose the owner-only transition for a distributed online moment.

The owner mutates MAXIMUM/MASS and emits FACTOR plus integer RENORMALIZE.
Other workers broadcast those two scalars and update their disjoint moment
components with ONLINE-SOFTMAX-MOMENT-DISTRIBUTED-UPDATE-FORM. Exactly one
worker therefore evaluates the bounded exponential for each stream item."
  (%validated-online-softmax-moment algebra)
  (unless (and (every #'symbolp
                      (list index score maximum mass factor renormalize))
               (notany #'keywordp
                       (list index score maximum mass factor renormalize)))
    (error 'dsl-syntax-error
           :form (list index score maximum mass factor renormalize)
           :reason "distributed online softmax names must be symbols"))
  `(if (= ,index 0)
       (progn
         (setq ,maximum ,score)
         (setq ,mass 1.0)
         (setq ,factor 0.0)
         (setq ,renormalize 0))
       (if (> ,score ,maximum)
           (progn
             (setq ,factor (call expf (- ,maximum ,score)))
             (setq ,mass (+ (* ,mass ,factor) 1.0))
             (setq ,maximum ,score)
             (setq ,renormalize 1))
           (progn
             (setq ,factor (call expf (- ,score ,maximum)))
             (setq ,mass (+ ,mass ,factor))
             (setq ,renormalize 0)))))

(defun online-softmax-moment-distributed-update-form
    (algebra index state value factor renormalize)
  "Update one worker-owned moment using owner-broadcast transition data.

The branch shape deliberately preserves the directional singleton algebra's
association: first item assigns, a new maximum rescales the old state before
adding VALUE, and the ordinary branch scales only VALUE."
  (%validated-online-softmax-moment algebra)
  (unless (and (every #'symbolp
                      (list index state factor renormalize))
               (notany #'keywordp
                       (list index state factor renormalize)))
    (error 'dsl-syntax-error
           :form (list index state factor renormalize)
           :reason "distributed online moment state names must be symbols"))
  `(if (= ,index 0)
       (setq ,state ,value)
       (if (= ,renormalize 1)
           (setq ,state (+ (* ,state ,factor) ,value))
           (setq ,state (+ ,state (* ,value ,factor))))))
