;;;; scheduler-effect-internals.lisp --- private effect-normalization helpers.

(in-package #:rosette-work-scheduler)

(defun %normalize-commutative-regions (word laws)
  (let ((result nil)
        (trace nil)
        (phase 1))
    (labels ((reorderable-p (symbol)
               (or (getf (getf laws symbol) :commutative)
                   (getf (getf laws symbol) :anticommutes-with)))
             (monotone-p (symbol)
               (getf (getf laws symbol) :monotone))
             (flush (region start)
               (when region
                 (let* ((original (nreverse region))
                        (sorted (sort (copy-list original) #'%effect-symbol<))
                        (region-phase (%anticommutative-phase original sorted
                                                              laws)))
                   (setf phase (* phase region-phase))
                   (setf result (append (nreverse sorted) result))
                   (unless (equal original sorted)
                     (push (list :law (if (= region-phase -1)
                                          :anticommutative
                                          :commutative)
                                 :start start
                                 :end (+ start (length original) -1)
                                 :original original
                                 :replacement sorted
                                 :phase region-phase)
                           trace)))))
             (emit-barrier (symbol index)
               (push symbol result)
               (when (monotone-p symbol)
                 (push (list :law :monotone
                             :symbol symbol
                             :index index
                             :preserved-p t)
                       trace))))
      (loop with region = nil
            with region-start = 0
            for symbol in word
            for index from 0
            do (if (reorderable-p symbol)
                   (progn
                     (unless region
                       (setf region-start index))
                     (push symbol region))
                   (progn
                     (flush region region-start)
                     (setf region nil)
                     (emit-barrier symbol index)))
            finally (flush region region-start)))
    (values (nreverse result) (nreverse trace) phase)))

(defun %normalize-effect-run (symbol count laws)
  (let* ((law (getf laws symbol))
         (nilpotent (getf law :nilpotent))
         (unipotent (getf law :unipotent)))
    (cond
      ((getf law :identity)
       (values nil
               (list :law :identity
                     :replacement nil)))
      ((and nilpotent (>= count nilpotent))
       (values nil
               (list :law :nilpotent
                     :index nilpotent
                     :replacement nil)))
      ((getf law :idempotent)
       (values (list symbol)
               (when (> count 1)
                 (list :law :idempotent
                       :replacement (list symbol)))))
      ((getf law :projector)
       (values (list symbol)
               (when (> count 1)
                 (list :law :projector
                       :replacement (list symbol)))))
      ((getf law :involutive)
       (let ((keep (if (oddp count) (list symbol) nil)))
         (values keep
                 (when (> count 1)
                   (list :law :involutive
                         :replacement keep)))))
      (unipotent
       (let* ((residual (mod count unipotent))
              (replacement (%repeat-effect (list :unipotent symbol residual)
                                           (if (zerop residual) 0 1))))
         (values replacement
                 (when (> count 1)
                   (list :law :unipotent
                         :index unipotent
                         :residual residual
                         :replacement replacement)))))
      (t
       (values (%repeat-effect symbol count) nil)))))

(defun %repeat-effect (value count)
  (loop repeat count collect value))

(defun %effect-symbol< (a b)
  (string< (prin1-to-string a) (prin1-to-string b)))

(defun %absorbing-effect (word laws)
  (loop for symbol in word
        for index from 0
        when (getf (getf laws symbol) :absorbing)
          do (return (values t symbol index))
        finally (return (values nil nil nil))))

(defun %remove-annihilating-pairs (word laws)
  (let ((stack nil)
        (trace nil))
    (dolist (symbol word)
      (if (and stack
               (%annihilating-p (first stack) symbol laws))
          (let ((left (pop stack)))
            (push (list :law :annihilation
                        :left left
                        :right symbol
                        :replacement nil)
                  trace))
          (push symbol stack)))
    (values (nreverse stack) (nreverse trace))))

(defun %annihilating-p (a b laws)
  (or (member b (getf (getf laws a) :annihilates) :test #'equal)
      (member a (getf (getf laws b) :annihilates) :test #'equal)))

(defun %anticommuting-p (a b laws)
  (or (member b (getf (getf laws a) :anticommutes-with) :test #'equal)
      (member a (getf (getf laws b) :anticommutes-with) :test #'equal)))

(defun %anticommutative-phase (original sorted laws)
  (let ((phase 1)
        (current (copy-list original)))
    (loop for target-position from 0
          for target in sorted
          do (let ((position (position target current :start target-position
                                       :test #'equal)))
               (loop while (> position target-position)
                     for left = (nth (1- position) current)
                     for right = (nth position current)
                     do (when (%anticommuting-p left right laws)
                          (setf phase (- phase)))
                        (rotatef (nth (1- position) current)
                                 (nth position current))
                        (decf position))))
    phase))
