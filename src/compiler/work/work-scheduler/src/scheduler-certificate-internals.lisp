;;;; scheduler-certificate-internals.lisp --- private certificate payload helpers.

(in-package #:rosette-work-scheduler)

(defun %work-item-signature (item)
  (list :name (work-item-name item)
        :reads (work-item-reads item)
        :writes (work-item-writes item)
        :cost (work-item-cost item)
        :depends-on (work-item-depends-on item)
        :commutes-with (work-item-commutes-with item)))

(defun %candidate-signatures (candidates)
  (mapcar (lambda (candidate)
            (let ((name (getf candidate :name))
                  (items (getf candidate :items)))
              (list :name name
                    :items (mapcar #'%work-item-signature items))))
          candidates))

(defun %action-ensemble-payload (candidates ensemble compute-weight
                                            critical-path-weight
                                            communication-weight
                                            parallelism-pressure-weight)
  (list :candidates (%candidate-signatures candidates)
        :weights (%action-weight-plist compute-weight
                                       critical-path-weight
                                       communication-weight
                                       parallelism-pressure-weight)
        :temperature (work-action-ensemble-temperature ensemble)
        :winner (and (work-action-ensemble-winner ensemble)
                     (work-action-choice-name
                      (work-action-ensemble-winner ensemble)))
        :probabilities (work-action-ensemble-probabilities ensemble)
        :free-energy (work-action-ensemble-free-energy ensemble)
        :log-partition (work-action-ensemble-log-partition ensemble)
        :scores (%action-choices->score-rows
                 (work-action-ensemble-choices ensemble))))

(defun %verify-action-ensemble-payload (expected actual)
  (let ((reasons nil))
    (labels ((field= (field &key (test #'equal))
               (let ((left (getf expected field))
                     (right (getf actual field)))
                 (unless (funcall test left right)
                   (push (list :kind :ensemble-payload-mismatch
                               :field field
                               :expected left
                               :actual right)
                         reasons)))))
      (field= :weights)
      (field= :temperature :test #'%close-real=)
      (field= :winner)
      (field= :probabilities :test #'%probability-rows=)
      (field= :free-energy :test #'%close-real=)
      (field= :log-partition :test #'%close-real=)
      (field= :scores)
      (%finish-reasons reasons))))

(defun %close-real= (a b)
  (and (realp a)
       (realp b)
       (f64-distance< a b 1d-12)))

(defun %probability-rows= (left right)
  (and (= (length left) (length right))
       (every (lambda (a b)
                (and (equal (getf a :name) (getf b :name))
                     (%close-real= (getf a :probability)
                                   (getf b :probability))
                     (%close-real= (getf a :score)
                                   (getf b :score))))
              left right)))

(defun %verify-item-signatures (items signatures)
  (let ((reasons nil)
        (table (%items-by-name items)))
    (cond
      ((null signatures)
       (push (list :kind :missing-payload :field :items) reasons))
      (t
       (dolist (signature signatures)
         (let* ((name (getf signature :name))
                (item (gethash name table)))
           (cond
             ((null item)
              (push (list :kind :unknown-task :task name) reasons))
             ((not (equal signature (%work-item-signature item)))
              (push (list :kind :item-signature-mismatch
                          :task name
                          :expected signature
                          :actual (%work-item-signature item))
                    reasons)))))))
    (%finish-reasons reasons)))

(defun %verify-candidate-signatures (candidates signatures)
  (let ((reasons nil)
        (table (%make-equal-table)))
    (dolist (candidate candidates)
      (let ((name (getf candidate :name)))
        (when (gethash name table)
          (push (list :kind :duplicate-candidate :candidate name) reasons))
        (setf (gethash name table) candidate)))
    (cond
      ((null signatures)
       (push (list :kind :missing-payload :field :candidates) reasons))
      (t
       (dolist (signature signatures)
         (let* ((name (getf signature :name))
                (candidate (gethash name table)))
           (cond
             ((null candidate)
              (push (list :kind :unknown-candidate :candidate name) reasons))
             ((not (equal (getf signature :items)
                          (mapcar #'%work-item-signature
                                  (getf candidate :items))))
              (push (list :kind :candidate-signature-mismatch
                          :candidate name
                          :expected (getf signature :items)
                          :actual (mapcar #'%work-item-signature
                                          (getf candidate :items)))
                    reasons)))))))
    (dolist (candidate candidates)
      (unless (find (getf candidate :name) signatures
                    :key (lambda (signature) (getf signature :name))
                    :test #'equal)
        (push (list :kind :unclaimed-candidate
                    :candidate (getf candidate :name))
              reasons)))
    (%finish-reasons reasons)))
