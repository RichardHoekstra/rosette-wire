;;;; analysis-extraction.lisp --- extraction candidates and patch sketches.

(in-package #:rosette-sexpr-dag)

(defstruct (extraction-candidate
            (:constructor make-extraction-candidate
                (name row parameters template call-template score
                 runtime-weight runtime-label)))
  name
  row
  parameters
  template
  call-template
  score
  runtime-weight
  runtime-label)

(defun read-runtime-costs (pathname)
  "Read runtime cost records from PATHNAME with read-time evaluation disabled.

Records are plists such as:
  (:match \"rosette-gguf/src/gpu.lisp\" :weight 8.0 :label \"GPU hot path\")
  (:symbol \"matrix-vector\" :weight 3.0 :label \"traced matvec\")

The top-level value may be a single plist or a list of plists.  :MATCH is a
simple substring check against candidate provenance file names.  :SYMBOL matches
symbols present in the candidate form, so profilers and lightweight tracers can
emit stable, dependency-free input without exact source spans."
  (with-open-file (stream pathname :direction :input)
    (let ((*read-eval* nil))
      (let ((records (read stream nil nil)))
        (cond ((null records) nil)
              ((and (consp records) (keywordp (first records)))
               (list records))
              (t records))))))

(defun %runtime-cost-weight (record)
  (let ((weight (getf record :weight 1.0d0)))
    (if (numberp weight)
        (coerce weight 'double-float)
        1.0d0)))

(defun %runtime-cost-file-match-p (record pathname)
  (let ((needle (getf record :match)))
    (and needle
         pathname
         (search (string needle) (string pathname)))))

(defun %runtime-cost-symbol-match-p (record sexp)
  (let ((needle (getf record :symbol)))
    (and needle
         (%sexp-contains-symbol-p
          sexp
          (list (string-downcase (string needle)))))))

(defun %runtime-cost-match-p (record row sexp)
  (or (%runtime-cost-symbol-match-p record sexp)
      (some (lambda (prov)
              (%runtime-cost-file-match-p record (getf prov :file)))
            (shared-subdag-provenance row))))

(defun %row-runtime-cost (row sexp runtime-costs)
  (let ((best-weight 1.0d0)
        (best-label nil))
    (dolist (record runtime-costs)
      (when (%runtime-cost-match-p record row sexp)
        (let ((weight (%runtime-cost-weight record)))
          (when (> weight best-weight)
            (setf best-weight weight
                  best-label (getf record :label))))))
    (values best-weight best-label)))

(defun %collect-symbol-leaves (sexp)
  (let ((symbols nil))
    (labels ((walk (x &optional operator-position-p)
               (cond ((consp x)
                      (walk (car x) t)
                      (let ((tail (cdr x)))
                        (loop while (consp tail) do
                          (walk (car tail) nil)
                          (setf tail (cdr tail)))
                        (when tail
                          (walk tail nil))))
                     ((and (symbolp x)
                           (not operator-position-p)
                           (not (keywordp x))
                           x
                           (not (eq (symbol-package x)
                                    (find-package :cl)))
                           (not (member x '(quote function lambda let let*
                                            flet labels loop dotimes dolist
                                            setf incf decf if when unless cond
                                            progn declare declaim the type)
                                        :test #'eq)))
                      (pushnew x symbols :test #'eq)))))
      (walk sexp))
    (sort symbols #'string< :key #'symbol-name)))

(defun %candidate-name (row sexp)
  (format nil "~(~A-~A-~D~)"
          (shared-subdag-motif sexp)
          (shared-subdag-shape sexp)
          (dag-node-id (shared-subdag-node row))))

(defun %extractable-expression-p (sexp)
  (and (consp sexp)
       (not (consp (first sexp)))
       (not (%sexp-head-name-in-p
             sexp
             '("declare" "declaim" "proclaim"
               "&key" "&optional" "&rest")))))

(defun extraction-candidates (index &key (limit 10)
                                        (min-size 8)
                                        (min-occurrences 2)
                                        runtime-costs)
  "Return draft extraction candidates for repeated source-like sub-DAGs."
  (let ((candidates nil))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 64))
      (let* ((sexp (shared-subdag-sexp index row))
             (kinds (shared-subdag-kinds row))
             (role (shared-subdag-role sexp))
             (shape (shared-subdag-shape sexp)))
        (when (and (< (length candidates) limit)
                   (member :source kinds :test #'eq)
                   (%actionable-shared-subdag-p row sexp)
                   (not (%transparent-role-p role))
                   (not (member role '(:byte-buffer :literal-data)
                                :test #'eq))
                   (not (member shape
                                '(:declaration :lambda-list
                                  :lambda-list-fragment
                                  :binding-list
                                  :literal-data)
                                :test #'eq))
                   (%extractable-expression-p sexp))
          (let ((params (%collect-symbol-leaves sexp)))
            (when (and params
                       (not (member (first sexp) params :test #'eq)))
              (let* ((name (%candidate-name row sexp))
                     (symbol (intern (string-upcase name))))
                (multiple-value-bind (runtime-weight runtime-label)
                    (%row-runtime-cost row sexp runtime-costs)
                  (push (make-extraction-candidate
                         name
                         row
                         params
                         `(defun ,symbol ,params ,sexp)
                         (cons symbol params)
                         (* (shared-subdag-beta1 row) runtime-weight)
                         runtime-weight
                         runtime-label)
                        candidates))))))))
    (let ((ranked (sort candidates #'>
                        :key #'extraction-candidate-score)))
      (subseq ranked 0 (min limit (length ranked))))))

(defun print-extraction-candidates
    (index &key (stream *standard-output*) (limit 10) (min-size 8)
                (min-occurrences 2) (sexp-limit 100) runtime-costs)
  "Print draft helper signatures for repeated source sub-DAGs."
  (format stream "~&Extraction candidates~%")
  (loop for candidate in (extraction-candidates index
                                                :limit limit
                                                :min-size min-size
                                                :min-occurrences min-occurrences
                                                :runtime-costs runtime-costs)
        for rank from 1
        for row = (extraction-candidate-row candidate)
        do (format stream "~&~D. ~A size=~D occurrences=~D beta1=~D~%"
                   rank
                   (extraction-candidate-name candidate)
                   (shared-subdag-size row)
                   (shared-subdag-occurrences row)
                   (shared-subdag-beta1 row))
           (when runtime-costs
             (format stream "   score=~,2F runtime-weight=~,2F"
                     (extraction-candidate-score candidate)
                     (extraction-candidate-runtime-weight candidate))
             (when (extraction-candidate-runtime-label candidate)
               (format stream " label=~A"
                       (extraction-candidate-runtime-label candidate)))
             (terpri stream))
           (format stream "   parameters=~S~%"
                   (extraction-candidate-parameters candidate))
           (format stream "   gravity=~A~%"
                   (shared-subdag-gravity
                    row
                    (shared-subdag-sexp index row)))
           (format stream "   template=~A~%"
                   (%short-sexp-string
                    (extraction-candidate-template candidate)
                    sexp-limit))
           (format stream "   call=~S~%"
                   (extraction-candidate-call-template candidate))))

(defun print-extraction-patches
    (index &key (stream *standard-output*) (limit 10) (min-size 8)
                (min-occurrences 2) (sexp-limit 120))
  "Print reviewable helper-definition drafts for extraction candidates.

This intentionally does not rewrite source files.  It gives a concrete patch
body plus provenance so a human can choose the owning package and call sites."
  (format stream "~&Extraction patch drafts~%")
  (let ((candidates (extraction-candidates index
                                           :limit limit
                                           :min-size min-size
                                           :min-occurrences min-occurrences)))
    (if candidates
        (loop for candidate in candidates
              for rank from 1
              for row = (extraction-candidate-row candidate)
              for sexp = (shared-subdag-sexp index row)
              do (format stream "~&;;; Candidate ~D: ~A~%"
                         rank
                         (extraction-candidate-name candidate))
                 (format stream ";;; gravity: ~A~%"
                         (shared-subdag-gravity row sexp))
                 (format stream ";;; occurrences: ~D  size: ~D  beta1: ~D~%"
                         (shared-subdag-occurrences row)
                         (shared-subdag-size row)
                         (shared-subdag-beta1 row))
                 (dolist (prov (subseq (shared-subdag-provenance row)
                                       0
                                       (min 5 (length (shared-subdag-provenance row)))))
                   (format stream ";;; at ~A form ~A~%"
                           (getf prov :file)
                           (getf prov :form-index)))
                 (format stream "~S~2%" (extraction-candidate-template candidate))
                 (format stream ";;; replace repeated form like: ~A~%"
                         (%short-sexp-string sexp sexp-limit))
                 (format stream ";;; with call: ~S~2%"
                         (extraction-candidate-call-template candidate)))
        (format stream "  none at current thresholds~%"))))

