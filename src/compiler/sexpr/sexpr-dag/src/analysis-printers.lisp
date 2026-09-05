;;;; analysis-printers.lisp --- layer-collapse and simplification printers.

(in-package #:rosette-sexpr-dag)

(defun %print-layer-collapse-sample (stream sample sexp-limit)
  (when sample
    (format stream
            "   sample size=~D occurrences=~D role=~S gravity=~A ~A~%"
            (getf sample :size)
            (getf sample :occurrences)
            (getf sample :role)
            (getf sample :gravity)
            (%short-sexp-string (getf sample :sexp) sexp-limit))))

(defun print-layer-target-pressure
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) (sexp-limit 80))
  "Print repeated sexpr pressure grouped by inferred lower-layer target."
  (let ((rows (layer-target-pressure index
                                     :limit limit
                                     :min-size min-size
                                     :min-occurrences min-occurrences)))
    (format stream "~&Layer target pressure~%")
    (if rows
        (loop for row in rows
              for rank from 1
              do (format stream "~&~D. role=~S gravity=~A count=~D beta1=~D tree-mass=~D~%"
                         rank
                         (getf row :role)
                         (getf row :gravity)
                         (getf row :count)
                         (getf row :beta1)
                         (getf row :tree-mass))
                 (%print-layer-collapse-sample
                  stream
                  (%profile-sample-summary index (getf row :sample))
                  sexp-limit))
        (format stream "  none at current thresholds~%"))))

(defun print-dag-knots
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) actionable-only (sexp-limit 80)
                runtime-costs)
  "Print high-pressure shared sub-DAGs that entangle reuse boundaries."
  (let ((rows (dag-knots index
                         :limit limit
                         :min-size min-size
                         :min-occurrences min-occurrences
                         :actionable-only actionable-only
                         :runtime-costs runtime-costs)))
    (format stream "~&DAG knots~%")
    (if rows
        (loop for row in rows
              for rank from 1
              do (format stream "~&~D. score=~,2F scope=~S libs=~D kinds=~D role=~S gravity=~A beta1=~D tree-mass=~D"
                         rank
                         (getf row :score)
                         (getf row :scope)
                         (getf row :library-width)
                         (getf row :kind-width)
                         (getf row :role)
                         (getf row :gravity)
                         (getf row :beta1)
                         (getf row :tree-mass))
                 (when runtime-costs
                   (format stream " runtime-weight=~,2F"
                           (getf row :runtime-weight))
                   (when (getf row :runtime-label)
                     (format stream " runtime-label=~S"
                             (getf row :runtime-label))))
                 (terpri stream)
                 (%print-layer-collapse-sample stream
                                               (getf row :sample)
                                               sexp-limit))
        (format stream "  none at current thresholds~%"))))

(defun print-layer-collapse-certificate
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) (sexp-limit 80))
  "Print a hierarchy-oriented layer-collapse certificate.

The report is intentionally operational: primitive role pressure says which
semantic layers are asking to become lower-level abstractions, hotspots name
the libraries with the largest remaining graph mass, and implications show
attribute rules that can seed future extraction passes."
  (let* ((certificate (layer-collapse-certificate
                       index
                       :limit limit
                       :min-size min-size
                       :min-occurrences min-occurrences))
         (summary (getf certificate :summary)))
    (format stream "~&Layer collapse certificate~%")
    (format stream "  tree nodes: ~D~%" (getf summary :tree-nodes))
    (format stream "  dag nodes:  ~D~%" (getf summary :dag-nodes))
    (format stream "  beta1:      ~D~%" (getf summary :beta1))
    (format stream "  ratio:      ~,4F~2%" (getf summary :shared-ratio))
    (format stream "Primitive role pressure~%")
    (loop for row in (getf certificate :primitive-roles)
          for rank from 1
          do (format stream "~&~D. role=~S count=~D beta1=~D tree-mass=~D~%"
                     rank
                     (getf row :role)
                     (getf row :count)
                     (getf row :beta1)
                     (getf row :tree-mass))
             (%print-layer-collapse-sample stream
                                           (getf row :sample)
                                           sexp-limit))
    (format stream "~%Layer target pressure~%")
    (if (getf certificate :layer-targets)
        (loop for row in (getf certificate :layer-targets)
              for rank from 1
              do (format stream "~&~D. role=~S gravity=~A count=~D beta1=~D tree-mass=~D~%"
                         rank
                         (getf row :role)
                         (getf row :gravity)
                         (getf row :count)
                         (getf row :beta1)
                         (getf row :tree-mass))
                 (%print-layer-collapse-sample stream
                                               (getf row :sample)
                                               sexp-limit))
        (format stream "  none at current thresholds~%"))
    (format stream "~%Actionable library hotspots~%")
    (if (getf certificate :hotspots)
        (loop for row in (getf certificate :hotspots)
              for rank from 1
              do (format stream "~&~D. ~A rows=~D beta1=~D tree-mass=~D role=~S gravity=~A~%"
                         rank
                         (getf row :library)
                         (getf row :rows)
                         (getf row :beta1)
                         (getf row :tree-mass)
                         (getf row :top-role)
                         (getf row :top-gravity))
                 (%print-layer-collapse-sample stream
                                               (getf row :sample)
                                               sexp-limit))
        (format stream "  none at current thresholds~%"))
    (format stream "~%Concept implications~%")
    (if (getf certificate :implications)
        (loop for rule in (getf certificate :implications)
              for rank from 1
              do (format stream "~&~D. ~A -> ~A support=~D confidence=~,3F~%"
                         rank
                         (%format-concept-attribute (getf rule :antecedent))
                         (%format-concept-attribute (getf rule :consequent))
                         (getf rule :support)
                         (float (getf rule :confidence)))
                 (%print-layer-collapse-sample stream
                                               (getf rule :sample)
                                               sexp-limit))
        (format stream "  none at current thresholds~%"))
    certificate))

(defun %print-leap-section (stream title rows empty-printer row-printer)
  (format stream "~%~A~%" title)
  (if rows
      (loop for row in rows
            for rank from 1
            do (funcall row-printer rank row))
      (funcall empty-printer)))

(defun print-simplification-leaps
    (index &key (stream *standard-output*) (limit 5) (min-size 8)
                (min-occurrences 2) (sexp-limit 80))
  "Print the five high-leverage sexpr simplification reports."
  (let* ((report (simplification-leaps
                  index
                  :limit limit
                  :min-size min-size
                  :min-occurrences min-occurrences))
         (summary (getf report :summary))
         (empty (lambda () (format stream "  none at current thresholds~%"))))
    (format stream "~&Five sexpr simplification leaps~%")
    (format stream "  tree nodes: ~D~%" (getf summary :tree-nodes))
    (format stream "  dag nodes:  ~D~%" (getf summary :dag-nodes))
    (format stream "  beta1:      ~D~%" (getf summary :beta1))
    (format stream "  ratio:      ~,4F~%" (getf summary :shared-ratio))
    (%print-leap-section
     stream
     "1. Extract concrete helpers"
     (getf report :extractions)
     empty
     (lambda (rank row)
       (let ((sample (getf row :sample)))
         (format stream "~&~D. ~A params=~S size=~D occurrences=~D beta1=~D~%"
                 rank
                 (getf row :name)
                 (getf row :parameters)
                 (getf sample :size)
                 (getf sample :occurrences)
                 (getf sample :beta1))
         (format stream "   call=~S~%" (getf row :call-template)))))
    (%print-leap-section
     stream
     "2. Collapse structural isomorphisms"
     (getf report :isomorphisms)
     empty
     (lambda (rank row)
       (format stream "~&~D. count=~D beta1=~D scope=~S libs=~A~%"
               rank
               (getf row :count)
               (getf row :beta1)
               (getf row :scope)
               (%library-summary-string (getf row :libraries)))
       (%print-layer-collapse-sample stream (getf row :sample) sexp-limit)))
    (%print-leap-section
     stream
     "3. Cut dependency cones"
     (getf report :dependency-cones)
     empty
     (lambda (rank row)
       (format stream "~&~D. width=~D libs=~{~A~^,~} rows=~D beta1=~D gravity=~A~%"
               rank
               (getf row :width)
               (getf row :libraries)
               (getf row :rows)
               (getf row :beta1)
               (getf row :top-gravity))
       (%print-layer-collapse-sample stream (getf row :sample) sexp-limit)))
    (%print-leap-section
     stream
     "4. Promote lower-layer targets"
     (getf report :layer-targets)
     empty
     (lambda (rank row)
       (format stream "~&~D. role=~S gravity=~A count=~D beta1=~D~%"
               rank
               (getf row :role)
               (getf row :gravity)
               (getf row :count)
               (getf row :beta1))
       (%print-layer-collapse-sample stream (getf row :sample) sexp-limit)))
    (%print-leap-section
     stream
     "5. Turn implications into hierarchy rules"
     (getf report :implications)
     empty
     (lambda (rank row)
       (format stream "~&~D. ~A -> ~A support=~D confidence=~,3F~%"
               rank
               (%format-concept-attribute (getf row :antecedent))
               (%format-concept-attribute (getf row :consequent))
               (getf row :support)
               (float (getf row :confidence)))
       (%print-layer-collapse-sample stream (getf row :sample) sexp-limit)))
    report))
