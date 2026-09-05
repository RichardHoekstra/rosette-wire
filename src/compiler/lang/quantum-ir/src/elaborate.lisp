;;;; elaborate.lisp --- compatibility seam and public admission entry point.

(in-package #:rosette-quantum-ir)

(defun elaborate-legacy-qprogram (legacy)
  "Elaborate the old closed unitary QPROGRAM tape into the quantum dialect.

The accepted legacy source is
  (:QPROGRAM N (:PREPARE S INDEX) (:GATE OUT IN GATE TARGET...) ...
               (:RETURN S)).
The result is a fresh ProgramIR carrier containing one closed :PROGRAM.
Malformed legacy statements are preserved where possible so CHECK-PROGRAM can
return ordinary diagnostics instead of turning elaboration into admission."
  (let ((form (if (rosette-program-ir:programp legacy)
                  (rosette-program-ir:program->lisp legacy)
                  legacy)))
    (unless (and (%proper-list-p form) (eq (first form) :qprogram))
      (error "Expected legacy (:QPROGRAM ...), got ~S" form))
    (let ((n (second form)))
      (make-quantum-program
       (list :quantum-unit
             (append
              (list :program :legacy (list (list :qstate n)))
              (loop for instruction in (cddr form)
                    collect
                    (cond
                      ((and (%proper-list-p instruction)
                            (eq (first instruction) :prepare)
                            (= (length instruction) 3))
                       (list :prepare (second instruction) n (third instruction)))
                      ((and (%proper-list-p instruction)
                            (eq (first instruction) :gate))
                       (cons :apply (rest instruction)))
                      (t (copy-tree instruction))))))))))

(defun %invalid-carrier-report (control &rest arguments)
  (%make-program-report
   :ok-p nil :kind :invalid :definitions nil :programs nil
   :errors (list (%make-diagnostic :kind :syntax :path nil
                                   :message (apply #'format nil control arguments)))))

(defun check-program (program)
  "Admit PROGRAM without signalling for source errors.

PROGRAM must be the shared rosette-program-ir carrier.  Native :QUANTUM-UNIT forms
are checked directly.  Legacy :QPROGRAM forms pass through the explicit,
lossless compatibility elaborator first.  Measurement is syntax for a labelled
instrument; this checker never samples or chooses a branch."
  (cond
    ((not (rosette-program-ir:programp program))
     (%invalid-carrier-report "not an rosette-program-ir PROGRAM: ~S" program))
    (t
     (handler-case
         (let ((form (rosette-program-ir:program->lisp program)))
           (case (and (consp form) (first form))
             (:quantum-unit (%check-quantum-unit program))
             (:qprogram
              (%check-quantum-unit (elaborate-legacy-qprogram program)
                                   :kind-override :legacy-program))
             (otherwise
              (%invalid-carrier-report
               "ProgramIR root must be :QUANTUM-UNIT or :QPROGRAM, got ~S"
               (and (consp form) (first form))))))
       (error (condition)
         (%invalid-carrier-report "quantum IR admission failed safely: ~A" condition))))))

(defun program-valid-p (program)
  "True exactly when CHECK-PROGRAM returns a green report."
  (program-report-ok-p (check-program program)))
