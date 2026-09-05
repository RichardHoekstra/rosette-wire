;;;; core.lisp --- one surface program, one ProgramIR view, one checked route.

(in-package #:rosette-front-door)

(define-condition front-door-refusal (error)
  ((stage :initarg :stage :reader front-door-refusal-stage)
   (input :initarg :input :reader front-door-refusal-input)
   (reason :initarg :reason :reader front-door-refusal-reason))
  (:report (lambda (condition stream)
             (format stream "rosette-front-door refused ~A at ~A: ~S"
                     (front-door-refusal-reason condition)
                     (front-door-refusal-stage condition)
                     (front-door-refusal-input condition)))))

(defun %refuse (stage input control &rest arguments)
  (error 'front-door-refusal
         :stage stage
         :input input
         :reason (apply #'format nil control arguments)))

(defun %symbol-name= (object name)
  (and (symbolp object) (string= (symbol-name object) name)))

(defun %proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %name-member-p (name names)
  (member (symbol-name name) names :test #'string=))

(defun %parse-surface (surface)
  "Validate SURFACE and return its single canonical CORE-PROGRAM.

The surface contains zero or more (DEFUN NAME (ARGS...) BODY) forms and
exactly one (MAIN TERM). Symbol names, rather than package identity, determine
the two form heads, matching rosette-core-term's operator discipline."
  (unless (%proper-list-p surface)
    (%refuse :parse surface "the surface must be a proper list of forms"))
  (let ((functions '())
        (function-names '())
        (main nil)
        (main-seen-p nil))
    (dolist (form surface)
      (unless (and (%proper-list-p form) (consp form) (symbolp (first form)))
        (%refuse :parse form "each program form must be a headed proper list"))
      (cond
        ((%symbol-name= (first form) "DEFUN")
         (unless (= (length form) 4)
           (%refuse :parse form "DEFUN wants NAME, an argument list, and BODY"))
         (destructuring-bind (head name arguments body) form
           (declare (ignore head))
           (unless (symbolp name)
             (%refuse :parse form "a function name must be a symbol"))
           (unless (and (%proper-list-p arguments)
                        (every #'symbolp arguments))
             (%refuse :parse form "function arguments must be a proper symbol list"))
           (when (/= (length arguments)
                     (length (remove-duplicates arguments
                                                :key #'symbol-name
                                                :test #'string=)))
             (%refuse :parse form "function argument names must be unique"))
           (when (%name-member-p name function-names)
             (%refuse :parse form "function ~A is defined more than once" name))
           (push (symbol-name name) function-names)
           (push (list name (copy-list arguments) (copy-tree body)) functions)))
        ((%symbol-name= (first form) "MAIN")
         (unless (= (length form) 2)
           (%refuse :parse form "MAIN wants exactly one term"))
         (when main-seen-p
           (%refuse :parse form "the program must have exactly one MAIN"))
         (setf main-seen-p t
               main (copy-tree (second form))))
        (t
         (%refuse :parse form "unknown surface head ~A" (first form)))))
    (unless main-seen-p
      (%refuse :parse surface "the program has no MAIN"))
    (let ((program (rosette-core-term:make-core-program
                    :funs (nreverse functions)
                    :main main)))
      ;; The parser's success means semantic typing succeeded, not merely that
      ;; the outer list had the right punctuation.
      (handler-case (rosette-core-term:term-check program)
        (error (condition)
          (%refuse :typecheck surface "core-term rejected it: ~A" condition)))
      program)))

(defun core-program->surface (program)
  "Project a CORE-PROGRAM to the canonical front-door surface form."
  (unless (rosette-core-term:core-program-p program)
    (%refuse :represent program "expected an rosette-core-term CORE-PROGRAM"))
  (rosette-core-term:term-check program)
  (append
   (loop for (name arguments body) in (rosette-core-term:core-program-funs program)
         collect (list 'defun name (copy-list arguments) (copy-tree body)))
   (list (list 'main (copy-tree (rosette-core-term:core-program-main program))))))

(defun program-ir->surface (program-ir)
  "Project PROGRAM-IR back to its validated surface program."
  (unless (rosette-program-ir:programp program-ir)
    (%refuse :represent program-ir "expected an rosette-program-ir PROGRAM"))
  (let ((surface (rosette-program-ir:program->lisp program-ir)))
    (%parse-surface surface)
    (copy-tree surface)))

(defun surface->program-ir (surface)
  "Lift SURFACE to ProgramIR after parsing and semantic type-checking it."
  (%parse-surface surface)
  (rosette-program-ir:lisp->program (copy-tree surface)))

(defun parse (source)
  "Parse SOURCE into the one rosette-core-term CORE-PROGRAM.

SOURCE may be a surface list, the ProgramIR view of such a list, or an already
constructed CORE-PROGRAM. All three paths perform core semantic type-checking."
  (cond
    ((rosette-core-term:core-program-p source)
     (rosette-core-term:term-check source)
     source)
    ((rosette-program-ir:programp source)
     (%parse-surface (program-ir->surface source)))
    (t (%parse-surface source))))

(defun eval (source)
  "Evaluate SOURCE through rosette-core-term's direct semantics."
  (rosette-core-term:program-eval (parse source)))

(defun lower (source)
  "Lower SOURCE through rosette-core-term to the shared kernel-VM program."
  (rosette-core-term:lower-to-program (parse source)))

(defun gate (source)
  "Compare direct evaluation with the CPU oracle of the lowered program.

Returns three values: PASSED-P, DIRECT-VALUE, ORACLE-VALUE."
  (let* ((program (parse source))
         (direct (rosette-core-term:program-eval program))
         (lowered (rosette-core-term:lower-to-program program))
         (oracle (rosette-core-term:oracle-run lowered)))
    (values (eql direct oracle) direct oracle)))

(defun %canonical-program-ir-form (surface)
  (rosette-program-ir:program->lisp
   (rosette-program-ir:canonical-form (surface->program-ir surface))))

(defun %verify-front-door-certificate (certificate)
  (handler-case
      (let* ((payload (rosette-proof-witness:certificate-payload certificate))
             (surface (getf payload :surface))
             (program (%parse-surface surface))
             (direct (rosette-core-term:program-eval program))
             (lowered (rosette-core-term:lower-to-program program))
             (oracle (rosette-core-term:oracle-run lowered)))
        (and (eql direct (getf payload :eval-value))
             (eql oracle (getf payload :oracle-value))
             (eql direct oracle)
             (equal (%canonical-program-ir-form surface)
                    (getf payload :program-ir))))
    (error () nil)))

(defun %seal-gate (program program-ir direct oracle)
  (let* ((surface (core-program->surface program))
         (canonical-ir (rosette-program-ir:program->lisp
                        (rosette-program-ir:canonical-form program-ir)))
         (roundtrip-p
           (equal surface (program-ir->surface program-ir)))
         (certificate
           (rosette-proof-witness:make-certificate
            :name :front-door-oracle-gate
            :kind :proof
            :claim :one-program-agreement
            :payload (list :surface (copy-tree surface)
                           :program-ir (copy-tree canonical-ir)
                           :eval-value direct
                           :oracle-value oracle
                           :program-ir-roundtrip-p roundtrip-p)
            :passed (and roundtrip-p (eql direct oracle))
            :metadata '(:representations (:surface :program-ir :kernel-vm)
                        :independent-readouts (:core-eval :cpu-oracle)))))
    (setf (rosette-proof-witness:certificate-witness certificate)
          (rosette-proof-witness:make-lean-witness
           :front-door-gate-re-runs
           (lambda () (%verify-front-door-certificate certificate))))
    certificate))

(defun certify (source)
  "Run the route and seal its agreement in a re-runnable certificate."
  (let* ((program (parse source))
         (program-ir (surface->program-ir (core-program->surface program)))
         (direct (rosette-core-term:program-eval program))
         (lowered (rosette-core-term:lower-to-program program))
         (oracle (rosette-core-term:oracle-run lowered)))
    (%seal-gate program program-ir direct oracle)))

(defstruct (front-door-result
            (:constructor %make-front-door-result
                (&key program program-ir eval-value lowered-program
                      oracle-value gate-passed certificate)))
  "All readouts of one admitted front-door source."
  program
  program-ir
  eval-value
  lowered-program
  oracle-value
  (gate-passed nil :type boolean)
  certificate)

(defun run (source)
  "Parse, represent, evaluate, lower, oracle-gate, and certify SOURCE.

Returns a FRONT-DOOR-RESULT. A false gate remains visible in the result and in
its certificate; no backend result is silently promoted to truth."
  (let* ((program (parse source))
         (surface (core-program->surface program))
         (program-ir (surface->program-ir surface))
         (direct (rosette-core-term:program-eval program))
         (lowered (rosette-core-term:lower-to-program program))
         (oracle (rosette-core-term:oracle-run lowered))
         (passed (eql direct oracle))
         (certificate (%seal-gate program program-ir direct oracle)))
    (%make-front-door-result
     :program program
     :program-ir program-ir
     :eval-value direct
     :lowered-program lowered
     :oracle-value oracle
     :gate-passed passed
     :certificate certificate)))
