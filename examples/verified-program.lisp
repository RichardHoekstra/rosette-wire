#!/usr/bin/env -S sbcl --script
;;;; A real computation, an independent verifier, and a deliberate wrong answer.
(require :asdf)
(require :uiop)
(defparameter *root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname (truename *load-truename*))))
(asdf:initialize-output-translations
 '(:output-translations (t "/tmp/rosette-example-cache/")
   :ignore-inherited-configuration))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,*root*) :ignore-inherited-configuration))
(let ((*standard-output* *error-output*))
  (asdf:load-system :wire-graph)
  (asdf:load-system :front-door))

(defpackage #:public-example
  (:use #:cl)
  (:local-nicknames (#:wire #:rosette-wire) (#:front #:rosette-front-door)))
(in-package #:public-example)

(defparameter *source*
  '((defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))
    (main (fact 6))))

(defun composition ()
  (let* ((descriptor
           (wire:make-component-descriptor
            :name "org.rosette/factorial-example" :version "1.0.0"
            :imports nil
            :exports (list (wire:make-port
                            "program" (list (wire:make-component-operation
                                             "evaluate" nil
                                             (wire:scalar-type :s64)))))
            :effects '(:pure) :capabilities nil
            :adapter '(("kind" . "front-door"))
            :verifiers '("factorial-oracle/v1")))
         (implementation-id
           (wire:canonical-id
            (list (cons "source" (write-to-string *source* :readably t))))))
    (wire:make-composition
     :name "org.rosette/verified-factorial"
     :nodes (list (wire:make-component-node "program" descriptor implementation-id))
     :services nil :inputs nil
     :steps (list (wire:make-wire-step :id "evaluate" :node-id "program"
                                      :port "program" :operation "evaluate"
                                      :bindings nil))
     :outputs (list (wire:make-wire-output "answer" "evaluate"))
     :capability-grants nil :required-evidence '("factorial-oracle/v1")
     :limits '(("maxSteps" . 1) ("maxOutputBytes" . 4096)))))

(defun runner (&key mutant)
  (let ((runner (wire:make-composition-runner)))
    (wire:register-component-handler
     runner "program" "program" "evaluate"
     (lambda (arguments services context)
       (declare (ignore arguments services context))
       (let ((answer (front:front-door-result-eval-value (front:run *source*))))
         (if mutant (1+ answer) answer))))
    (wire:register-component-verifier
     runner "factorial-oracle/v1"
     (lambda (graph receipt)
       ;; Bind the trusted verifier to this exact program/contract, then rerun.
       ;; Neither a stored pass flag nor the producer's answer is an oracle.
       (multiple-value-bind (agrees direct oracle) (front:gate *source*)
         (let ((actual (cdr (assoc "answer" (wire:wire-receipt-outputs receipt)
                                   :test #'string=))))
           (values
            (and (string= (wire:composition-id graph)
                          (wire:composition-id (composition)))
                 (eq :pass (wire:wire-receipt-verdict receipt))
                 agrees (eql direct 720) (eql actual oracle))
            (list (cons "direct" direct) (cons "kernel" oracle)))))))
    runner))

(defun write-json (path value)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-line (wire:canonical-json value) out)))

(defun output-directory (argument)
  (unless argument (error "This command requires an output directory."))
  (let ((path (uiop:ensure-directory-pathname (merge-pathnames argument))))
    (ensure-directories-exist (merge-pathnames "receipt.json" path))
    (let ((resolved (truename path)))
      (when (uiop:subpathp resolved cl-user::*root*)
        (error "Keep generated evidence outside the source checkout."))
      resolved)))

(defun replay (directory)
  (let* ((graph (wire:wire-graph-from-json
                 (uiop:read-file-string (merge-pathnames "composition.json" directory))))
         (receipt (wire:wire-receipt-from-json
                   (uiop:read-file-string (merge-pathnames "receipt.json" directory))))
         (verification (wire:verify-composition-receipt graph receipt (runner))))
    (values verification graph receipt)))

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (mode (first args)))
    (unless (and (member mode '("run" "mutate" "replay" "studio" "eshkol")
                         :test #'equal)
                 (= (length args) (if (equal mode "eshkol") 1 2)))
      (format *error-output*
              "usage: verified-program.lisp {run|mutate|replay|studio} DIRECTORY | eshkol~%")
      (return-from main 2))
    (when (equal mode "eshkol")
      (asdf:load-system :front-door/eshkol)
      (let* ((report (uiop:symbol-call :rosette-front-door/eshkol :gate-eshkol
                                      (front:surface->program-ir *source*)))
             (passed (uiop:symbol-call :rosette-front-door/eshkol
                                      :eshkol-gate-report-passed report)))
        (format t "Eshkol direct=VM=JIT=AOT: ~A~%" (if passed "pass" "fail"))
        (return-from main (if passed 0 1))))
    (let ((directory (output-directory (second args))))
      (when (member mode '("run" "mutate") :test #'equal)
        (let* ((graph (composition))
               (receipt (wire:run-composition graph
                          (runner :mutant (equal mode "mutate")) nil)))
          (write-json (merge-pathnames "composition.json" directory)
                      (wire:composition->value graph))
          (write-json (merge-pathnames "receipt.json" directory)
                      (wire:wire-receipt->value receipt))))
      (multiple-value-bind (verification graph receipt) (replay directory)
        (write-json (merge-pathnames "verification.json" directory)
                    (wire:wire-receipt->value verification))
        (let ((passed (eq :pass (wire:wire-receipt-verdict verification))))
          (when (and passed (equal mode "studio"))
            (unless (asdf:find-system :rose-studio nil)
              (error "Studio is provided by the Rose Workbench export."))
            (asdf:load-system :rose-studio)
            (with-open-file (out (merge-pathnames "studio.html" directory)
                                 :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
              (write-string (uiop:symbol-call :rose-studio :render-rose-studio
                                              graph :receipt receipt :verification verification) out)))
          (write-line (wire:canonical-json (wire:wire-receipt->value verification)))
          (if passed 0 1))))))

(handler-case (uiop:quit (main))
  (error (condition)
    (format *error-output* "verified-program: ~A~%" condition)
    (uiop:quit 2)))
