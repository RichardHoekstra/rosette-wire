;;;; port-equivalence/tests/tests.lisp --- Test suite.

(defpackage #:port-equivalence/tests
  (:use #:cl #:port-equivalence)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-wire
                #:scalar-type #:make-component-field
                #:make-component-operation #:make-port)
  (:export #:run-all-tests))

(in-package #:port-equivalence/tests)

(defvar *assertions* 0)

(defun check (condition control &rest arguments)
  (incf *assertions*)
  (assert-true condition (apply #'format nil control arguments)))

(defun refusal-code (thunk)
  (handler-case (progn (funcall thunk) nil)
    (port-equivalence-refusal (condition)
      (port-equivalence-refusal-code condition))))

(defun colour-certificate ()
  (let ((string (scalar-type :string)))
    (certify-type-equivalence
     :name "lowercase-colour/uppercase-colour"
     :source-wire-type string :target-wire-type string
     :forward (lambda (value) (string-upcase value))
     :backward (lambda (value) (string-downcase value))
     :source-samples '("blue" "green" "red")
     :target-samples '("BLUE" "GREEN" "RED"))))

(defun colour-ports (certificate)
  (let* ((string (scalar-type :string))
         (source-operation
           (make-component-operation
            "rotate" (list (make-component-field "colour" string)) string))
         (target-operation
           (make-component-operation
            "cycle" (list (make-component-field "COLOR" string)) string))
         (source-port (make-port "lowercase" (list source-operation)))
         (target-port (make-port "uppercase" (list target-operation)))
         (mapping
           (make-operation-equivalence
            :source-operation "rotate" :target-operation "cycle"
            :inputs (list (make-input-equivalence
                           "colour" "COLOR" certificate))
            :output certificate)))
    (values source-port target-port mapping)))

(defun rotate-colour (arguments services context)
  (declare (ignore services context))
  (let ((value (cdr (assoc "colour" arguments :test #'string=))))
    (cdr (assoc value '(("red" . "green")
                        ("green" . "blue")
                        ("blue" . "red"))
                :test #'string=))))

(defun rotate-colour-spec (arguments services context)
  (declare (ignore services context))
  (let ((value (cdr (assoc "colour" arguments :test #'string=))))
    (cond ((string= value "red") "green")
          ((string= value "green") "blue")
          ((string= value "blue") "red"))))

(defun wrong-colour-spec (arguments services context)
  (declare (ignore services context))
  (cdr (assoc "colour" arguments :test #'string=)))

(defun test-type-equivalence ()
  (let ((certificate (colour-certificate)))
    (check (verify-type-equivalence-certificate certificate)
           "type certificate did not replay")
    (check (string= "GREEN"
                    (transport-value-forward certificate "green"))
           "forward cubical transport was wrong")
    (check (string= "red"
                    (transport-value-backward certificate "RED"))
           "inverse cubical transport was wrong")
    (check (eq :not-an-equivalence
               (refusal-code
                (lambda ()
                  (certify-type-equivalence
                   :name "collapse" :source-wire-type (scalar-type :string)
                   :target-wire-type (scalar-type :string)
                   :forward (constantly "X") :backward (constantly "red")
                   :source-samples '("red" "green")
                   :target-samples '("X" "Y")))))
           "non-equivalence received a certificate")))

(defun test-port-transport ()
  (let ((type-certificate (colour-certificate)))
    (multiple-value-bind (source target mapping)
        (colour-ports type-certificate)
      (let* ((certificate
               (certify-port-equivalence source target (list mapping)))
             (handler
               (transport-operation-handler certificate "cycle"
                                            #'rotate-colour)))
        (check (verify-port-equivalence-certificate certificate)
               "Port certificate did not replay")
        (check (string= "GREEN"
                        (funcall handler '(("COLOR" . "RED")) nil nil))
               "transported handler did not conjugate the source operation")
        (check (eq :missing-argument
                   (refusal-code
                    (lambda () (funcall handler nil nil nil))))
               "transported handler accepted a missing target argument")
        (check (eq :empty-port-map
                   (refusal-code
                    (lambda ()
                      (certify-port-equivalence source target nil))))
               "incomplete Port map was accepted")))))

(defun test-proof-transport ()
  (let ((type-certificate (colour-certificate)))
    (multiple-value-bind (source target mapping)
        (colour-ports type-certificate)
      (let* ((certificate
               (certify-port-equivalence source target (list mapping)))
             (receipt
               (certify-transported-correctness
                certificate "cycle" #'rotate-colour #'rotate-colour-spec
                :implementation-id "sha256:implementation"
                :specification-id "sha256:specification")))
        (check (verify-transported-correctness
                certificate "cycle" #'rotate-colour #'rotate-colour-spec receipt
                :implementation-id "sha256:implementation"
                :specification-id "sha256:specification")
               "transported correctness receipt did not replay")
        (check (not (verify-transported-correctness
                     certificate "cycle" #'rotate-colour #'rotate-colour-spec
                     receipt :implementation-id "sha256:grafted"
                     :specification-id "sha256:specification"))
               "proof receipt grafted onto a different implementation")
        (check (eq :source-proof-failed
                   (refusal-code
                    (lambda ()
                      (certify-transported-correctness
                       certificate "cycle" #'rotate-colour #'wrong-colour-spec
                       :implementation-id "sha256:implementation"
                       :specification-id "sha256:wrong"))))
               "false source theorem transported")))))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-type-equivalence)
  (test-port-transport)
  (test-proof-transport)
  (format t "port-equivalence: ~D assertions passed~%" *assertions*)
  t)
