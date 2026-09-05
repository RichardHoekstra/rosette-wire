;;;; tests.lisp --- capability, deadlock, IFC, identity, and bound gates.

(defpackage #:composition-sel4/tests
  (:use #:cl #:composition-sel4 #:rosette-wire)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:composition-sel4/tests)

(defvar *assertions* 0)
(defun check (value message)
  (incf *assertions*) (assert-true value message))
(defun digest (n) (format nil "sha256:~64,'0x" n))

(defun unary-port (port-name operation-name input-name)
  (make-port
   port-name
   (list (make-component-operation
          operation-name
          (if input-name
              (list (make-component-field input-name (scalar-type :s64))) nil)
          (scalar-type :s64)))))

(defun data-flow-composition (&key (grants '("compute/consumer@1"
                                              "compute/producer@1")))
  (let* ((producer-port (unary-port "produce" "make" "x"))
         (consumer-port (unary-port "consume" "use" "y"))
         (producer
           (make-component-node
            "producer"
            (make-component-descriptor
             :name "org.rose/producer" :version "1.0.0" :imports nil
             :exports (list producer-port) :effects '(:pure)
             :capabilities '("compute/producer@1")
             :adapter '(("kind" . "test")) :verifiers nil)
            (digest 1)))
         (consumer
           (make-component-node
            "consumer"
            (make-component-descriptor
             :name "org.rose/consumer" :version "1.0.0" :imports nil
             :exports (list consumer-port) :effects '(:pure)
             :capabilities '("compute/consumer@1")
             :adapter '(("kind" . "test")) :verifiers nil)
            (digest 2)))
         (first
           (make-wire-step
            :id "produce" :node-id "producer" :port "produce"
            :operation "make"
            :bindings (list (make-data-binding "x" (input-source "x")))))
         (second
           (make-wire-step
            :id "consume" :node-id "consumer" :port "consume"
            :operation "use"
            :bindings (list (make-data-binding "y" (step-source "produce"))))))
    (make-composition
     :name "org.rose/sel4-flow" :nodes (list producer consumer)
     :services nil :steps (list first second)
     :inputs (list (make-component-field "x" (scalar-type :s64)))
     :outputs (list (make-wire-output "result" "consume"))
     :capability-grants grants :required-evidence nil
     :limits '(("maxSteps" . 2) ("maxOutputBytes" . 1024)))))

(defun service-cycle-composition ()
  (let* ((service (unary-port "svc" "ping" nil))
         (descriptor-a
           (make-component-descriptor
            :name "org.rose/a" :version "1.0.0"
            :imports (list service) :exports (list service)
            :effects '(:pure) :capabilities nil
            :adapter '(("kind" . "test")) :verifiers nil))
         (descriptor-b
           (make-component-descriptor
            :name "org.rose/b" :version "1.0.0"
            :imports (list service) :exports (list service)
            :effects '(:pure) :capabilities nil
            :adapter '(("kind" . "test")) :verifiers nil))
         (a (make-component-node "a" descriptor-a (digest 3)))
         (b (make-component-node "b" descriptor-b (digest 4))))
    (make-composition
     :name "org.rose/sel4-cycle" :nodes (list a b)
     :services
     (list
      (make-service-binding :id "a-to-b" :consumer-node "a"
                            :import-port "svc" :provider-node "b"
                            :provider-port "svc")
      (make-service-binding :id "b-to-a" :consumer-node "b"
                            :import-port "svc" :provider-node "a"
                            :provider-port "svc"))
     :steps nil :inputs nil :outputs nil :capability-grants nil
     :required-evidence nil :limits '(("maxSteps" . 1)))))

(defun test-safe-lowering ()
  (let* ((composition (data-flow-composition))
         (policy
           (make-sel4-lowering-policy
            :node-levels '(("producer" . 0) ("consumer" . 1))))
         (description (lower-composition-to-sel4 composition :policy policy))
         (grants (sel4-authority-description-authority-grants description)))
    (check (sel4-authority-description-safe-p description)
           "rising data flow and acyclic calls are safe")
    (check (= 3 (length
                 (sel4-authority-description-protection-domains description)))
           "Rosette orchestrator plus two Components become three PDs")
    (check (= 2 (length grants))
           "every declared Component capability becomes an authority record")
    (check (equal '(#x1000 #x1001)
                  (mapcar #'authority-grant-endpoint-slot grants))
           "raw endpoint slots delegate to sel4-cap checked arithmetic")
    (check (= 3 (length (sel4-authority-description-channels description)))
           "two orchestrator calls and one data flow remain distinct channels")
    (check (verify-sel4-authority-description description composition)
           "authority description freshly re-lowers and verifies")
    (let* ((parsed
             (rosette-sel4-verify:parse-system-string
              (sel4-authority-description-system-xml description)))
           (verdict (rosette-sel4-verify:referee parsed)))
      (check (rosette-sel4-verify:verdict-deadlock-free verdict)
             "emitted Microkit XML re-parses as deadlock-free"))
    (let ((graft (copy-sel4-authority-description description)))
      (setf (sel4-authority-description-system-xml graft) "<system/>")
      (check (not (verify-sel4-authority-description graft composition))
             "grafted system XML is rejected"))))

(defun test-information-flow-refusal ()
  (let* ((composition (data-flow-composition))
         (policy
           (make-sel4-lowering-policy
            :node-levels '(("producer" . 2) ("consumer" . 0))))
         (description (lower-composition-to-sel4 composition :policy policy)))
    (check (eq :refused (sel4-authority-description-status description))
           "high-to-low Wire data flow refuses activation")
    (check (find :information-flow
                 (sel4-authority-description-violations description)
                 :key #'first)
           "information leak is located")
    (check (verify-sel4-authority-description description composition)
           "a correctly reported unsafe lowering is still replayable evidence")))

(defun test-service-cycle-and-explicit-mode ()
  (let ((composition (service-cycle-composition)))
    (check
     (handler-case
         (progn (lower-composition-to-sel4 composition) nil)
       (composition-sel4-error (condition)
         (eq :undeclared-service-mode
             (composition-sel4-error-code condition))))
     "service IPC cannot silently become synchronous or asynchronous")
    (let* ((policy
             (make-sel4-lowering-policy
              :service-modes '(("a-to-b" . :ppcall)
                               ("b-to-a" . :ppcall))))
           (description
             (lower-composition-to-sel4 composition :policy policy)))
      (check (eq :refused (sel4-authority-description-status description))
             "explicit synchronous service cycle is a deadlock refusal")
      (check (not (rosette-sel4-verify:verdict-deadlock-free
                   (sel4-authority-description-deadlock-verdict description)))
             "sel4-verify, not the lowering, owns the deadlock judgment"))))

(defun test-admission-and-bounds ()
  (check
   (handler-case
       (progn
         (lower-composition-to-sel4
          (data-flow-composition :grants '("compute/producer@1")))
         nil)
     (composition-sel4-error (condition)
       (eq :rosette-refusal (composition-sel4-error-code condition))))
   "missing Rosette capability grant blocks lowering")
  (check
   (handler-case
       (progn
         (lower-composition-to-sel4
          (data-flow-composition)
          :policy (make-sel4-lowering-policy :max-pds 2))
         nil)
     (composition-sel4-error (condition)
       (eq :pd-budget (composition-sel4-error-code condition))))
   "protection-domain ceiling is enforced"))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-safe-lowering)
  (test-information-flow-refusal)
  (test-service-cycle-and-explicit-mode)
  (test-admission-and-bounds)
  (format t "composition-sel4: ~D assertions passed~%" *assertions*)
  t)
