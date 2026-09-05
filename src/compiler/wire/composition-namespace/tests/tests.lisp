;;;; tests.lisp --- namespace projection and bounded 9P laws.

(defpackage #:composition-namespace/tests
  (:use #:cl #:composition-namespace #:rosette-wire)
  (:import-from #:rosette-assert-core #:assert-true)
  (:local-nicknames (#:crypto #:rosette-symmetric-crypto))
  (:export #:run-all-tests))

(in-package #:composition-namespace/tests)

(defvar *assertions* 0)
(defun check (value message)
  (incf *assertions*) (assert-true value message))
(defun digest (n) (format nil "sha256:~64,'0x" n))
(defun bytes->string (bytes) (map 'string #'code-char bytes))

(defun fixture (&key (node-name "adder") (capabilities nil) (grants nil))
  (let* ((operation
           (make-component-operation
            "add" (list (make-component-field "x" (scalar-type :s64)))
            (scalar-type :s64)))
         (port (make-port "math" (list operation)))
         (descriptor
           (make-component-descriptor
            :name "org.rose/adder" :version "1.0.0" :imports nil
            :exports (list port) :effects '(:pure)
            :capabilities capabilities :adapter '(("kind" . "test"))
            :verifiers nil))
         (node (make-component-node node-name descriptor (digest 1)))
         (step (make-wire-step
                :id "sum" :node-id node-name :port "math" :operation "add"
                :bindings (list (make-data-binding "x" (input-source "x"))))))
    (make-composition
     :name "org.rose/namespace-test" :nodes (list node) :services nil
     :steps (list step)
     :inputs (list (make-component-field "x" (scalar-type :s64)))
     :outputs (list (make-wire-output "result" "sum"))
     :capability-grants grants :required-evidence nil
     :limits '(("maxSteps" . 1) ("maxOutputBytes" . 4096)))))

(defun runner ()
  (let ((runner (make-composition-runner)))
    (register-component-handler
     runner "adder" "math" "add"
     (lambda (arguments services context)
       (declare (ignore services context))
       (1+ (cdr (assoc "x" arguments :test #'string=)))))
    runner))

(defun request (type tag &rest args)
  (apply #'make-ninep-request type tag args))

(defun test-projection ()
  (let* ((composition (fixture))
         (left (project-composition-namespace composition))
         (right (project-composition-namespace composition))
         (endpoint
           (namespace-lookup left "/nodes/adder/exports/math/add/call")))
    (check (string= (composition-namespace-id left)
                    (composition-namespace-id right))
           "projection identity is deterministic")
    (check (verify-composition-namespace left composition)
           "projection reconstructs from the Composition")
    (check (and endpoint (eq :endpoint (namespace-entry-kind endpoint)))
           "exported operation becomes a call endpoint")
    (check (equal '("adder") (namespace-list left "/nodes"))
           "directory listing is deterministic")
    (let ((quoted (project-composition-namespace (fixture :node-name "a/b"))))
      (check (not (namespace-lookup quoted "/nodes/a/b"))
             "a Wire slash cannot escape its pathname segment"))))

(defun test-rosette-backed-ninep-session ()
  (let* ((composition (fixture))
         (namespace (project-composition-namespace composition))
         (session
           (make-ninep-session
            namespace :invoker (make-rosette-composition-invoker
                                composition (runner))
            :max-requests 32 :max-fids 4 :max-msize 4096 :max-io 4096)))
    (check (eq :version
               (ninep-reply-type
                (handle-ninep-request
                 session (request :version 1 :msize 4096 :version "9P2000"))))
           "9P2000 version negotiation succeeds")
    (check (eq :attach
               (ninep-reply-type
                (handle-ninep-request session (request :attach 2 :fid 1))))
           "attach binds a root fid")
    (let ((walk
            (handle-ninep-request
             session
             (request :walk 3 :fid 1 :newfid 2
                      :names '("nodes" "adder" "exports" "math" "add" "call")))))
      (check (= 6 (length (ninep-reply-qids walk)))
             "walk returns one qid per resolved segment"))
    (check (eq :open
               (ninep-reply-type
                (handle-ninep-request
                 session (request :open 4 :fid 2 :mode :read-write))))
           "operation endpoint opens read-write")
    (let* ((payload (crypto:string->bytes
                     (canonical-json '(("x" . 41)))))
           (reply (handle-ninep-request
                   session (request :write 5 :fid 2 :offset 0 :data payload))))
      (check (= (length payload) (ninep-reply-count reply))
             "write is consumed by the explicit Rosette invoker"))
    (let* ((reply (handle-ninep-request
                   session (request :read 6 :fid 2 :offset 0 :count 4096)))
           (text (bytes->string (ninep-reply-data reply))))
      (check (and (search "execution" text) (search "verification" text)
                  (search "pass" text))
             "read returns Rosette execution and verification receipts"))
    (check (eq :clunk
               (ninep-reply-type
                (handle-ninep-request session (request :clunk 7 :fid 2))))
           "clunk releases the fid")))

(defun test-negative-bounds ()
  (let* ((composition (fixture))
         (namespace (project-composition-namespace composition))
         (session (make-ninep-session namespace :max-requests 3
                                      :max-fids 1 :max-io 8 :max-msize 256)))
    (handle-ninep-request
     session (request :version 1 :msize 256 :version "9P2000"))
    (handle-ninep-request session (request :attach 2 :fid 1))
    (let ((bad-walk
            (handle-ninep-request
             session (request :walk 3 :fid 1 :newfid 1 :names '("..")))))
      (check (eq :error (ninep-reply-type bad-walk))
             "parent traversal is refused"))
    (check (eq :error
               (ninep-reply-type
                (handle-ninep-request
                 session (request :read 4 :fid 1 :offset 0 :count 1))))
           "request budget exhaustion is a reply, not an unbounded session"))
  (let ((bad (fixture :capabilities '("clock/monotonic@1") :grants nil)))
    (check (handler-case
               (progn (project-composition-namespace bad) nil)
             (namespace-error () t))
           "Rosette capability refusal prevents namespace projection")))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-projection)
  (test-rosette-backed-ninep-session)
  (test-negative-bounds)
  (format t "composition-namespace: ~D assertions passed~%" *assertions*)
  t)
