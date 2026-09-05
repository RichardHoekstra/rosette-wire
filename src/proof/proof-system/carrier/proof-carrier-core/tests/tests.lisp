;;;; tests.lisp --- tests for rosette-proof-carrier-core.

(defpackage #:rosette-proof-carrier-core/tests
  (:use #:cl #:rosette-proof-carrier-core)
  (:export #:run-all-tests))

(in-package #:rosette-proof-carrier-core/tests)

(defvar *test-count* 0)
(defvar *failure-count* 0)

(defun is (condition label)
  (incf *test-count*)
  (unless condition
    (incf *failure-count*)
    (format *error-output* "~&FAIL: ~A~%" label))
  condition)

(defun run-all-tests ()
  (setf *test-count* 0
        *failure-count* 0)
  (let* ((leaf (proof-leaf :witness :residual :payload 0d0))
         (root (proof-branch :certificate :solve-ok (list leaf)
                             :metadata '(:source :test)))
         (roundtrip (plist->proof-node (proof-node->plist root))))
    (is (proof-node-p leaf) "leaf constructor")
    (is (proof-node-p root) "branch constructor")
    (is (= (proof-node-size root) 2) "size")
    (is (= (proof-node-depth root) 2) "depth")
    (is (equal (proof-node->plist root)
               (proof-node->plist roundtrip))
        "plist roundtrip"))
  (let* ((leaf (proof-leaf :witness :residual))
         (children (list leaf))
         (metadata (list :source :caller))
         (root (make-proof-node :kind :certificate
                                :label :copy
                                :children children
                                :metadata metadata)))
    (setf (first children) (proof-leaf :witness :mutated)
          (getf metadata :source) :mutated)
    (is (eq (first (proof-node-children root)) leaf)
        "children are isolated from caller list mutation")
    (is (equal (proof-node-metadata root) '(:source :caller))
        "metadata is isolated from caller plist mutation")
    (is (equal (getf (proof-node->plist root) :metadata)
               '(:source :caller))
        "serialized metadata is isolated from caller plist mutation"))
  (let ((rejected nil))
    (handler-case
        (make-proof-node :kind :certificate
                         :label :bad
                         :metadata '(:source))
      (error ()
        (setf rejected t)))
    (is rejected "proof node rejects malformed metadata plists"))
  (let* ((source-metadata (list :source :plist))
         (source-child (list :kind :witness
                             :label :child
                             :metadata (list :child :plist)))
         (source-children (list source-child))
         (source-plist (list :kind :certificate
                             :label :decoded
                             :children source-children
                             :metadata source-metadata))
         (decoded (plist->proof-node source-plist)))
    (setf (getf source-metadata :source) :mutated
          (first source-children) nil)
    (is (equal (proof-node-metadata decoded) '(:source :plist))
        "decoded metadata is isolated from source plist mutation")
    (is (= (length (proof-node-children decoded)) 1)
        "decoded children are isolated from source child-list mutation")
    (let ((serialized (proof-node->plist decoded)))
      (setf (getf (getf serialized :metadata) :source) :serialized-mutated)
      (is (equal (proof-node-metadata decoded) '(:source :plist))
          "serialized metadata is isolated from decoded node")))
  (format t "~&rosette-proof-carrier-core: ~D assertions, ~D failures~%"
          *test-count* *failure-count*)
  (when (plusp *failure-count*)
    (error "rosette-proof-carrier-core test failures: ~D" *failure-count*))
  t)
