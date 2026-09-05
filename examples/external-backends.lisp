#!/usr/bin/env -S sbcl --script
;;;; Explicit native execution with portable evidence and independent replay.
(require :asdf)
(require :uiop)
(defparameter *root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname (truename *load-truename*))))
(asdf:initialize-output-translations
 '(:output-translations (t "/tmp/rosette-example-cache/") :ignore-inherited-configuration))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,*root*) :ignore-inherited-configuration))
(let ((*standard-output* *error-output*)) (asdf:load-system :wire-diagnostics))
(defpackage #:external-example
  (:use #:cl)
  (:local-nicknames (#:d #:rosette-wire-diagnostics) (#:wire #:rosette-wire)
                    (#:json #:rosette-json)))
(in-package #:external-example)

(defun required-env (name)
  (let ((value (uiop:getenv name)))
    (unless (and value (plusp (length value))) (error "Set ~A explicitly." name))
    value))

(defun main ()
  (let* ((args (uiop:command-line-arguments)) (backend (first args)))
    (unless (and (= 2 (length args)) (member backend '("eshkol" "moonlab") :test #'equal))
      (error "usage: external-backends.lisp {eshkol|moonlab} OUTPUT-DIRECTORY"))
    (let* ((directory (uiop:ensure-directory-pathname (merge-pathnames (second args))))
           (moonlab-p (equal backend "moonlab"))
           (command (list (required-env (if moonlab-p "ROSETTE_MOONLAB_PROBE" "ROSETTE_ESHKOL_BIN"))))
           (implementation (required-env (if moonlab-p "ROSETTE_MOONLAB_ID" "ROSETTE_ESHKOL_ID")))
           (library (when moonlab-p (required-env "ROSETTE_MOONLAB_LIB")))
           (encode (if moonlab-p #'d:moonlab-diagnostic-bundle->wire-value #'d:eshkol-diagnostic-bundle->wire-value))
           (decode (if moonlab-p #'d:moonlab-diagnostic-bundle-from-wire-value #'d:eshkol-diagnostic-bundle-from-wire-value))
           (verify (if moonlab-p #'d:verify-moonlab-diagnostic-bundle #'d:verify-diagnostic-bundle))
           (bundle
             (if moonlab-p
                 ;; Two-qubit interferometer, including rotations and entanglement.
                 (d:run-moonlab-campaign
                  (d:make-moonlab-campaign
                   '(:qubits 2 :basis-index 1 :gates (("h" 0) ("ry" 0.37d0 1) ("cnot" 0 1) ("rz" -0.22d0 0)))
                   :implementation-id implementation)
                  :moonlab-command command :moonlab-library library)
                 ;; Integer least-squares objective for y=2*x+1, x=0..4.
                 ;; Candidate y=x+1 has sum of squared residuals 30.
                 (d:run-eshkol-campaign
                  (d:make-eshkol-campaign
                   '((defun residual (x) (- (+ (* 2 x) 1) (+ x 1)))
                     (defun loss (n) (if (= n 0) 0 (+ (* (residual n) (residual n)) (loss (- n 1)))))
                     (main (loss 4))) :toolchain-id implementation)
                  :eshkol-command command))))
      (ensure-directories-exist (merge-pathnames "bundle.json" directory))
      (when (uiop:subpathp (truename directory) cl-user::*root*)
        (error "Keep evidence outside the source checkout."))
      (let ((path (merge-pathnames (concatenate 'string backend "-bundle.json") directory)))
        (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-line (wire:canonical-json (funcall encode bundle)) out))
        ;; Decode the on-disk transfer, not the original in-memory object.
        (let ((loaded (funcall decode (json:json-parse (uiop:read-file-string path)))))
          (assert (funcall verify loaded))
          (assert (eq :pass (if moonlab-p (d:moonlab-diagnostic-bundle-verdict loaded) (d:diagnostic-bundle-verdict loaded))))
          (multiple-value-bind (fresh identical-p)
              (if moonlab-p
                  (d:replay-moonlab-diagnostic-bundle loaded :moonlab-command command :moonlab-library library)
                  (d:replay-diagnostic-bundle loaded :eshkol-command command))
            (assert (and identical-p (funcall verify fresh))))
          ;; A forged schema must be rejected after the same public transfer.
          (let ((mutant (funcall encode loaded)))
            (setf (cdr (assoc "schema" mutant :test #'string=)) "forged")
            (assert (not (handler-case (funcall verify (funcall decode mutant)) (error () nil)))))))
      (format t "~A: PASS (native execution, saved evidence, native replay, tamper refusal)~%" backend))))
(handler-case (main)
  (error (condition) (format *error-output* "~&External execution refused: ~A~%" condition) (uiop:quit 1)))
