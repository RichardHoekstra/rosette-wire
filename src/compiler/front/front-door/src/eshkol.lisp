;;;; eshkol.lisp --- ProgramIR/front-door integer dialect -> Eshkol backend.
;;;;
;;;; ProgramIR is deliberately a semantics-free carrier.  This backend accepts
;;;; only the dialect admitted by ROSETTE-FRONT-DOOR:PARSE, then emits Eshkol from
;;;; the resulting CORE-PROGRAM.  It does not claim that arbitrary ProgramIR
;;;; trees are Eshkol programs.

(defpackage #:rosette-front-door/eshkol
  (:use #:cl)
  (:local-nicknames (#:cid #:rosette-content-identity)
                    (#:core #:rosette-core-term)
                    (#:envelope #:rosette-tool-envelope)
                    (#:front #:rosette-front-door)
                    (#:pir #:rosette-program-ir)
                    (#:worker #:rosette-isolated-worker))
  (:export
   #:emit-eshkol-source
   #:eshkol-available-p
   #:gate-eshkol
   #:eshkol-gate-report
   #:eshkol-gate-report-p
   #:eshkol-gate-report-program-id
   #:eshkol-gate-report-toolchain-command
   #:eshkol-gate-report-direct-value
   #:eshkol-gate-report-kernel-value
   #:eshkol-gate-report-jit-value
   #:eshkol-gate-report-aot-value
   #:eshkol-gate-report-jit-envelope
   #:eshkol-gate-report-aot-compile-envelope
   #:eshkol-gate-report-aot-envelope
   #:eshkol-gate-report-passed
   #:eshkol-backend-error
   #:eshkol-backend-error-stage
   #:eshkol-backend-error-status
   #:eshkol-backend-error-envelope
   #:eshkol-backend-error-detail
   #:eshkol-unavailable))

(in-package #:rosette-front-door/eshkol)

(defparameter +receipt-marker+ "ROSETTE-FRONT-DOOR-ESHKOL-RESULT ")
(defconstant +receipt-schema+ :rosette-front-door-eshkol-result/v1)

(define-condition eshkol-backend-error (error)
  ((stage :initarg :stage :reader eshkol-backend-error-stage)
   (status :initarg :status :reader eshkol-backend-error-status
           :initform :error)
   (envelope :initarg :envelope :reader eshkol-backend-error-envelope
             :initform nil)
   (detail :initarg :detail :reader eshkol-backend-error-detail
           :initform nil))
  (:report (lambda (condition stream)
             (format stream "Eshkol backend failed at ~A (~A)~@[ — ~A~]."
                     (eshkol-backend-error-stage condition)
                     (eshkol-backend-error-status condition)
                     (eshkol-backend-error-detail condition)))))

(define-condition eshkol-unavailable (eshkol-backend-error) ()
  (:documentation "The configured Eshkol compiler could not run."))

(defstruct (eshkol-gate-report
            (:constructor %make-eshkol-gate-report
                (&key program-id toolchain-command direct-value kernel-value
                      jit-value aot-value jit-envelope aot-compile-envelope
                      aot-envelope passed)))
  "One ephemeral direct/kernel/JIT/AOT agreement observation."
  program-id
  toolchain-command
  direct-value
  kernel-value
  jit-value
  aot-value
  jit-envelope
  aot-compile-envelope
  aot-envelope
  (passed nil :type boolean))

(defun %name= (object name)
  (and (symbolp object) (string= (symbol-name object) name)))

(defun %mangle-identifier (symbol namespace)
  "Map a CL symbol name injectively into a conservative Eshkol identifier.

Function and value namespaces have distinct prefixes.  Every source character
is encoded as an underscore followed by its hexadecimal character code, so
punctuation, case, package syntax, and visually similar names cannot collide."
  (unless (symbolp symbol)
    (error 'eshkol-backend-error :stage :emit :status :invalid-identifier
           :detail (format nil "expected a symbol, got ~S" symbol)))
  (with-output-to-string (stream)
    (write-string (ecase namespace
                    (:function "rshf")
                    (:value "rshv"))
                  stream)
    (loop for character across (symbol-name symbol)
          do (format stream "_~X" (char-code character)))))

(defun %emit-term (term stream &key operator-mutant)
  (labels ((emit (node)
             (cond
               ((integerp node) (format stream "~D" node))
               ((symbolp node)
                (write-string (%mangle-identifier node :value) stream))
               ((consp node)
                (let ((head (first node)))
                  (cond
                    ((%name= head "IF")
                     ;; Scheme regards 0 as true; Rosette regards it as false.
                     (write-string "(if (= " stream)
                     (emit (second node))
                     (write-string " 0) " stream)
                     (emit (fourth node))
                     (write-char #\Space stream)
                     (emit (third node))
                     (write-char #\) stream))
                    ((%name= head "LET")
                     (format stream "(let ((~A "
                             (%mangle-identifier (second node) :value))
                     (emit (third node))
                     (write-string ")) " stream)
                     (emit (fourth node))
                     (write-char #\) stream))
                    ((%name= head "LAM")
                     (format stream "(lambda (~A) "
                             (%mangle-identifier (second node) :value))
                     (emit (fourth node))
                     (write-char #\) stream))
                    ((%name= head "APP")
                     (write-char #\( stream)
                     (emit (second node))
                     (write-char #\Space stream)
                     (emit (third node))
                     (write-char #\) stream))
                    ((member (symbol-name head) '("<" ">" "=")
                             :test #'string=)
                     ;; Eshkol comparisons are booleans; Rosette comparisons are
                     ;; integer values that may flow into arithmetic.
                     (format stream "(if (~A " (symbol-name head))
                     (emit (second node))
                     (write-char #\Space stream)
                     (emit (third node))
                     (write-string ") 1 0)" stream))
                    ((member (symbol-name head) '("+" "-" "*")
                             :test #'string=)
                     (let ((operator
                             (if (and (eq operator-mutant :add-to-sub)
                                      (string= (symbol-name head) "+"))
                                 "-"
                                 (symbol-name head))))
                       (format stream "(~A " operator)
                       (emit (second node))
                       (write-char #\Space stream)
                       (emit (third node))
                       (write-char #\) stream)))
                    ((%name= head "/")
                     (write-string "(quotient " stream)
                     (emit (second node))
                     (write-char #\Space stream)
                     (emit (third node))
                     (write-char #\) stream))
                    ((%name= head "%")
                     (write-string "(remainder " stream)
                     (emit (second node))
                     (write-char #\Space stream)
                     (emit (third node))
                     (write-char #\) stream))
                    (t
                     (format stream "(~A"
                             (%mangle-identifier head :function))
                     (dolist (argument (rest node))
                       (write-char #\Space stream)
                       (emit argument))
                     (write-char #\) stream)))))
               (t
                (error 'eshkol-backend-error :stage :emit
                       :status :unsupported-term
                       :detail (format nil "unsupported core term ~S" node))))))
    (emit term)))

(defun %admit-program-ir (program-ir)
  (unless (pir:programp program-ir)
    (error 'eshkol-backend-error :stage :admit :status :not-program-ir
           :detail "input is not an rosette-program-ir PROGRAM"))
  (handler-case
      (front:parse program-ir)
    (error (condition)
      (error 'eshkol-backend-error :stage :admit
             :status :front-door-refusal
             :detail (princ-to-string condition)))))

(defun %emit-module-body (program &key operator-mutant)
  (with-output-to-string (stream)
    (write-line ";; Generated from the validated rosette-front-door ProgramIR dialect." stream)
    (dolist (definition (core:core-program-funs program))
      (destructuring-bind (name parameters body) definition
        (format stream "(define (~A"
                (%mangle-identifier name :function))
        (dolist (parameter parameters)
          (format stream " ~A" (%mangle-identifier parameter :value)))
        (write-string ") " stream)
        (%emit-term body stream :operator-mutant operator-mutant)
        (write-line ")" stream)))
    (write-string "(define rosette_result " stream)
    (%emit-term (core:core-program-main program) stream
                :operator-mutant operator-mutant)
    (write-line ")" stream)))

(defun %emit-eshkol-source (program-ir &key operator-mutant)
  (let* ((program (%admit-program-ir program-ir))
         (body (%emit-module-body program :operator-mutant operator-mutant))
         (program-id
           (cid:content-id-long
            (list :rosette-front-door-eshkol-source/v1 body))))
    (values
     (with-output-to-string (stream)
       (write-string body stream)
       (format stream "(display ~S)~%" +receipt-marker+)
       (format stream "(display ~S)~%"
               (format nil
                       "(:schema ~S :ok t :program-id ~S :value "
                       +receipt-schema+ program-id))
       (write-line "(display rosette_result)" stream)
       (write-line "(display \")\")" stream)
       (write-line "(newline)" stream))
     program-id
     program)))

(defun %preflight-kernel-shape (program)
  "Refuse programs outside the canonical four-way kernel subset.

The front-door type system admits higher-order terms, while its silicon floor
currently lowers only applications that beta-reduce from a literal LAM.  Run
that structural lowering before probing Eshkol so GATE-ESHKOL never starts an
external tool for a program that cannot participate in all four readouts."
  (handler-case
      (core:lower-to-program program)
    (error (condition)
      (error 'eshkol-backend-error
             :stage :kernel-preflight
             :status :unsupported-shape
             :detail (princ-to-string condition)))))

(defun emit-eshkol-source (program-ir)
  "Emit deterministic Eshkol source for an admitted front-door PROGRAM-IR.

Returns two values: SOURCE and its durable PROGRAM-ID.  Arbitrary ProgramIR
dialects are refused; admission is delegated to ROSETTE-FRONT-DOOR:PARSE."
  (multiple-value-bind (source program-id)
      (%emit-eshkol-source program-ir)
    (values source program-id)))

(defun %proper-plist-p (value)
  (and (listp value)
       (handler-case
           (let ((length (list-length value)))
             (and length (evenp length)))
         (type-error () nil))))

(defun %exact-plist-keys-p (value keys)
  (and (%proper-plist-p value)
       (equal (loop for tail on value by #'cddr collect (first tail))
              keys)))

(defun %accept-result (form program-id)
  (and (%exact-plist-keys-p
        form '(:schema :ok :program-id :value))
       (eq +receipt-schema+ (getf form :schema))
       (eq t (getf form :ok))
       (stringp (getf form :program-id))
       (string= program-id (getf form :program-id))
       (integerp (getf form :value))
       form))

(defun %default-command ()
  (let ((override (uiop:getenv "ROSETTE_ESHKOL_BIN")))
    (if (and override (plusp (length override))) override "eshkol-run")))

(defun %default-policy ()
  (worker:make-isolation-policy
   :timeout-ms 120000
   :max-output-bytes 1048576
   :max-address-space-bytes (* 128 1024 1024 1024)
   :max-cpu-seconds 120
   :max-processes 256
   :max-open-files 256
   :poll-ms 10))

(defun %command (eshkol-command)
  (let ((command (or eshkol-command (%default-command))))
    (when (stringp command)
      (setf command (list command)))
    (unless (and (consp command)
                 (every (lambda (part)
                          (and (stringp part) (plusp (length part))))
                        command))
      (error 'eshkol-backend-error :stage :configuration
             :status :invalid-command
             :detail "ESHKOL-COMMAND must be a nonempty string or argv prefix"))
    (copy-list command)))

(defun %argv-prefix (prefix label)
  (when (stringp prefix)
    (setf prefix (list prefix)))
  (unless (or (null prefix)
              (and (consp prefix)
                   (every (lambda (part)
                            (and (stringp part) (plusp (length part))))
                          prefix)))
    (error 'eshkol-backend-error :stage :configuration
           :status :invalid-command
           :detail (format nil "~A must be NIL, a string, or an argv prefix"
                           label)))
  (copy-list prefix))

(defun eshkol-available-p (&key eshkol-command (policy (%default-policy)))
  "Return whether the configured Eshkol runner launches successfully.

The worker envelope is returned as a second value.  This probe is bounded and
accepts the compiler's zero exit status without retaining its raw version text."
  (let* ((command (%command eshkol-command))
         (result
           (worker:run-isolated-command
            (append command (list "--version"))
            :policy policy
            :require-receipt nil
            :metadata (list :backend :eshkol :stage :availability))))
    (values (envelope:tool-envelope-ok result) result)))

(defun %timeout-ms (policy)
  (getf (worker:isolation-policy->form policy) :timeout-ms))

(defun %run-jit (source-path program-id command policy)
  (worker:run-isolated-command
   (append
    (list "env"
          "ESHKOL_JIT_CACHE=0"
          "ESHKOL_JIT_COMPILE_THREADS=1"
          (format nil "ESHKOL_TIMEOUT_MS=~D" (%timeout-ms policy)))
    command
    (list "--no-stdlib"
          "--strict-types"
          "--optimize" "0"
          "--run" (uiop:native-namestring source-path)))
   :policy policy
   :marker +receipt-marker+
   :accept (lambda (form) (%accept-result form program-id))
   :metadata (list :backend :eshkol :mode :jit :program-id program-id
                   :optimization 0 :strict-types t :jit-cache nil)))

(defun %compile-aot (source-path output-path program-id command policy)
  (worker:run-isolated-command
   (append
    (list "env"
          "ESHKOL_JIT_COMPILE_THREADS=1"
          (format nil "ESHKOL_TIMEOUT_MS=~D" (%timeout-ms policy)))
    command
    (list "--no-stdlib"
          "--strict-types"
          "--optimize" "0"
          "--output" (uiop:native-namestring output-path)
          (uiop:native-namestring source-path)))
   :policy policy
   :require-receipt nil
   :metadata (list :backend :eshkol :mode :aot-compile
                   :program-id program-id :optimization 0 :strict-types t)))

(defun %run-aot (output-path program-id policy aot-run-prefix)
  (worker:run-isolated-command
   (append aot-run-prefix (list (uiop:native-namestring output-path)))
   :policy policy
   :marker +receipt-marker+
   :accept (lambda (form) (%accept-result form program-id))
   :metadata (list :backend :eshkol :mode :aot-run
                   :program-id program-id :optimization 0)))

(defun %require-success (stage result)
  (unless (envelope:tool-envelope-ok result)
    (error 'eshkol-backend-error
           :stage stage
           :status (envelope:tool-envelope-status result)
           :envelope result))
  result)

(defun %receipt-value (result)
  (getf (envelope:tool-envelope-data result) :value))

(defun %artifact-paths (output-path)
  (list output-path
        (make-pathname :type "o" :defaults output-path)
        (make-pathname :type "bc" :defaults output-path)))

(defun %delete-artifacts (output-path)
  (dolist (path (%artifact-paths output-path))
    (when (probe-file path)
      (ignore-errors (delete-file path)))))

(defun %make-temporary-workspace ()
  "Create a cross-UID workspace whose owner can remove container artifacts.

The sticky, non-listable directory permits a remapped container UID to read a
known source path and create the known output path.  Unlike the shared /tmp
root, its host owner may then unlink those artifacts regardless of their UID."
  #+sbcl
  (progn
    (require :sb-posix)
    (let ((created nil))
      (handler-case
          (let ((template
                  (uiop:native-namestring
                   (merge-pathnames "rosette-front-door-eshkol-XXXXXX"
                                    (uiop:temporary-directory)))))
            (setf created (uiop:symbol-call :sb-posix :mkdtemp template))
            (uiop:symbol-call :sb-posix :chmod created #o1733)
            (uiop:ensure-directory-pathname created))
        (error (condition)
          (when created
            (ignore-errors
              (uiop:delete-empty-directory
               (uiop:ensure-directory-pathname created))))
          (error 'eshkol-backend-error
                 :stage :workspace :status :creation-failed
                 :detail (type-of condition))))))
  #-sbcl
  (error 'eshkol-backend-error :stage :workspace
         :status :unsupported-runtime))

(defun %workspace-nonce ()
  (subseq
   (cid:content-id-long
    (list :rosette-front-door-eshkol-workspace/v1
          (get-universal-time) (get-internal-real-time)
          (random most-positive-fixnum)))
   0 24))

(defun %make-source-container-readable (source-path)
  #+sbcl
  (uiop:symbol-call :sb-posix :chmod
                    (uiop:native-namestring source-path) #o444)
  #-sbcl
  (declare (ignore source-path)))

(defun %cleanup-workspace (workspace source-path output-path)
  (let ((clean-p t))
    (%delete-artifacts output-path)
    (when (some #'probe-file (%artifact-paths output-path))
      (setf clean-p nil))
    (when (probe-file source-path)
      (ignore-errors (delete-file source-path)))
    (when (probe-file source-path)
      (setf clean-p nil))
  ;; Avoid recursive deletion: every path the backend may create is named
  ;; above, and an unexpected residue deliberately leaves evidence behind.
    (ignore-errors (uiop:delete-empty-directory workspace))
    (and clean-p (not (probe-file workspace)))))

(defun %execute-in-workspace
    (source program-id command aot-prefix policy front-result direct kernel
     source-path output-path)
  (with-open-file
      (source-stream source-path :direction :output
       :if-exists :error :if-does-not-exist :create
       :external-format :utf-8)
    (write-string source source-stream))
  (%make-source-container-readable source-path)
  (let* ((jit (%require-success
               :jit
               (%run-jit source-path program-id command policy)))
         (aot-compile
           (%require-success
            :aot-compile
            (%compile-aot source-path output-path program-id command policy))))
    (unless (probe-file output-path)
      (error 'eshkol-backend-error
             :stage :aot-compile :status :missing-artifact
             :envelope aot-compile))
    (let* ((aot (%require-success
                 :aot-run
                 (%run-aot output-path program-id policy aot-prefix)))
           (jit-value (%receipt-value jit))
           (aot-value (%receipt-value aot))
           (passed (and (front:front-door-result-gate-passed front-result)
                        (= direct kernel jit-value aot-value))))
      (%make-eshkol-gate-report
       :program-id program-id
       :toolchain-command command
       :direct-value direct
       :kernel-value kernel
       :jit-value jit-value
       :aot-value aot-value
       :jit-envelope jit
       :aot-compile-envelope aot-compile
       :aot-envelope aot
       :passed passed))))

(defun %gate-eshkol
    (program-ir &key eshkol-command aot-run-prefix
                      (policy (%default-policy)) operator-mutant)
  ;; Admission and deterministic emission precede configuration/probing so an
  ;; invalid or non-kernel-lowerable ProgramIR is always refused without
  ;; launching external work.
  (multiple-value-bind (source program-id program)
      (%emit-eshkol-source program-ir :operator-mutant operator-mutant)
    (%preflight-kernel-shape program)
    (let ((command (%command eshkol-command))
          (aot-prefix (%argv-prefix aot-run-prefix "AOT-RUN-PREFIX")))
      (multiple-value-bind (available availability-envelope)
          (eshkol-available-p :eshkol-command command :policy policy)
        (unless available
          (error 'eshkol-unavailable
                 :stage :availability
                 :status (envelope:tool-envelope-status availability-envelope)
                 :envelope availability-envelope)))
      (let* ((front-result (front:run program-ir))
             (direct (front:front-door-result-eval-value front-result))
             (kernel (front:front-door-result-oracle-value front-result))
             (workspace (%make-temporary-workspace))
             (nonce (%workspace-nonce))
             (source-path
               (merge-pathnames (format nil "source-~A.esk" nonce) workspace))
             (output-path
               (merge-pathnames (format nil "artifact-~A" nonce) workspace))
             (completed-p nil))
        (unwind-protect
             (let ((report
                     (%execute-in-workspace
                      source program-id command aot-prefix policy front-result
                      direct kernel source-path output-path)))
               (setf completed-p t)
               report)
          (unless (%cleanup-workspace workspace source-path output-path)
            (if completed-p
                (error 'eshkol-backend-error
                       :stage :cleanup :status :residue
                       :detail (uiop:native-namestring workspace))
                (warn "Eshkol workspace cleanup left residue at ~A"
                      (uiop:native-namestring workspace)))))))))

(defun gate-eshkol
    (program-ir &key eshkol-command aot-run-prefix (policy (%default-policy)))
  "Compile an admitted PROGRAM-IR through Eshkol JIT and AOT.

Returns an ESHKOL-GATE-REPORT whose PASSED field is true exactly when the
front-door direct evaluator, canonical kernel VM, cache-disabled Eshkol JIT,
and separately compiled AOT executable return the same integer.  An unavailable
or failed toolchain, or a front-door term outside the canonical kernel shape,
signals ESHKOL-BACKEND-ERROR; semantic disagreement remains visible as a
non-passing report."
  (%gate-eshkol program-ir
                :eshkol-command eshkol-command
                :aot-run-prefix aot-run-prefix
                :policy policy))
