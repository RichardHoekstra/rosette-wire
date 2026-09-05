;;;; cli.lisp --- six-command machine-readable Rosette Wire interface.

(in-package #:rosette-wire)

(defun %read-stream-text (stream)
  (with-output-to-string (output)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          do (write-line line output))))

(defun %load-command-text (path loader input-stream)
  (if (string= path "-")
      (%read-stream-text input-stream)
      (funcall loader path)))

(defun %command-object (text)
  (json-parse text))

(defun %command-option (arguments option)
  (let ((position (position option arguments :test #'string=)))
    (when position
      (unless (< position (1- (length arguments)))
        (%wire-error :invalid-invocation "$" "~A requires a path" option))
      (nth (1+ position) arguments))))

(defun %command-target (arguments)
  (or (first arguments)
      (%wire-error :invalid-invocation "$" "a descriptor or receipt path is required")))

(defun %reject-extra-options (arguments allowed)
  (loop for tail on (rest arguments)
        for item = (first tail)
        when (and (plusp (length item)) (char= (char item 0) #\-))
          do (unless (member item allowed :test #'string=)
               (%wire-error :invalid-invocation "$" "unknown option ~A" item))
             (unless (rest tail)
               (%wire-error :invalid-invocation "$" "~A requires a value" item))
             (setf tail (rest tail))
        else do (%wire-error :invalid-invocation "$" "unexpected argument ~A" item)))

(defun %value-envelope (kind id value)
  `(("id" . ,id) ("kind" . ,kind) ("value" . ,value)))

(defun %receipt-envelope (receipt)
  (%value-envelope "receipt" (wire-receipt-id receipt)
                   (wire-receipt->value receipt)))

(defparameter +resource-exit-codes+
  '(:step-budget :wall-budget :output-budget))
(defparameter +unavailable-exit-codes+
  '(:handler-unavailable :verifier-unavailable :capability-denied
    :missing-verifier :missing-provider :missing-lock))

(defun %receipt-exit-code (receipt)
  (if (eq :pass (wire-receipt-verdict receipt)) 0
      (let ((codes (mapcar #'wire-violation-code
                           (wire-receipt-violations receipt))))
        (cond ((intersection codes +resource-exit-codes+) 4)
              ((intersection codes +unavailable-exit-codes+) 3)
              (t 1)))))

(defun %decode-command-inputs (text)
  (let ((value (%command-object text)))
    (if (eq value :empty-object) nil
        (%decode-object value "$.inputs"))))

(defun %describe-command (text)
  (let* ((value (%command-object text))
         (object (%decode-object value "$"))
         (schema (%decode-ref object "schema")))
    (cond ((and (stringp schema) (string= schema +wire-schema+))
           (let ((descriptor (wire-descriptor-from-value value)))
             (%value-envelope "wire" (wire-contract-id descriptor)
                              (wire-descriptor->value descriptor))))
          ((and (stringp schema) (string= schema +graph-schema+))
           (let ((graph (wire-graph-from-value value)))
             (%value-envelope "graph" (wire-graph-id graph)
                              (wire-graph->value graph))))
          ((and (stringp schema) (string= schema +receipt-schema+))
           (%receipt-envelope (wire-receipt-from-value value)))
          (t (%wire-error :schema-mismatch "$.schema"
                          "expected a Wire, graph, or receipt schema")))))

(defun rosette-command (arguments &key (runner (make-wire-runner))
                                       (loader #'uiop:read-file-string)
                                       (input-stream *standard-input*))
  "Execute one Wire command and return (values EXIT-CODE JSON-VALUE).

ARGUMENTS excludes the executable name. LOADER makes the command embeddable and
testable without ambient filesystem authority. No command calls UIOP:QUIT."
  (%require-list arguments "$.argv")
  (let* ((verb (or (first arguments)
                   (%wire-error :invalid-invocation "$" "missing command")))
         (rest (rest arguments))
         (target (%command-target rest)))
    (cond
      ((string= verb "describe")
       (%reject-extra-options rest nil)
       (values 0 (%describe-command
                  (%load-command-text target loader input-stream))))
      ((string= verb "connect")
       (%reject-extra-options rest nil)
       (let ((graph (wire-graph-from-json
                     (%load-command-text target loader input-stream))))
         (values 0 (%value-envelope "graph" (wire-graph-id graph)
                                    (wire-graph->value graph)))))
      ((string= verb "validate")
       (%reject-extra-options rest nil)
       (let* ((graph (wire-graph-from-json
                      (%load-command-text target loader input-stream)))
              (receipt (validate-wire-graph graph)))
         (values (%receipt-exit-code receipt) (%receipt-envelope receipt))))
      ((string= verb "run")
       (%reject-extra-options rest '("--input"))
       (let* ((graph (wire-graph-from-json
                      (%load-command-text target loader input-stream)))
              (input-path (%command-option rest "--input"))
              (inputs (if input-path
                          (%decode-command-inputs
                           (%load-command-text input-path loader input-stream))
                          nil))
              (receipt (run-wire-graph graph runner inputs)))
         (values (%receipt-exit-code receipt) (%receipt-envelope receipt))))
      ((string= verb "verify")
       (%reject-extra-options rest '("--graph"))
       (let ((graph-path (%command-option rest "--graph")))
         (unless graph-path
           (%wire-error :invalid-invocation "$"
                        "verify requires --graph until content-store resolution is installed"))
         (let* ((receipt (wire-receipt-from-json
                          (%load-command-text target loader input-stream)))
                (graph (wire-graph-from-json
                        (%load-command-text graph-path loader input-stream)))
                (verified (verify-wire-receipt graph receipt runner)))
           (values (%receipt-exit-code verified) (%receipt-envelope verified)))))
      ((string= verb "certify")
       (%reject-extra-options rest '("--input"))
       (let* ((graph (wire-graph-from-json
                      (%load-command-text target loader input-stream)))
              (input-path (%command-option rest "--input"))
              (inputs (if input-path
                          (%decode-command-inputs
                           (%load-command-text input-path loader input-stream))
                          nil))
              (receipt (certify-wire-graph graph runner inputs)))
         (values (%receipt-exit-code receipt) (%receipt-envelope receipt))))
      (t (%wire-error :invalid-invocation "$" "unknown command ~S" verb)))))

(defun %error-envelope (code path detail)
  `(("error" . (("code" . ,(%enum-name code))
                 ("detail" . ,detail)
                 ("path" . ,path)))
    ("kind" . "error")))

(defun rosette-main (&key (arguments (uiop:command-line-arguments))
                          (runner (make-wire-runner))
                          (input-stream *standard-input*)
                          (output-stream *standard-output*)
                          (loader #'uiop:read-file-string))
  "Write exactly one canonical JSON result and return its documented exit code."
  (multiple-value-bind (code value)
      (handler-case
          (rosette-command arguments :runner runner :loader loader
                                     :input-stream input-stream)
        (wire-error (condition)
          (values 2 (%error-envelope (wire-error-code condition)
                                     (wire-error-path condition)
                                     (wire-error-detail condition))))
        (json-parse-error (condition)
          (values 2 (%error-envelope :malformed-json "$"
                                     (princ-to-string condition))))
        (file-error ()
          (values 2 (%error-envelope :unavailable-input "$"
                                     "input path is unavailable")))
        (error ()
          (values 5 (%error-envelope :internal-runner "$"
                                     "internal runner error"))))
    (write-string (canonical-json value) output-stream)
    (terpri output-stream)
    (finish-output output-stream)
    code))
