;;;; composition-namespace.lisp --- Composition projection + bounded 9P subset.

(in-package #:composition-namespace)

(define-condition namespace-error (error)
  ((code :initarg :code :reader namespace-error-code)
   (detail :initarg :detail :reader namespace-error-detail))
  (:report (lambda (condition stream)
             (format stream "Composition namespace ~A: ~A"
                     (namespace-error-code condition)
                     (namespace-error-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'namespace-error :code code
         :detail (apply #'format nil control arguments)))

(defun %field (name object)
  (cdr (assoc name object :test #'string=)))

(defun %safe-segment (string)
  "Injectively quote a Wire name as one conservative pathname segment."
  (with-output-to-string (stream)
    (loop for character across string
          for code = (char-code character)
          if (or (alphanumericp character)
                 (find character "-_." :test #'char=))
            do (write-char character stream)
          else do (format stream "_~4,'0X" code))))

(defun %join (base segment)
  (if (string= base "/")
      (concatenate 'string "/" segment)
      (concatenate 'string base "/" segment)))

(defstruct (operation-endpoint (:constructor %make-operation-endpoint))
  node port operation id)

(defun %endpoint-form (endpoint)
  (list :node (operation-endpoint-node endpoint)
        :port (operation-endpoint-port endpoint)
        :operation (operation-endpoint-operation endpoint)
        :id (operation-endpoint-id endpoint)))

(defstruct (namespace-entry (:constructor %make-namespace-entry))
  path kind content endpoint)

(defstruct (composition-namespace (:constructor %make-composition-namespace))
  id composition-id entries)

(defun %entry-form (entry)
  (list :path (namespace-entry-path entry)
        :kind (namespace-entry-kind entry)
        :content (namespace-entry-content entry)
        :endpoint (and (namespace-entry-endpoint entry)
                       (%endpoint-form (namespace-entry-endpoint entry)))))

(defun %projection-content (composition-id entries)
  (list :schema :rosette-plan9-namespace/v1
        :composition-id composition-id
        :entries (mapcar #'%entry-form entries)))

(defun %make-endpoint (composition-id node port operation)
  (let ((form (list :composition composition-id :node node
                    :port port :operation operation)))
    (%make-operation-endpoint
     :node node :port port :operation operation
     :id (cid:content-id-long form))))

(defun project-composition-namespace (composition)
  "Project one Rosette Composition into a deterministic, read-mostly namespace.
Only exported operations acquire writable CALL endpoints."
  (unless (rw:composition-p composition)
    (%refuse :invalid-composition "expected a Rosette Composition"))
  (let* ((receipt (rw:validate-composition composition))
         (composition-id (rw:composition-id composition)))
    (unless (eq :pass (rw:wire-receipt-verdict receipt))
      (%refuse :rosette-refusal "Rosette validation refused the Composition"))
    (let ((entries nil) (paths (make-hash-table :test #'equal)))
      (labels ((add (path kind &key content endpoint)
                 (when (gethash path paths)
                   (%refuse :path-collision "projection path collision at ~A" path))
                 (setf (gethash path paths) t)
                 (push (%make-namespace-entry
                        :path path :kind kind :content content
                        :endpoint endpoint)
                       entries))
               (dir (path)
                 (unless (gethash path paths) (add path :directory))))
        (dir "/")
        (add "/id" :file :content composition-id)
        (add "/manifest.json" :file
             :content (rw:canonical-json (rw:composition->value composition)))
        (dolist (root '("/nodes" "/inputs" "/outputs")) (dir root))
        (let ((value (rw:composition->value composition)))
          (dolist (node (%field "nodes" value))
            (let* ((node-name (%field "id" node))
                   (node-root (%join "/nodes" (%safe-segment node-name)))
                   (contract (%field "contract" node)))
              (dir node-root)
              (add (%join node-root "contract.json") :file
                   :content (rw:canonical-json contract))
              (let ((exports-root (%join node-root "exports")))
                (dir exports-root)
                (dolist (port (%field "exports" contract))
                  (let* ((port-name (%field "name" port))
                         (port-root (%join exports-root
                                           (%safe-segment port-name))))
                    (dir port-root)
                    (dolist (operation (%field "operations" port))
                      (let* ((operation-name (%field "name" operation))
                             (operation-root
                               (%join port-root (%safe-segment operation-name)))
                             (endpoint (%make-endpoint
                                        composition-id node-name port-name
                                        operation-name)))
                        (dir operation-root)
                        (add (%join operation-root "contract.json") :file
                             :content (rw:canonical-json operation))
                        (add (%join operation-root "call") :endpoint
                             :endpoint endpoint))))))))
          (dolist (input (%field "inputs" value))
            (add (%join "/inputs" (%safe-segment (%field "name" input)))
                 :file :content (rw:canonical-json input)))
          (dolist (output (%field "outputs" value))
            (add (%join "/outputs" (%safe-segment (%field "name" output)))
                 :file :content (rw:canonical-json output))))
        (setf entries (sort entries #'string< :key #'namespace-entry-path))
        (let ((content (%projection-content composition-id entries)))
          (%make-composition-namespace
           :id (cid:content-id-long content) :composition-id composition-id
           :entries entries))))))

(defun namespace-lookup (namespace path)
  (check-type namespace composition-namespace)
  (find path (composition-namespace-entries namespace)
        :test #'string= :key #'namespace-entry-path))

(defun namespace-list (namespace path)
  "Return immediate child segment names in deterministic order."
  (let ((entry (namespace-lookup namespace path)))
    (unless (and entry (eq :directory (namespace-entry-kind entry)))
      (%refuse :not-directory "~A is not a directory" path))
    (let ((prefix (if (string= path "/") "/"
                      (concatenate 'string path "/"))))
      (sort
       (remove-duplicates
        (loop for candidate in (composition-namespace-entries namespace)
              for candidate-path = (namespace-entry-path candidate)
              when (and (> (length candidate-path) (length prefix))
                        (string= prefix candidate-path
                                 :end2 (length prefix)))
                collect (let* ((tail (subseq candidate-path (length prefix)))
                               (slash (position #\/ tail)))
                          (if slash (subseq tail 0 slash) tail)))
        :test #'string=)
       #'string<))))

(defun verify-composition-namespace (namespace composition)
  (and (composition-namespace-p namespace) (rw:composition-p composition)
       (handler-case
           (let ((fresh (project-composition-namespace composition)))
             (and (string= (composition-namespace-id namespace)
                           (composition-namespace-id fresh))
                  (equal (mapcar #'%entry-form
                                 (composition-namespace-entries namespace))
                         (mapcar #'%entry-form
                                 (composition-namespace-entries fresh)))))
         (error () nil))))

(defun %octets-p (value)
  (typep value '(vector (unsigned-byte 8))))

(defun %bytes->string (bytes)
  (unless (%octets-p bytes) (%refuse :invalid-bytes "expected an octet vector"))
  (map 'string #'code-char bytes))

(defun make-rosette-composition-invoker (composition runner)
  "Return an endpoint invoker that runs the owning Composition through its
explicit Rosette runner and returns execution + independent verification
receipts as canonical JSON bytes. The endpoint must occur in a declared step."
  (unless (and (rw:composition-p composition)
               (rw:composition-runner-p runner))
    (%refuse :invalid-invoker "expected a Composition and Composition runner"))
  (let* ((value (rw:composition->value composition))
         (targets
           (mapcar (lambda (step)
                     (list (%field "node" step) (%field "port" step)
                           (%field "operation" step)))
                   (%field "steps" value))))
    (lambda (endpoint payload)
      (unless (member (list (operation-endpoint-node endpoint)
                            (operation-endpoint-port endpoint)
                            (operation-endpoint-operation endpoint))
                      targets :test #'equal)
        (%refuse :unbound-endpoint
                 "endpoint is exported but not invoked by this Composition"))
      (let* ((inputs (json:json-parse (%bytes->string payload)))
             (execution (rw:run-composition composition runner inputs))
             (verification
               (rw:verify-composition-receipt composition execution runner)))
        (crypto:string->bytes
         (rw:canonical-json
          `(("execution" . ,(rw:wire-receipt->value execution))
            ("verification" . ,(rw:wire-receipt->value verification)))))))))

;;; Bounded semantic 9P2000 subset. Framing/socket transport remains external.

(defparameter +request-types+
  '(:version :attach :walk :open :read :write :clunk))

(defstruct (ninep-request (:constructor %make-ninep-request))
  type tag fid newfid names mode offset count data msize version uname aname)

(defun make-ninep-request
    (type tag &key fid newfid names mode offset count data msize version uname aname)
  (unless (member type +request-types+) (%refuse :unsupported "request ~S" type))
  (unless (typep tag '(integer 0 65535)) (%refuse :invalid-tag "tag ~S" tag))
  (%make-ninep-request :type type :tag tag :fid fid :newfid newfid
                       :names names :mode mode :offset offset :count count
                       :data data :msize msize :version version
                       :uname uname :aname aname))

(defstruct (ninep-reply (:constructor %make-ninep-reply))
  type tag data count qids msize version error)

(defstruct (%fid-state (:constructor %make-fid-state))
  path mode response)

(defstruct (ninep-session (:constructor %make-ninep-session))
  namespace invoker fids
  (request-count 0) (max-requests 256) (max-fids 64)
  (max-msize 65536) (msize 8192) (max-walk 16) (max-io 65536))

(defun make-ninep-session
    (namespace &key invoker (max-requests 256) (max-fids 64)
                    (max-msize 65536) (max-walk 16) (max-io 65536))
  (check-type namespace composition-namespace)
  (unless (and (or (null invoker) (functionp invoker))
               (every (lambda (n) (and (integerp n) (plusp n)))
                      (list max-requests max-fids max-msize max-walk max-io)))
    (%refuse :invalid-limits "session limits must be positive integers"))
  (%make-ninep-session
   :namespace namespace :invoker invoker :fids (make-hash-table)
   :max-requests max-requests :max-fids max-fids :max-msize max-msize
   :msize (min 8192 max-msize) :max-walk max-walk :max-io max-io))

(defun %reply (request type &rest initargs)
  (apply #'%make-ninep-reply :type type :tag (ninep-request-tag request)
         initargs))

(defun %error-reply (request condition)
  (%reply request :error
          :error (if (typep condition 'namespace-error)
                     (format nil "~A: ~A" (namespace-error-code condition)
                             (namespace-error-detail condition))
                     "adapter failure")))

(defun %fid (session number)
  (or (and (typep number '(integer 0 #xffffffff))
           (gethash number (ninep-session-fids session)))
      (%refuse :unknown-fid "unknown fid ~S" number)))

(defun %qid (namespace path)
  (cid:content-id-long
   (list :namespace (composition-namespace-id namespace) :path path)))

(defun %read-bytes (session state)
  (let* ((namespace (ninep-session-namespace session))
         (entry (namespace-lookup namespace (%fid-state-path state))))
    (case (namespace-entry-kind entry)
      (:directory
       (crypto:string->bytes
        (with-output-to-string (stream)
          (dolist (name (namespace-list namespace (namespace-entry-path entry)))
            (write-line name stream)))))
      (:file (crypto:string->bytes (or (namespace-entry-content entry) "")))
      (:endpoint (or (%fid-state-response state) (make-array 0 :element-type '(unsigned-byte 8))))
      (otherwise (%refuse :not-readable "entry is not readable")))))

(defun %suboctets (bytes offset count)
  (let ((start (min offset (length bytes))))
    (subseq bytes start (min (length bytes) (+ start count)))))

(defun %handle-ninep-request (session request)
  (let ((namespace (ninep-session-namespace session))
        (fids (ninep-session-fids session)))
    (ecase (ninep-request-type request)
      (:version
       (unless (and (string= "9P2000" (ninep-request-version request))
                    (typep (ninep-request-msize request) '(integer 256 *)))
         (%refuse :version "only 9P2000 with msize >= 256 is supported"))
       (clrhash fids)
       (setf (ninep-session-msize session)
             (min (ninep-request-msize request)
                  (ninep-session-max-msize session)))
       (%reply request :version :msize (ninep-session-msize session)
               :version "9P2000"))
      (:attach
       (let ((fid (ninep-request-fid request)))
         (when (gethash fid fids) (%refuse :fid-in-use "fid ~D" fid))
         (when (>= (hash-table-count fids) (ninep-session-max-fids session))
           (%refuse :fid-budget "fid budget exhausted"))
         (setf (gethash fid fids) (%make-fid-state :path "/"))
         (%reply request :attach :qids (list (%qid namespace "/")))))
      (:walk
       (let* ((old (%fid session (ninep-request-fid request)))
              (newfid (ninep-request-newfid request))
              (names (or (ninep-request-names request) nil))
              (path (%fid-state-path old)) (qids nil))
         (unless (and (listp names)
                      (<= (length names) (ninep-session-max-walk session)))
           (%refuse :walk-budget "walk exceeds the segment bound"))
         (when (and (/= newfid (ninep-request-fid request))
                    (gethash newfid fids))
           (%refuse :fid-in-use "newfid ~D" newfid))
         (dolist (name names)
           (unless (and (stringp name) (plusp (length name))
                        (not (member name '("." "..") :test #'string=))
                        (not (find #\/ name)))
             (%refuse :invalid-walk "invalid path segment ~S" name))
           (setf path (%join path name))
           (unless (namespace-lookup namespace path)
             (%refuse :not-found "~A" path))
           (push (%qid namespace path) qids))
         (when (and (= (hash-table-count fids) (ninep-session-max-fids session))
                    (null (gethash newfid fids)))
           (%refuse :fid-budget "fid budget exhausted"))
         (setf (gethash newfid fids) (%make-fid-state :path path))
         (%reply request :walk :qids (nreverse qids))))
      (:open
       (let* ((state (%fid session (ninep-request-fid request)))
              (entry (namespace-lookup namespace (%fid-state-path state)))
              (mode (ninep-request-mode request)))
         (unless (member mode '(:read :write :read-write))
           (%refuse :invalid-mode "mode ~S" mode))
         (when (and (eq :directory (namespace-entry-kind entry))
                    (not (eq :read mode)))
           (%refuse :read-only "directories are read-only"))
         (when (and (eq :file (namespace-entry-kind entry))
                    (not (eq :read mode)))
           (%refuse :read-only "static files are read-only"))
         (setf (%fid-state-mode state) mode)
         (%reply request :open :qids
                 (list (%qid namespace (%fid-state-path state))))))
      (:read
       (let* ((state (%fid session (ninep-request-fid request)))
              (offset (ninep-request-offset request))
              (count (ninep-request-count request)))
         (unless (and (member (%fid-state-mode state) '(:read :read-write))
                      (typep offset '(integer 0 *))
                      (typep count '(integer 0 *))
                      (<= count (ninep-session-max-io session))
                      (<= count (ninep-session-msize session)))
           (%refuse :read-bound "read is unopened or exceeds its bound"))
         (let ((data (%suboctets (%read-bytes session state) offset count)))
           (%reply request :read :data data :count (length data)))))
      (:write
       (let* ((state (%fid session (ninep-request-fid request)))
              (entry (namespace-lookup namespace (%fid-state-path state)))
              (data (ninep-request-data request)))
         (unless (and (member (%fid-state-mode state) '(:write :read-write))
                      (eq :endpoint (namespace-entry-kind entry))
                      (zerop (or (ninep-request-offset request) 0))
                      (%octets-p data)
                      (<= (length data) (ninep-session-max-io session))
                      (<= (length data) (ninep-session-msize session)))
           (%refuse :write-bound "write is not a bounded endpoint write"))
         (unless (ninep-session-invoker session)
           (%refuse :no-invoker "session has no Rosette invocation capability"))
         (let ((response
                 (funcall (ninep-session-invoker session)
                          (namespace-entry-endpoint entry) (copy-seq data))))
           (unless (and (%octets-p response)
                        (<= (length response) (ninep-session-max-io session)))
             (%refuse :response-bound "invoker response exceeds its bound"))
           (setf (%fid-state-response state) (copy-seq response))
           (%reply request :write :count (length data)))))
      (:clunk
       (%fid session (ninep-request-fid request))
       (remhash (ninep-request-fid request) fids)
       (%reply request :clunk)))))

(defun handle-ninep-request (session request)
  "Handle one semantic 9P2000 request. All protocol errors become RERROR-like
replies; they never mutate namespace authority."
  (check-type session ninep-session)
  (check-type request ninep-request)
  (incf (ninep-session-request-count session))
  (if (> (ninep-session-request-count session)
         (ninep-session-max-requests session))
      (%reply request :error :error "request-budget: exhausted")
      (handler-case (%handle-ninep-request session request)
        (error (condition) (%error-reply request condition)))))
