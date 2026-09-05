(in-package #:rosette-isolated-worker)

(defstruct (isolation-policy (:constructor %make-isolation-policy))
  timeout-ms max-output-bytes max-address-space-bytes max-cpu-seconds
  max-processes max-open-files poll-ms)

(defun make-isolation-policy (&key (timeout-ms 30000) (max-output-bytes 1048576)
                                   (max-address-space-bytes (* 128 1024 1024 1024))
                                   (max-cpu-seconds 60) (max-processes 256)
                                   (max-open-files 128) (poll-ms 10))
  (dolist (x (list timeout-ms max-output-bytes max-address-space-bytes
                   max-cpu-seconds max-open-files poll-ms))
    (unless (and (integerp x) (plusp x)) (error "invalid isolation ceiling ~S" x)))
  (unless (or (eq max-processes :inherit)
              (and (integerp max-processes) (plusp max-processes)))
    (error "invalid process ceiling ~S" max-processes))
  (%make-isolation-policy
   :timeout-ms timeout-ms :max-output-bytes max-output-bytes
   :max-address-space-bytes max-address-space-bytes
   :max-cpu-seconds max-cpu-seconds :max-processes max-processes
   :max-open-files max-open-files :poll-ms poll-ms))

(defun isolation-policy->form (p)
  (list :timeout-ms (isolation-policy-timeout-ms p)
        :max-output-bytes (isolation-policy-max-output-bytes p)
        :max-address-space-bytes (isolation-policy-max-address-space-bytes p)
        :max-cpu-seconds (isolation-policy-max-cpu-seconds p)
        :max-processes (isolation-policy-max-processes p)
        :max-open-files (isolation-policy-max-open-files p)
        :poll-ms (isolation-policy-poll-ms p)))

(defun %temporary-path (tag)
  (merge-pathnames
   (format nil "rosette-isolated-worker-~A-~36R-~36R.log" tag
           (get-universal-time) (random most-positive-fixnum))
   (uiop:temporary-directory)))

(defun %file-size (path)
  (or (ignore-errors (with-open-file (s path :direction :input
                                         :element-type '(unsigned-byte 8))
                       (file-length s))) 0))

(defun %slurp (path limit)
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (if (null s) ""
        (let* ((n (min limit (file-length s))) (out (make-string n)))
          (read-sequence out s) out))))

(defun %parse-result (output marker accept &optional receipt-parser)
  (let ((at (search marker output :from-end t)))
    (when (and at (null (search marker output :end2 at)))
      (handler-case
          (if receipt-parser
              (funcall accept (funcall receipt-parser
                                      (subseq output (+ at (length marker)))))
              (let ((*read-eval* nil))
                (multiple-value-bind (form end)
                    (read-from-string output nil nil
                                      :start (+ at (length marker)))
                  (and form
                       (loop for index from end below (length output)
                             always (member (char output index)
                                            '(#\Space #\Tab #\Newline
                                              #\Return #\Page)
                                            :test #'char=))
                       (funcall accept form)))))
        (error () nil)))))

#+sbcl
(defun %signal-worker-group (process signal)
  "Signal the dedicated process group created by SBCL for PROCESS."
  (handler-case
      (progn
        (require :sb-posix)
        (uiop:symbol-call :sb-posix :kill
                          (- (sb-ext:process-pid process)) signal)
        t)
    (error ()
      ;; Fail closed even in the narrow launch race where the process group is
      ;; not yet addressable.
      (ignore-errors (sb-ext:process-kill process signal :pid)))))

#+sbcl
(defun %kill-worker (process)
  "Terminate the isolated command and every descendant in its process group."
  (%signal-worker-group process 15)
  (sleep 0.05)
  ;; Send the hard kill even if the group leader exited after TERM: a
  ;; TERM-ignoring descendant may still retain the process group.
  (%signal-worker-group process 9)
  (ignore-errors (sb-ext:process-wait process)))

#+sbcl
(defun %process-group-empty-p (process-group-id)
  "Fail closed unless PROCESS-GROUP-ID no longer names a live process group."
  (require :sb-posix)
  (handler-case
      (progn
        (uiop:symbol-call :sb-posix :kill (- process-group-id) 0)
        nil)
    (error (condition)
      (let ((errno-reader (find-symbol "SYSCALL-ERRNO" :sb-posix))
            (esrch (find-symbol "ESRCH" :sb-posix)))
        (and errno-reader esrch
             (ignore-errors
               (= (funcall errno-reader condition)
                  (symbol-value esrch))))))))

#+sbcl
(defun %await-process-group-empty (process-group-id &optional (timeout-ms 500))
  "Allow bounded OS reaping after termination; only ESRCH proves cleanup."
  (let ((deadline (+ (get-internal-real-time)
                     (ceiling (* timeout-ms internal-time-units-per-second)
                              1000))))
    (loop
      (when (%process-group-empty-p process-group-id) (return t))
      (when (>= (get-internal-real-time) deadline) (return nil))
      (sleep 0.005))))

(defun %wrapped-argv (argv policy)
  (append (list "prlimit"
                (format nil "--cpu=~D" (isolation-policy-max-cpu-seconds policy))
                (format nil "--as=~D" (isolation-policy-max-address-space-bytes policy)))
          (unless (eq :inherit (isolation-policy-max-processes policy))
            (list (format nil "--nproc=~D"
                          (isolation-policy-max-processes policy))))
          (list (format nil "--nofile=~D"
                        (isolation-policy-max-open-files policy)) "--")
          argv))

;;; -------------------------------------------------------------------------
;;; Persistent line-framed sessions

#+sbcl
(defstruct (isolated-session (:constructor %make-isolated-session) (:copier nil))
  process input output stdout-copy stdout-path stderr-path policy
  started deadline cancel-predicate
  lock waitqueue reader-thread watchdog-thread
  (lines '())
  (output-bytes 0 :type integer)
  (status :running)
  process-group-empty-p
  receipt)

#+sbcl
(defun isolated-session-running-p (session)
  "Return true while SESSION still owns a live worker process group."
  (check-type session isolated-session)
  (sb-thread:with-mutex ((isolated-session-lock session))
    (eq :running (isolated-session-status session))))

#+sbcl
(defun %valid-argv-p (argv)
  (and (consp argv)
       (every (lambda (x) (and (stringp x) (plusp (length x)))) argv)))

#+sbcl
(defun %claim-session-terminal (session status)
  "Atomically install the first terminal STATUS and wake a blocked reader."
  (sb-thread:with-mutex ((isolated-session-lock session))
    (when (eq :running (isolated-session-status session))
      (setf (isolated-session-status session) status)
      (sb-thread:condition-notify (isolated-session-waitqueue session))
      t)))

#+sbcl
(defun %close-session-streams (session)
  (ignore-errors (close (isolated-session-input session)))
  (ignore-errors (close (isolated-session-output session)))
  (ignore-errors (close (isolated-session-stdout-copy session))))

#+sbcl
(defun %terminate-isolated-session (session status)
  "Win SESSION's terminal race, then kill and await its complete process group."
  (when (%claim-session-terminal session status)
    (%kill-worker (isolated-session-process session))
    ;; Closing a stream from another thread can wait on the reader's stream
    ;; lock while that reader is blocked in READ-LINE.  Process-group exit is
    ;; the wakeup; STOP joins the reader before closing its streams.
    (ignore-errors (close (isolated-session-input session)))
    (setf (isolated-session-process-group-empty-p session)
          (%await-process-group-empty
           (sb-ext:process-pid (isolated-session-process session))))
    t))

#+sbcl
(defun %finish-naturally-exited-session (session)
  (let ((process (isolated-session-process session)))
    (ignore-errors (sb-ext:process-wait process))
    (when (%claim-session-terminal
           session
           (if (and (integerp (sb-ext:process-exit-code process))
                    (zerop (sb-ext:process-exit-code process)))
               :exited
               :worker-error))
      (%close-session-streams session))))

#+sbcl
(defun %session-reader-loop (session)
  "Drain line-framed stdout so a worker cannot block on a full pipe."
  (handler-case
      (progn
        (loop for line = (read-line (isolated-session-output session) nil nil)
              while line
              do (let ((overflow-p nil))
                   (write-line line (isolated-session-stdout-copy session))
                   (finish-output (isolated-session-stdout-copy session))
                   (sb-thread:with-mutex ((isolated-session-lock session))
                     (incf (isolated-session-output-bytes session)
                           (1+ (length line)))
                     (if (> (+ (isolated-session-output-bytes session)
                               (%file-size
                                (isolated-session-stderr-path session)))
                            (isolation-policy-max-output-bytes
                             (isolated-session-policy session)))
                         (setf overflow-p t)
                         (setf (isolated-session-lines session)
                               (nconc (isolated-session-lines session)
                                      (list line))))
                     (sb-thread:condition-notify
                      (isolated-session-waitqueue session)))
                   (when overflow-p
                     (%terminate-isolated-session session :output-limit)
                     (return))))
        (%finish-naturally-exited-session session))
    (error ()
      (when (isolated-session-running-p session)
        (%terminate-isolated-session session :worker-error)))))

#+sbcl
(defun %session-watchdog-loop (session)
  "Enforce the global deadline, cancellation predicate, and stderr ceiling."
  (loop while (isolated-session-running-p session)
        do (cond
             ((>= (get-internal-real-time)
                  (isolated-session-deadline session))
              (%terminate-isolated-session session :timeout))
             ((> (+ (isolated-session-output-bytes session)
                    (%file-size (isolated-session-stderr-path session)))
                 (isolation-policy-max-output-bytes
                  (isolated-session-policy session)))
              (%terminate-isolated-session session :output-limit))
             ((and (isolated-session-cancel-predicate session)
                   (handler-case
                       (funcall (isolated-session-cancel-predicate session))
                     (error () t)))
              (%terminate-isolated-session session :cancelled))
             ((not (sb-ext:process-alive-p
                    (isolated-session-process session)))
              (%finish-naturally-exited-session session))
             (t
              (sleep (/ (isolation-policy-poll-ms
                         (isolated-session-policy session))
                        1000.0))))))

#+sbcl
(defun start-isolated-session
    (argv &key (policy (make-isolation-policy)) cancel-predicate directory
               (environment nil environment-supplied-p))
  "Start one persistent, line-framed worker under POLICY.

The global wall deadline starts at launch.  CANCEL-PREDICATE, when supplied,
is polled by the supervisor.  Either condition kills and awaits the dedicated
process group even when the caller is not currently reading from the worker."
  (unless (%valid-argv-p argv)
    (error "isolated session must use a nonempty argv list"))
  (when (and cancel-predicate (not (functionp cancel-predicate)))
    (error "CANCEL-PREDICATE must be a function or NIL"))
  (let* ((stdout-path (%temporary-path "session-stdout"))
         (stderr-path (%temporary-path "session-stderr"))
         (stdout-copy
           (open stdout-path :direction :output :if-exists :supersede
                             :if-does-not-exist :create))
         (started (get-internal-real-time))
         (deadline (+ started
                      (round (* internal-time-units-per-second
                                (isolation-policy-timeout-ms policy))
                             1000)))
         process session)
    (handler-case
        (progn
          (let* ((wrapped (%wrapped-argv argv policy))
                 (launch-options
                   (list :input :stream :output :stream :error stderr-path
                         :search t :wait nil :directory directory)))
            (when environment-supplied-p
              (setf launch-options
                    (append launch-options (list :environment environment))))
            (setf process
                  (apply #'sb-ext:run-program
                         (first wrapped) (rest wrapped) launch-options)))
          (setf session
                (%make-isolated-session
                 :process process
                 :input (sb-ext:process-input process)
                 :output (sb-ext:process-output process)
                 :stdout-copy stdout-copy
                 :stdout-path stdout-path
                 :stderr-path stderr-path
                 :policy policy :started started :deadline deadline
                 :cancel-predicate cancel-predicate
                 :lock (sb-thread:make-mutex :name "isolated worker session")
                 :waitqueue (sb-thread:make-waitqueue
                             :name "isolated worker session events")))
          (setf (isolated-session-reader-thread session)
                (sb-thread:make-thread
                 (lambda () (%session-reader-loop session))
                 :name "isolated worker session reader")
                (isolated-session-watchdog-thread session)
                (sb-thread:make-thread
                 (lambda () (%session-watchdog-loop session))
                 :name "isolated worker session watchdog"))
          session)
      (error (condition)
        (when process (ignore-errors (%kill-worker process)))
        (ignore-errors (close stdout-copy))
        (ignore-errors (delete-file stdout-path))
        (ignore-errors (delete-file stderr-path))
        (error condition)))))

#+sbcl
(defun isolated-session-send-line (session line)
  "Send one newline-terminated frame to a running SESSION."
  (check-type session isolated-session)
  (unless (stringp line) (error "isolated session frame must be a string"))
  (sb-thread:with-mutex ((isolated-session-lock session))
    (unless (eq :running (isolated-session-status session))
      (error "isolated session is terminal: ~S"
             (isolated-session-status session)))
    (write-line line (isolated-session-input session))
    (finish-output (isolated-session-input session)))
  line)

#+sbcl
(defun isolated-session-read-line (session &key timeout-ms)
  "Return (values LINE :MESSAGE), or (values NIL TERMINAL-STATUS).

TIMEOUT-MS is an optional hard deadline for this read.  Expiry terminates the
whole worker process group; it is not a soft polling timeout."
  (check-type session isolated-session)
  (when (and timeout-ms (not (and (integerp timeout-ms) (plusp timeout-ms))))
    (error "TIMEOUT-MS must be a positive integer or NIL"))
  (let ((read-deadline
          (and timeout-ms
               (+ (get-internal-real-time)
                  (round (* internal-time-units-per-second timeout-ms) 1000)))))
    (loop
      (let ((timed-out-p nil))
        (sb-thread:with-mutex ((isolated-session-lock session))
          (when (isolated-session-lines session)
            (return-from isolated-session-read-line
              (values (pop (isolated-session-lines session)) :message)))
          (unless (eq :running (isolated-session-status session))
            (return-from isolated-session-read-line
              (values nil (isolated-session-status session))))
          (setf timed-out-p
                (and read-deadline
                     (>= (get-internal-real-time) read-deadline)))
          (unless timed-out-p
            (sb-thread:condition-wait
             (isolated-session-waitqueue session)
             (isolated-session-lock session)
             :timeout (/ (isolation-policy-poll-ms
                          (isolated-session-policy session))
                         1000.0))))
        (when timed-out-p
          (%terminate-isolated-session session :timeout)
          (return (values nil :timeout)))))))

#+sbcl
(defun cancel-isolated-session (session)
  "Cancel SESSION, kill its process group, and await process termination."
  (check-type session isolated-session)
  (%terminate-isolated-session session :cancelled)
  (isolated-session-status session))

#+sbcl
(defun %join-session-thread (thread)
  (when (and thread (not (eq thread sb-thread:*current-thread*)))
    (ignore-errors (sb-thread:join-thread thread))))

#+sbcl
(defun stop-isolated-session (session &key (reason :stopped) metadata)
  "Stop SESSION and return its stable privacy-safe terminal receipt.

REASON is used only when the session is still running.  Repeated calls return
the same receipt and never signal or launch another cleanup action."
  (check-type session isolated-session)
  (unless (member reason '(:stopped :cancelled) :test #'eq)
    (error "session stop reason must be :STOPPED or :CANCELLED"))
  (or (isolated-session-receipt session)
      (progn
        (%terminate-isolated-session session reason)
        ;; Kill the group even after a natural leader exit: a descendant must
        ;; not survive merely because the JSONL transport closed first.
        (ignore-errors
          (%signal-worker-group (isolated-session-process session) 9))
        (ignore-errors (close (isolated-session-input session)))
        (%join-session-thread (isolated-session-reader-thread session))
        (%join-session-thread (isolated-session-watchdog-thread session))
        (%close-session-streams session)
        (let* ((stdout
                 (%slurp (isolated-session-stdout-path session)
                         (isolation-policy-max-output-bytes
                          (isolated-session-policy session))))
               (stderr
                 (%slurp (isolated-session-stderr-path session)
                         (isolation-policy-max-output-bytes
                          (isolated-session-policy session))))
               (elapsed
                 (round (* 1000
                           (- (get-internal-real-time)
                              (isolated-session-started session)))
                        internal-time-units-per-second))
               (receipt
                 (append
                  (list :status (isolated-session-status session)
                        :elapsed-ms elapsed
                        :stdout-id (cid:content-id-long stdout)
                        :stderr-id (cid:content-id-long stderr)
                        :process-group-id
                        (sb-ext:process-pid
                         (isolated-session-process session))
                        :process-group-empty-p
                        (or (isolated-session-process-group-empty-p session)
                            (%process-group-empty-p
                             (sb-ext:process-pid
                              (isolated-session-process session))))
                        :policy
                        (isolation-policy->form
                         (isolated-session-policy session)))
                  metadata)))
          (setf (isolated-session-receipt session) receipt)
          (ignore-errors (delete-file (isolated-session-stdout-path session)))
          (ignore-errors (delete-file (isolated-session-stderr-path session)))
          receipt))))

#-sbcl
(progn
  (defstruct isolated-session status)
  (defun isolated-session-running-p (session)
    (declare (ignore session)) nil)
  (defun start-isolated-session (&rest arguments)
    (declare (ignore arguments))
    (error "persistent isolated sessions require SBCL"))
  (defun isolated-session-send-line (&rest arguments)
    (declare (ignore arguments))
    (error "persistent isolated sessions require SBCL"))
  (defun isolated-session-read-line (&rest arguments)
    (declare (ignore arguments)) (values nil :unsupported-runtime))
  (defun cancel-isolated-session (session)
    (declare (ignore session)) :unsupported-runtime)
  (defun stop-isolated-session (session &key reason metadata)
    (declare (ignore session reason metadata))
    (list :status :unsupported-runtime)))

#+sbcl
(defun run-isolated-command (argv &key (policy (make-isolation-policy))
                                      (marker "ROSETTE-ISOLATED-RESULT ")
                                      (accept #'identity) metadata
                                      receipt-parser
                                      (require-receipt t)
                                      terminate-on-ready-path)
  "Run ARGV under hard ceilings and return a privacy-safe tool envelope.

By default success requires a marked, CL-readable receipt accepted by ACCEPT.
RECEIPT-PARSER, if supplied, receives all text after the unique MARKER and
must parse and validate the entire payload with bounded resource use. Its
result goes to ACCEPT. Conditions fail closed. Without it the legacy Lisp
reader is used with read-eval disabled; that reader is NOT allocation-bounded
and is appropriate only for trusted producer syntax.
When REQUIRE-RECEIPT is NIL, a zero exit status is sufficient and the success
data is (:EXIT-CODE 0).  The latter is for silent compiler/linker phases whose
artifact is checked by a separately supervised execution."
  (unless (and (consp argv) (every (lambda (x) (and (stringp x) (plusp (length x)))) argv))
    (error "isolated command must be a nonempty argv list"))
  (unless (and (stringp marker) (plusp (length marker)) (functionp accept))
    (error "invalid isolated receipt parser"))
  (unless (or (null receipt-parser) (functionp receipt-parser))
    (error "RECEIPT-PARSER must be a function or NIL"))
  (unless (typep require-receipt 'boolean)
    (error "REQUIRE-RECEIPT must be a boolean, got ~S" require-receipt))
  (when (and terminate-on-ready-path
             (probe-file terminate-on-ready-path))
    (error "ready path already exists before isolated launch"))
  (let* ((out (%temporary-path "stdout")) (err (%temporary-path "stderr"))
         (started (get-internal-real-time))
         (deadline (+ started (round (* internal-time-units-per-second
                                        (isolation-policy-timeout-ms policy)) 1000)))
         status process ready-metadata)
    (unwind-protect
         (handler-case
             (progn
               (let ((wrapped (%wrapped-argv argv policy)))
                 (setf process (sb-ext:run-program
                                (first wrapped) (rest wrapped) :output out :error err
                                :search t :wait nil)))
               (loop while (sb-ext:process-alive-p process)
                     do (cond ((> (+ (%file-size out) (%file-size err))
                                   (isolation-policy-max-output-bytes policy))
                               (setf status :output-limit) (%kill-worker process) (return))
                              ((and terminate-on-ready-path
                                    (probe-file terminate-on-ready-path))
                               (let* ((process-id (sb-ext:process-pid process))
                                      (ready-size
                                        (%file-size terminate-on-ready-path))
                                      (ready-content-id
                                        (and (<= ready-size
                                                 (isolation-policy-max-output-bytes
                                                  policy))
                                             (cid:content-id-long
                                              (%slurp
                                               terminate-on-ready-path
                                               ready-size))))
                                      (signal-sent-p
                                        (and ready-content-id
                                             (%signal-worker-group process 9))))
                                 (when signal-sent-p
                                   (ignore-errors
                                     (sb-ext:process-wait process)))
                                 (setf status
                                       (if signal-sent-p
                                           :ready-killed
                                           :ready-kill-failed)
                                       ready-metadata
                                       (list
                                        :worker-process-id process-id
                                        :process-group-id process-id
                                        :ready-content-id ready-content-id
                                        :signal-sent-p
                                        (not (null signal-sent-p))
                                        :process-status
                                        (sb-ext:process-status process)
                                        :termination-signal
                                        (sb-ext:process-exit-code process)
                                        :exit-code
                                        (sb-ext:process-exit-code process)
                                        :process-group-empty-p
                                        (%await-process-group-empty process-id)))
                                 (unless signal-sent-p
                                   (%kill-worker process))
                                 (return)))
                              ((>= (get-internal-real-time) deadline)
                               (setf status :timeout) (%kill-worker process) (return))
                              (t (sleep (/ (isolation-policy-poll-ms policy) 1000.0)))))
               (unless status
                 (sb-ext:process-wait process)
                 ;; A short-lived process can finish between polling ticks.
                 ;; Recheck both files after wait so receipt-free phases cannot
                 ;; turn a fast output flood into a green zero-exit result.
                 (setf status
                       (cond
                         ((> (+ (%file-size out) (%file-size err))
                             (isolation-policy-max-output-bytes policy))
                          :output-limit)
                         ;; A worker may exit while the supervisor is sleeping
                         ;; between polls.  Completion observed after the hard
                         ;; deadline fails closed even when exit status is zero.
                         ((>= (get-internal-real-time) deadline) :timeout)
                         ((zerop (sb-ext:process-exit-code process)) :exited)
                         (t :worker-error))))
               (let* ((stdout (%slurp out (isolation-policy-max-output-bytes policy)))
                      (parsed (and (eq status :exited)
                                   require-receipt
                                   (%parse-result stdout marker accept receipt-parser)))
                      (accepted (and (eq status :exited)
                                     (or (not require-receipt) parsed)))
                      (elapsed (round (* 1000 (- (get-internal-real-time) started))
                                      internal-time-units-per-second))
                      (md (append metadata ready-metadata
                                  (list :elapsed-ms elapsed
                                        :receipt-required-p require-receipt
                                        :policy (isolation-policy->form policy)))))
                 (if accepted
                     (envelope:ok-envelope
                      :status :green
                      :data (if require-receipt parsed (list :exit-code 0))
                      :metadata md)
                     (envelope:error-envelope
                      :status (if (eq status :exited) :malformed-receipt status)
                      :data (list :stdout-id (cid:content-id-long stdout)
                                  :stderr-id
                                  (cid:content-id-long
                                   (%slurp err (isolation-policy-max-output-bytes policy))))
                      :metadata md))))
           (error (condition)
             (envelope:error-envelope :status :launch-error
                                      :data (list :condition (type-of condition))
                                      :metadata metadata)))
      (when (and process (sb-ext:process-alive-p process)) (%kill-worker process))
      (ignore-errors (delete-file out))
      (ignore-errors (delete-file err)))))

#-sbcl
(defun run-isolated-command
    (argv &key policy marker accept metadata require-receipt receipt-parser
               terminate-on-ready-path)
  (declare (ignore argv policy marker accept require-receipt receipt-parser
                   terminate-on-ready-path))
  (envelope:error-envelope :status :unsupported-runtime :metadata metadata))
