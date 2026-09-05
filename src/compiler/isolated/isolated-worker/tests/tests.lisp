(defpackage #:rosette-isolated-worker/tests
  (:use #:cl)
  (:local-nicknames (#:w #:rosette-isolated-worker)
                    (#:e #:rosette-tool-envelope)
                    (#:cid #:rosette-content-identity))
  (:export #:run-all-tests))
(in-package #:rosette-isolated-worker/tests)

(defun test-policy (&rest arguments)
  "Use a host-compatible absolute UID process ceiling in worker tests."
  (apply #'w:make-isolation-policy
         (append arguments (list :max-processes 4096))))

(defun run-all-tests ()
  (let ((n 0) (fails 0))
    (flet ((gate (name value) (incf n) (format t "~&  [~A] ~A~%" (if value "PASS" "FAIL") name)
             (unless value (incf fails))))
      (let ((ok (w:run-isolated-command
                 (list "printf"
                       (format nil "ROSETTE-ISOLATED-RESULT (:ok t :value 7)~%"))
                 :policy (test-policy)
                 :accept (lambda (x) (and (eq t (getf x :ok)) x)))))
        (gate "compact accepted receipt crosses isolation boundary"
              (and (e:tool-envelope-ok ok) (= 7 (getf (e:tool-envelope-data ok) :value)))))
      (let ((called nil))
        (let ((parsed
                (w:run-isolated-command
                 '("printf" "ROSETTE-ISOLATED-RESULT #1000000000000(0)")
                 :policy (test-policy)
                 :receipt-parser (lambda (text)
                                   (setf called text)
                                   '(:decoded t)))))
          (gate "custom parser receives raw payload without invoking Lisp reader"
                (and (equal called "#1000000000000(0)")
                     (e:tool-envelope-ok parsed)
                     (equal '(:decoded t) (e:tool-envelope-data parsed))))))
      (let ((bad (w:run-isolated-command
                  '("printf" "ROSETTE-ISOLATED-RESULT rejected")
                  :policy (test-policy)
                  :receipt-parser (lambda (text) (declare (ignore text))
                                    (error "decoder refusal")))))
        (gate "custom decoder errors fail closed"
              (eq :malformed-receipt (e:tool-envelope-status bad))))
      (let* ((inherit-policy (test-policy :max-processes :inherit))
             (inherit
               (w:run-isolated-command
                (list "printf"
                      (format nil "ROSETTE-ISOLATED-RESULT (:inherit-ok t)~%"))
                :policy inherit-policy))
             (recorded (getf (e:tool-envelope-metadata inherit) :policy)))
        (gate "explicit inherited outer NPROC is typed and executable"
              (and (e:tool-envelope-ok inherit)
                   (eq t (getf (e:tool-envelope-data inherit) :inherit-ok))
                   (eq :inherit (getf recorded :max-processes)))))
      (let ((timed (w:run-isolated-command
                    '("sleep" "1")
                    :policy (test-policy :timeout-ms 30 :poll-ms 5))))
        (gate "wall timeout kills worker and fails closed"
              (eq :timeout (e:tool-envelope-status timed))))
      (let ((session
              (w:start-isolated-session
               '("sh" "-c" "while IFS= read -r line; do printf 'ack:%s\\n' \"$line\"; done")
               :policy (test-policy :timeout-ms 2000 :poll-ms 5))))
        (unwind-protect
             (progn
               (w:isolated-session-send-line session "alpha")
               (multiple-value-bind (line status)
                   (w:isolated-session-read-line session :timeout-ms 500)
                 (gate "persistent worker exchanges a bounded line frame"
                       (and (eq :message status)
                            (string= "ack:alpha" line)))))
          (let ((receipt (w:stop-isolated-session session)))
            (gate "explicit session stop awaits an empty process group"
                  (and (eq :stopped (getf receipt :status))
                       (getf receipt :process-group-empty-p)
                       (stringp (getf receipt :stdout-id))
                       (stringp (getf receipt :stderr-id)))))))
      (let ((session
              (w:start-isolated-session
               '("sh" "-c" "trap '' TERM; while :; do sleep 1; done")
               :policy (test-policy :timeout-ms 35 :poll-ms 5))))
        (multiple-value-bind (line status)
            (w:isolated-session-read-line session :timeout-ms 500)
          (declare (ignore line))
          (let ((receipt (w:stop-isolated-session session)))
            (gate "persistent global deadline kills and awaits the worker group"
                  (and (eq :timeout status)
                       (eq :timeout (getf receipt :status))
                       (getf receipt :process-group-empty-p))))))
      (let ((cancel-p nil))
        (let ((session
                (w:start-isolated-session
                 '("sh" "-c" "while :; do sleep 1; done")
                 :policy (test-policy :timeout-ms 2000 :poll-ms 5)
                 :cancel-predicate (lambda () cancel-p))))
          (setf cancel-p t)
          (loop repeat 100
                while (w:isolated-session-running-p session)
                do (sleep 0.005))
          (let ((receipt (w:stop-isolated-session session)))
            (gate "caller cancellation is enforced without an active read"
                  (and (eq :cancelled (getf receipt :status))
                       (getf receipt :process-group-empty-p))))))
      (let ((session
              (w:start-isolated-session
               '("yes" "overflow")
               :policy (test-policy :timeout-ms 2000 :poll-ms 5
                                    :max-output-bytes 64))))
        (loop repeat 100
              while (w:isolated-session-running-p session)
              do (sleep 0.005))
        (let ((receipt-a (w:stop-isolated-session session))
              (receipt-b (w:stop-isolated-session session)))
          (gate "persistent output flood is bounded and terminal"
                (and (eq :output-limit (getf receipt-a :status))
                     (getf receipt-a :process-group-empty-p)))
          (gate "persistent terminal receipt is stable across repeated stop"
                (eq receipt-a receipt-b))))
      (let ((flood (w:run-isolated-command
                    '("printf" "abcdefghijklmnopqrstuvwxyz")
                    :policy (test-policy :max-output-bytes 8))))
        (gate "output ceiling fails closed" (not (e:tool-envelope-ok flood))))
      (let ((silent-a (w:run-isolated-command
                       '("true") :policy (test-policy) :require-receipt nil))
            (silent-b (w:run-isolated-command
                       '("true") :policy (test-policy) :require-receipt nil)))
        (gate "silent zero-exit compiler phase may opt out of receipts"
              (and (e:tool-envelope-ok silent-a)
                   (equal '(:exit-code 0) (e:tool-envelope-data silent-a))))
        (setf (second (e:tool-envelope-data silent-a)) 99)
        (gate "receipt-free success data is fresh per envelope"
              (and (not (eq (e:tool-envelope-data silent-a)
                            (e:tool-envelope-data silent-b)))
                   (equal '(:exit-code 0)
                          (e:tool-envelope-data silent-b)))))
      (let ((failed (w:run-isolated-command
                     '("false") :policy (test-policy) :require-receipt nil)))
        (gate "receipt-free mode still fails closed on nonzero exit"
              (and (not (e:tool-envelope-ok failed))
                   (eq :worker-error (e:tool-envelope-status failed)))))
      (let ((trailing
              (w:run-isolated-command
               (list "printf"
                     (format nil "ROSETTE-ISOLATED-RESULT (:ok t) trailing~%"))
               :policy (test-policy))))
        (gate "non-whitespace after a receipt is rejected"
              (and (not (e:tool-envelope-ok trailing))
                   (eq :malformed-receipt
                       (e:tool-envelope-status trailing)))))
      (let ((duplicate
              (w:run-isolated-command
               (list "printf"
                     (format nil "ROSETTE-ISOLATED-RESULT (:ok t)~%~
                                  ROSETTE-ISOLATED-RESULT (:ok t)~%"))
               :policy (test-policy))))
        (gate "duplicate receipt markers are rejected"
              (and (not (e:tool-envelope-ok duplicate))
                   (eq :malformed-receipt
                       (e:tool-envelope-status duplicate)))))
      (let ((unreadable
              (w:run-isolated-command
               (list "printf"
                     (format nil "ROSETTE-ISOLATED-RESULT (#.forbidden)~%"))
               :policy (test-policy))))
        (gate "reader syntax cannot escape malformed-receipt classification"
              (and (not (e:tool-envelope-ok unreadable))
                   (eq :malformed-receipt
                       (e:tool-envelope-status unreadable)))))
      (let ((fast-flood
              (w:run-isolated-command
               '("printf" "abcdefghijklmnopqrstuvwxyz")
               :policy (test-policy :max-output-bytes 8 :poll-ms 1000)
               :require-receipt nil)))
        (gate "receipt-free fast output flood fails at the output ceiling"
              (and (not (e:tool-envelope-ok fast-flood))
                   (eq :output-limit (e:tool-envelope-status fast-flood)))))
      (let ((timed-out
              (w:run-isolated-command
               '("sleep" "1")
               :policy (test-policy :timeout-ms 10 :poll-ms 5)
               :require-receipt nil)))
        (gate "receipt-free mode still fails closed on timeout"
              (and (not (e:tool-envelope-ok timed-out))
                   (eq :timeout (e:tool-envelope-status timed-out)))))
      (let ((between-ticks
              (w:run-isolated-command
               '("sleep" "0.2")
               :policy (test-policy :timeout-ms 100 :poll-ms 1000)
               :require-receipt nil)))
        (gate "zero exit observed after deadline still times out"
              (and (not (e:tool-envelope-ok between-ticks))
                   (eq :timeout (e:tool-envelope-status between-ticks)))))
      (let ((probe
              (merge-pathnames
               (format nil "rosette-isolated-worker-descendant-~36R.tmp"
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
        (unwind-protect
             (let ((timed-out
                     (w:run-isolated-command
                      (list "sh" "-c"
                            "(sleep 0.2; touch \"$1\") & wait"
                            "rosette-isolation-probe"
                            (uiop:native-namestring probe))
                      :policy (test-policy :timeout-ms 20 :poll-ms 5)
                      :require-receipt nil)))
               ;; Wait past the child's delayed side effect. Killing only the
               ;; tracked shell leaves its background subshell able to create
               ;; PROBE; only process-group termination suppresses it.
               (sleep 0.35)
               (gate "timeout terminates the isolated worker process group"
                     (and (not (e:tool-envelope-ok timed-out))
                          (eq :timeout (e:tool-envelope-status timed-out))
                          (not (probe-file probe)))))
          (when (probe-file probe)
            (delete-file probe))))
      ;; A live group must never become cleanup evidence merely by waiting.
      (let ((live (sb-ext:run-program "sleep" '("10") :search t :wait nil)))
        (unwind-protect
             (gate "cleanup deadline refuses a still-live process group"
                   (not (w::%await-process-group-empty
                         (sb-ext:process-pid live) 20)))
          (w::%kill-worker live)))
      (loop repeat 20 do
        (let* ((ready
                 (merge-pathnames
                  (format nil "rosette-isolated-worker-ready-~36R.sexp"
                          (random most-positive-fixnum))
                  (uiop:temporary-directory)))
               (temporary
                 (make-pathname :name
                                (format nil "~A-tmp" (pathname-name ready))
                                :type (pathname-type ready) :defaults ready))
               (content "(:kind :durable-ready :step 3)\n"))
          (unwind-protect
               (let* ((killed
                        (w:run-isolated-command
                         (list "sh" "-c"
                               "printf '%s' \"$2\" > \"$1\"; mv \"$1\" \"$3\"; trap '' TERM; while :; do sleep 1; done"
                               "rosette-ready-worker"
                               (uiop:native-namestring temporary)
                               content
                               (uiop:native-namestring ready))
                         :policy (test-policy :timeout-ms 2000 :poll-ms 5)
                         :require-receipt nil
                         :terminate-on-ready-path ready))
                      (metadata (e:tool-envelope-metadata killed)))
                 (gate "durable ready boundary triggers observed process-group SIGKILL"
                       (and (eq :ready-killed
                                (e:tool-envelope-status killed))
                            (= 9 (getf metadata :termination-signal -1))
                            (= 9 (getf metadata :exit-code -1))
                            (eq :signaled (getf metadata :process-status))
                            (getf metadata :signal-sent-p)
                            (getf metadata :process-group-empty-p)
                            (= (getf metadata :worker-process-id)
                               (getf metadata :process-group-id))
                            (string=
                             (getf metadata :ready-content-id)
                             (cid:content-id-long content)))))
            (when (probe-file temporary) (delete-file temporary))
            (when (probe-file ready) (delete-file ready)))))
      (format t "~&==== rosette-isolated-worker: ~D gates, ~D failures ====~%" n fails)
      (assert (zerop fails)) t)))
