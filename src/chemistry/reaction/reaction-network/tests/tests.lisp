;;;; tests.lisp --- Law tests for rosette-reaction-network.

(defpackage #:rosette-reaction-network/tests
  (:use #:cl #:rosette-reaction-network)
  (:local-nicknames (#:pe #:rosette-process-engineering))
  (:export #:run-all-tests))

(in-package #:rosette-reaction-network/tests)

(defvar *assertions* 0)

(defmacro ok (form)
  `(progn
     (incf *assertions*)
     (unless ,form
       (error "Assertion failed: ~S" ',form))))

(defun approx= (a b &optional (tol 1d-9))
  (<= (abs (- a b)) tol))

(defun signals-error-p (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error () t)))

(defun run-all-tests ()
  (setf *assertions* 0)
  (let* ((shift (pe:make-reaction
                 "water gas shift"
                 '((:co . -1d0) (:water . -1d0)
                   (:co2 . 1d0) (:h2 . 1d0))))
         (methanol (pe:make-reaction
                    "methanol synthesis"
                    '((:co . -1d0) (:h2 . -2d0)
                      (:methanol . 1d0))))
         (network (make-reaction-network
                   "shift plus methanol"
                   (list
                    (make-reaction-step "shift reactor" shift)
                    (make-reaction-step "methanol loop" methanol))))
         (feed (pe:make-stream "syngas"
                               '((:co . 56.0202d0)
                                 (:water . 18.01528d0))))
         (report (run-reaction-network-report "methanol outlet"
                                              feed network))
         (out (reaction-network-report-outlet report))
         (step-reports (reaction-network-report-step-reports report))
         (mass (pe:make-mass-balance "mass" (list feed) (list out)))
         (elements (make-element-balance "elements" (list feed) (list out))))
    (ok (equal (reaction-network-name network) "shift plus methanol"))
    (ok (reaction-network-p network))
    (ok (= (length (reaction-network-steps network)) 2))
    (ok (null (reaction-network-note network)))
    (ok (= (length (reaction-network-reactions network)) 2))
    (ok (every #'pe:reaction-p (reaction-network-reactions network)))
    (ok (reaction-step-p (first (reaction-network-steps network))))
    (ok (equal (reaction-step-name (first (reaction-network-steps network)))
               "shift reactor"))
    (ok (eq (reaction-step-reaction (first (reaction-network-steps network)))
            shift))
    (ok (approx= (reaction-step-conversion
                  (first (reaction-network-steps network)))
                 1d0))
    (ok (null (reaction-step-note (first (reaction-network-steps network)))))
    (ok (reaction-step-balanced-p (first (reaction-network-steps network))))
    (ok (reaction-network-balanced-p network))
    (ok (null (reaction-network-invalid-steps network)))
    (ok (getf (reaction-network-balance-report network) :balanced-p))
    (ok (= (length (getf (reaction-network-balance-report network) :steps)) 2))
    (ok (equal (getf (reaction-step->plist
                      (first (reaction-network-steps network)))
                     :name)
               "shift reactor"))
    (ok (= (length step-reports) 2))
    (ok (reaction-network-report-p report))
    (ok (equal (reaction-network-report-name report) "methanol outlet"))
    (ok (eq (reaction-network-report-network report) network))
    (ok (eq (reaction-network-report-inlet report) feed))
    (ok (eq (reaction-network-report-outlet report) out))
    (ok (eq (reaction-step-report-limiting-reactant (first step-reports))
            :water))
    (ok (eq (reaction-step-report-limiting-reactant (second step-reports))
            :h2))
    (ok (reaction-step-report-p (first step-reports)))
    (ok (equal (reaction-step-report-name (first step-reports))
               "shift reactor"))
    (ok (eq (reaction-step-report-step (first step-reports))
            (first (reaction-network-steps network))))
    (ok (eq (reaction-step-report-inlet (first step-reports)) feed))
    (ok (pe:stream-p (reaction-step-report-outlet (first step-reports))))
    (ok (> (reaction-step-report-limiting-extent (first step-reports)) 0d0))
    (ok (pe:mass-balance-p
         (reaction-step-report-mass-balance (first step-reports))))
    (ok (element-balance-p
         (reaction-step-report-element-balance (first step-reports))))
    (ok (approx= (pe:stream-component-flow out :methanol) 16.02093d0 1d-3))
    (ok (equal (pe:stream-name out) "methanol outlet"))
    (ok (pe:mass-balance-closed-p mass))
    (ok (pe:mass-balance-closed-p
         (reaction-network-report-mass-balance report)))
    (ok (element-balance-closed-p elements))
    (ok (element-balance-closed-p
         (reaction-network-report-element-balance report)))
    (ok (element-balance-closed-p elements "C"))
    (ok (element-balance-p elements))
    (ok (equal (element-balance-name elements) "elements"))
    (ok (equal (mapcar #'car (element-balance-input-elements elements))
               '("C" "H" "O")))
    (ok (equal (mapcar #'car (element-balance-output-elements elements))
               '("C" "H" "O")))
    (ok (equal (mapcar #'car (element-balance-residuals elements))
               '("C" "H" "O")))
    (ok (approx= (element-balance-tolerance elements)
                 pe:+default-balance-tolerance+))
    (ok (null (element-balance-note elements)))
    (ok (approx= (element-balance-residual elements "H") 0d0 1d-9))
    (ok (approx= (element-balance-residual elements "missing") 0d0 1d-9))
    (ok (approx= (cdr (assoc :methanol
                             (reaction-step-report-theoretical-yields
                              (second step-reports))))
                 16.02093d0 1d-3))
    (ok (getf (reaction-step-report->plist (first step-reports))
              :element-balance))
    (ok (getf (getf (reaction-network->plist network)
                    :reaction-balance)
              :balanced-p))
    (ok (getf (reaction-network-report->plist report) :element-balance))
    (ok (equal (getf (reaction-network-report->plist report) :name)
               "methanol outlet"))
    (ok (= (length (getf (reaction-network-report->plist report)
                         :step-reports))
           2)))

  (let* ((rxn (pe:make-reaction
               "methanol synthesis"
               '((:co . -1d0) (:h2 . -2d0) (:methanol . 1d0))))
         (step (make-reaction-step "half conversion" rxn
                                   :conversion 0.5d0
                                   :note :partial))
         (network (make-reaction-network "single step" (list step)
                                         :note :demo))
         (feed (pe:make-stream "feed"
                               '((:co . 28.0101d0)
                                 (:h2 . 4.03176d0))))
         (out (run-reaction-network "out" feed network :note :outlet)))
    (ok (eq (reaction-step-note step) :partial))
    (ok (eq (reaction-network-note network) :demo))
    (ok (approx= (reaction-step-conversion step) 0.5d0))
    (ok (approx= (pe:stream-component-flow out :methanol)
                 16.02093d0
                 1d-3))
    (ok (approx= (pe:stream-component-flow out :co)
                 14.00505d0
                 1d-3))
    (ok (eq (pe:stream-note out) :outlet)))

  (let* ((water (pe:make-stream "water" '((:water . 18.01528d0))))
         (flows (stream-element-flows water)))
    (ok (equal (mapcar #'car flows) '("H" "O")))
    (ok (approx= (cdr (assoc "H" flows :test #'string=))
                 2.01588d0
                 1d-4))
    (ok (approx= (cdr (assoc "O" flows :test #'string=))
                 15.9994d0
                 1d-4)))

  (let* ((stream (pe:make-stream "water" '((:water . 18.01528d0))))
         (balance (make-element-balance "self" (list stream) (list stream)
                                        :tolerance 1d-6
                                        :note :same)))
    (ok (element-balance-closed-p balance))
    (ok (element-balance-closed-p balance "H"))
    (ok (approx= (element-balance-tolerance balance) 1d-6))
    (ok (eq (element-balance-note balance) :same))
    (ok (getf (element-balance->plist balance) :closed-p)))

  (let* ((bad (pe:make-reaction
               "bad methanol"
               '((:co . -1d0) (:h2 . -1d0) (:methanol . 1d0))))
         (network (make-reaction-network
                   "invalid network"
                   (list (make-reaction-step "bad loop" bad))))
         (balance (reaction-network-balance-report network)))
    (ok (not (reaction-network-balanced-p network)))
    (ok (equal (reaction-network-invalid-steps network)
               '("bad loop")))
    (ok (not (getf balance :balanced-p)))
    (ok (equal (getf balance :invalid-steps)
               '("bad loop"))))

  (let ((stream (pe:make-stream "unformulated" '((:inerts . 1d0)))))
    (ok (signals-error-p
         (lambda ()
           (stream-element-flows stream))))
    (ok (null (stream-element-flows stream :on-missing-formula :ignore))))

  (ok (signals-error-p
       (lambda ()
         (make-reaction-step
          "bad conversion"
          (pe:make-reaction "noop" '((:co . -1d0) (:co . 1d0)))
          :conversion 2d0))))
  (ok (signals-error-p
       (lambda ()
         (make-reaction-network "bad network" (list :not-a-step)))))
  (ok (signals-error-p
       (lambda ()
         (make-element-balance "bad input" (list :not-a-stream) nil))))

  (format t "~&rosette-reaction-network: ~D assertions, 0 failures.~%"
          *assertions*)
  t)
