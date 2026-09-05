;;;; tests.lisp --- Law tests for rosette-chemical-thermo.

(defpackage #:rosette-chemical-thermo/tests
  (:use #:cl #:rosette-chemical-thermo)
  (:local-nicknames (#:pe #:rosette-process-engineering)
                    (#:rn #:rosette-reaction-network))
  (:export #:run-all-tests))

(in-package #:rosette-chemical-thermo/tests)

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
  (let* ((methanol (pe:make-reaction
                    "methanol synthesis"
                    '((:co . -1d0) (:h2 . -2d0)
                      (:methanol . 1d0))))
         (step (rn:make-reaction-step "methanol reactor" methanol
                                      :conversion 0.5d0))
         (network (rn:make-reaction-network "methanol loop" (list step)))
         (feed (pe:make-stream "syngas"
                               '((:co . 28.0101d0)
                                 (:h2 . 4.03176d0))
                               :temperature 350d0))
         (duty (reaction-step-heat-duty feed step))
         (duties (reaction-network-heat-duties feed network))
         (balance (reaction-network-heat-balance
                   "methanol reaction heat" feed network)))
    (ok (thermo-catalog-p *default-thermo-catalog*))
    (ok (equal (thermo-catalog-name *default-thermo-catalog*)
               "small gas/vapor thermochemistry"))
    (ok (plusp (length (thermo-catalog-entries *default-thermo-catalog*))))
    (ok (stringp (thermo-catalog-note *default-thermo-catalog*)))
    (ok (component-thermo-p
         (thermo-catalog-property *default-thermo-catalog* :methanol)))
    (ok (eq (component-thermo-key
             (thermo-catalog-property *default-thermo-catalog* :methanol))
            :methanol))
    (ok (approx= (component-thermo-formation-enthalpy
                  (thermo-catalog-property *default-thermo-catalog* :methanol))
                 -201000d0))
    (ok (approx= (component-thermo-formation-gibbs
                  (thermo-catalog-property *default-thermo-catalog* :methanol))
                 -162000d0))
    (ok (approx= (component-thermo-specific-heat
                  (thermo-catalog-property *default-thermo-catalog* :methanol))
                 1560d0))
    (ok (getf (component-thermo->plist
               (thermo-catalog-property *default-thermo-catalog* :co))
              :formation-enthalpy-unit))
    (ok (equal (getf (thermo-catalog->plist *default-thermo-catalog*) :name)
               "small gas/vapor thermochemistry"))
    (ok (plusp (length (getf (thermo-catalog->plist *default-thermo-catalog*)
                             :entries))))
    (ok (approx= (reaction-standard-enthalpy methanol)
                 -90470d0
                 1d-6))
    (ok (approx= (reaction-standard-gibbs methanol)
                 -24840d0
                 1d-6))
    (ok (approx= (reaction-step-actual-extent feed step)
                 500d0
                 1d-9))
    (ok (pe:heat-duty-p duty))
    (ok (approx= (pe:heat-duty-value duty)
                 -45235000d0
                 1d-6))
    (ok (equal (pe:heat-duty-unit duty) "kJ/day"))
    (ok (approx= (getf (pe:heat-duty-note duty)
                       :standard-reaction-enthalpy)
                 -90470d0
                 1d-6))
    (ok (approx= (getf (pe:heat-duty-note duty) :extent)
                 500d0
                 1d-9))
    (ok (= (length duties) 1))
    (ok (approx= (pe:heat-duty-value (first duties))
                 (pe:heat-duty-value duty)
                 1d-9))
    (ok (approx= (pe:heat-balance-net-duty balance)
                 -45235000d0
                 1d-6))
    (ok (not (pe:heat-balance-closed-p balance)))
    (ok (equal (pe:heat-balance-name balance) "methanol reaction heat"))
    (ok (approx= (reaction-network-net-heat-duty feed network)
                 -45235000d0
                 1d-6))
    (ok (approx= (stream-sensible-enthalpy feed
                                           :reference-temperature 298.15d0)
                 (+ (* 28.0101d0 1040d0 (- 350d0 298.15d0))
                    (* 4.03176d0 14300d0 (- 350d0 298.15d0)))
                 1d-6))
    (ok (approx= (stream-sensible-enthalpy feed
                                           :reference-temperature 350d0)
                 0d0
                 1d-9))
    (ok (approx= (pe:heat-duty-value
                  (stream-heating-duty feed 400d0))
                 (+ (* 28.0101d0 1040d0 50d0)
                    (* 4.03176d0 14300d0 50d0))
                 1d-6))
    (ok (minusp (pe:heat-duty-value
                 (stream-heating-duty feed 300d0))))
    (ok (equal (pe:heat-duty-unit
                (stream-heating-duty feed 400d0 :unit "MJ/day"))
               "MJ/day"))
    (ok (eq (pe:heat-duty-note
             (stream-heating-duty feed 400d0 :note :sensible))
            :sensible)))

  (let* ((catalog (make-thermo-catalog
                   "partial"
                   (list (make-component-thermo :co
                                                :specific-heat 1040d0
                                                :note :only-cp))
                   :note :partial-catalog))
         (reaction (pe:make-reaction "missing hf"
                                     '((:co . -1d0) (:co2 . 1d0))))
         (stream (pe:make-stream "cold co" '((:co . 1d0))
                                 :temperature 300d0))
         (no-temperature (pe:make-stream "no temp" '((:co . 1d0)))))
    (ok (thermo-catalog-p catalog))
    (ok (equal (thermo-catalog-name catalog) "partial"))
    (ok (eq (thermo-catalog-note catalog) :partial-catalog))
    (ok (= (length (thermo-catalog-entries catalog)) 1))
    (ok (eq (component-thermo-note (thermo-catalog-property catalog :co))
            :only-cp))
    (ok (null (component-thermo-formation-enthalpy
               (thermo-catalog-property catalog :co))))
    (ok (null (component-thermo-formation-gibbs
               (thermo-catalog-property catalog :co))))
    (ok (signals-error-p
         (lambda ()
           (thermo-catalog-property catalog :co2))))
    (ok (signals-error-p
         (lambda ()
           (reaction-standard-enthalpy reaction :catalog catalog))))
    (ok (signals-error-p
         (lambda ()
           (reaction-standard-gibbs reaction :catalog catalog))))
    (ok (approx= (stream-sensible-enthalpy
                  stream :catalog catalog :reference-temperature 298d0)
                 2080d0
                 1d-9))
    (ok (signals-error-p
         (lambda ()
           (stream-sensible-enthalpy no-temperature :catalog catalog)))))

  (let* ((entry (make-component-thermo :x
                                       :formation-enthalpy 10
                                       :formation-gibbs 7
                                       :specific-heat 20
                                       :note :demo))
         (catalog (make-thermo-catalog "custom" (list entry)))
         (rxn (pe:make-reaction "x split"
                                '((:x . -2d0) (:x . 1d0)))))
    (ok (component-thermo-p entry))
    (ok (eq (component-thermo-key entry) :x))
    (ok (approx= (component-thermo-formation-enthalpy entry) 10d0))
    (ok (approx= (component-thermo-formation-gibbs entry) 7d0))
    (ok (approx= (component-thermo-specific-heat entry) 20d0))
    (ok (eq (getf (component-thermo->plist entry) :note) :demo))
    (ok (approx= (reaction-standard-enthalpy rxn :catalog catalog)
                 -10d0))
    (ok (approx= (reaction-standard-gibbs rxn :catalog catalog)
                 -7d0)))

  (let* ((rxn (pe:make-reaction
               "methanol synthesis"
               '((:co . -1d0) (:h2 . -2d0) (:methanol . 1d0))))
         (step (rn:make-reaction-step "full reactor" rxn))
         (network (rn:make-reaction-network "full loop" (list step)))
         (feed (pe:make-stream "full feed"
                               '((:co . 28.0101d0)
                                 (:h2 . 4.03176d0))
                               :temperature 298.15d0))
         (balance (reaction-network-heat-balance
                   "custom heat" feed network
                   :unit "MJ/day"
                   :note :heat-only)))
    (ok (= (length (reaction-network-heat-duties feed network
                                                 :unit "MJ/day"))
           1))
    (ok (equal (pe:heat-duty-unit
                (first (reaction-network-heat-duties feed network
                                                     :unit "MJ/day")))
               "MJ/day"))
    (ok (approx= (reaction-network-net-heat-duty feed network)
                 -90470000d0
                 1d-6))
    (ok (equal (pe:heat-balance-unit balance) "MJ/day"))
    (ok (eq (pe:heat-balance-note balance) :heat-only)))

  (ok (signals-error-p
       (lambda () (make-component-thermo :bad :formation-enthalpy "hot"))))
  (ok (signals-error-p
       (lambda () (make-component-thermo :bad :formation-gibbs "free"))))
  (ok (signals-error-p
       (lambda () (make-component-thermo :bad :specific-heat "warm"))))
  (ok (signals-error-p
       (lambda () (make-thermo-catalog 42 nil))))
  (ok (signals-error-p
       (lambda () (make-thermo-catalog "bad" (list :not-thermo)))))
  (ok (signals-error-p
       (lambda ()
         (stream-heating-duty
          (pe:make-stream "bad target" '((:co . 1d0)) :temperature 300d0)
          "hot"))))

  (format t "~&rosette-chemical-thermo: ~D assertions, 0 failures.~%"
          *assertions*)
  t)
