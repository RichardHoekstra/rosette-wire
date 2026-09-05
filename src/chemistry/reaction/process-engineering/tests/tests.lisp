;;;; tests.lisp --- Law tests for rosette-process-engineering.

(defpackage #:rosette-process-engineering/tests
  (:use #:cl #:rosette-process-engineering)
  (:export #:run-all-tests))

(in-package #:rosette-process-engineering/tests)

(defvar *assertions* 0)

(defmacro ok (form)
  `(progn
     (incf *assertions*)
     (unless ,form
       (error "Assertion failed: ~S" ',form))))

(defun approx= (a b &optional (tol 1d-9))
  (<= (abs (- a b)) tol))

(defun run-all-tests ()
  (setf *assertions* 0)
  (let* ((a (make-stream "A" '((:water . 10) (:salt . 1) (:water . 2))))
         (b (make-stream "B" '((:salt . 3) (:oil . 4))))
         (mixed (mix-streams "mixed" (list a b))))
    (ok (approx= (stream-component-flow a :water) 12d0))
    (ok (approx= (stream-total-flow mixed) 20d0))
    (ok (equal (mapcar #'car (stream-flows mixed)) '(:oil :salt :water)))
    (multiple-value-bind (head tail) (split-stream mixed 0.25d0)
      (ok (approx= (stream-total-flow head) 5d0))
      (ok (approx= (stream-total-flow tail) 15d0))
      (ok (mass-balance-closed-p
           (make-mass-balance "split" (list mixed) (list head tail))))
      (ok (mass-balance-component-closed-p
           (make-mass-balance "split" (list mixed) (list head tail))))))

  (let* ((inlet (make-stream "reactor in" '((:a . 10d0) (:inert . 5d0))))
         (outlet (conversion-reactor "reactor out" inlet :a 0.4d0
                                     '((:b . 0.5d0) (:c . 0.5d0)))))
    (ok (approx= (stream-component-flow outlet :a) 6d0))
    (ok (approx= (stream-component-flow outlet :b) 2d0))
    (ok (mass-balance-closed-p
         (make-mass-balance "reactor" (list inlet) (list outlet))))
    (ok (not (mass-balance-component-closed-p
              (make-mass-balance "reactor" (list inlet) (list outlet)))))
    (ok (approx= (mass-balance-component-residual
                  (make-mass-balance "reactor" (list inlet) (list outlet))
                  :a)
                 -4d0)))

  (let* ((wet (make-stream "wet biomass" '((:water . 18d0)
                                           (:volatile-solids . 12d0))))
         (products (yield-reactor "pyrolysis" wet
                                  '((:bio-oil . 0.45d0)
                                    (:biochar . 0.20d0)
                                    (:syngas . 0.20d0)))))
    (ok (approx= (stream-total-flow products) 25.5d0))
    (ok (not (mass-balance-closed-p
              (make-mass-balance "empirical yield" (list wet) (list products))))))

  (let ((duty (make-heat-duty "dryer" 1.5d0 :unit "MW" :note "demo")))
    (ok (equal (getf (heat-duty->plist duty) :unit) "MW")))

  (let* ((demand (make-heat-duty "dryer demand" 1.5d0 :unit "MW"))
         (source (make-heat-duty "synthesis heat" -1.5d0 :unit "MW"))
         (closed (make-heat-balance "closed heat" (list demand source)))
         (open (make-heat-balance "open heat" (list demand))))
    (ok (heat-balance-closed-p closed))
    (ok (not (heat-balance-closed-p open)))
    (ok (approx= (heat-balance-net-duty open) 1.5d0))
    (ok (approx= (heat-balance-demand-total closed) 1.5d0))
    (ok (approx= (heat-balance-source-total closed) -1.5d0))
    (ok (approx= (getf (heat-balance->plist closed) :demand-total)
                 1.5d0)))

  (let* ((emission (make-carbon-entry "calcination" 3d0
                                      :role :emission))
         (storage (make-carbon-entry "biochar" -4d0
                                     :role :sequestration))
         (ledger (make-carbon-ledger "carbon" (list emission storage))))
    (ok (carbon-ledger-improves-p ledger))
    (ok (approx= (carbon-ledger-net-burden ledger) -1d0))
    (ok (approx= (carbon-ledger-role-total ledger :emission) 3d0))
    (ok (approx= (cdr (assoc :sequestration
                             (carbon-ledger-role-breakdown ledger)))
                 -4d0))
    (ok (getf (carbon-ledger->plist ledger) :role-breakdown)))

  (let* ((revenue (make-economic-entry "methanol" 5d0
                                       :role :revenue))
         (cost (make-economic-entry "operations" -3d0
                                    :role :opex))
         (ledger (make-economic-ledger "daily value" (list revenue cost))))
    (ok (economic-ledger-viable-p ledger))
    (ok (approx= (economic-ledger-net-value ledger) 2d0))
    (ok (approx= (economic-ledger-role-total ledger :opex) -3d0))
    (ok (approx= (cdr (assoc :revenue
                             (economic-ledger-role-breakdown ledger)))
                 5d0))
    (ok (getf (economic-ledger->plist ledger) :role-breakdown)))

  (let* ((water (make-component :water :formula "H2O"))
         (methanol (make-component :methanol :formula "CH3OH")))
    (ok (approx= (formula-molecular-weight "H2O") 18.01528d0 1d-5))
    (ok (approx= (formula-molecular-weight "Ar") 39.948d0 1d-5))
    (ok (approx= (formula-molecular-weight "UO2") 270.02771d0 1d-5))
    (ok (approx= (formula-molecular-weight "Og") 294d0 1d-5))
    (ok (equal (parse-formula "CaCO3")
               '(("C" . 1) ("Ca" . 1) ("O" . 3))))
    (ok (approx= (component-molecular-weight water) 18.01528d0 1d-5))
    (ok (> (component-molecular-weight methanol)
           (component-molecular-weight water))))

  (let* ((acetone (make-component :acetone :formula "C3H6O"))
         (steam (make-component :water :formula "H2O" :phase :gas))
         (catalog (component-catalog-with *default-component-catalog*
                                          (list acetone steam))))
    (ok (approx= (catalog-molecular-weight catalog :acetone) 58.07914d0 1d-5))
    (ok (eq (component-phase (catalog-component catalog :water)) :gas))
    (ok (eq (component-phase (catalog-component *default-component-catalog*
                                                :water))
            :liquid)))

  (let* ((calcination (make-reaction
                       "calcination"
                       '((:caco3 . -1d0) (:cao . 1d0) (:co2 . 1d0))))
         (feed (make-stream "shells" '((:caco3 . 10d0))))
         (out (stoichiometric-reactor "kiln" feed calcination 1d0))
         (balance (make-mass-balance "kiln" (list feed) (list out))))
    (ok (approx= (reaction-balanced-mass-residual calcination) 0d0 1d-3))
    (ok (reaction-element-balanced-p calcination))
    (ok (reaction-balanced-p calcination))
    (ok (mass-balance-closed-p balance))
    (ok (> (stream-component-flow out :cao) 5d0))
    (ok (> (stream-component-flow out :co2) 4d0)))

  (let* ((methanol (make-reaction
                    "methanol synthesis"
                    '((:co . -1d0) (:h2 . -2d0) (:methanol . 1d0))))
         (feed (make-stream "syngas" '((:co . 28.0101d0)
                                       (:h2 . 4.03176d0)
                                       (:co2 . 2d0))))
         (out (stoichiometric-reactor "reactor" feed methanol 0.5d0))
         (balance (make-mass-balance "methanol" (list feed) (list out))))
    (ok (approx= (reaction-balanced-mass-residual methanol) 0d0 1d-3))
    (ok (equal (reaction-element-residuals methanol)
               '(("C" . 0.0d0) ("H" . 0.0d0) ("O" . 0.0d0))))
    (ok (reaction-balanced-p methanol))
    (ok (getf (reaction->plist methanol) :balanced-p))
    (ok (eq (reaction-limiting-reactant feed methanol) :co))
    (ok (approx= (reaction-reactant-extent feed methanol :co) 1000d0 1d-2))
    (ok (approx= (reaction-product-theoretical-yield feed methanol :methanol)
                 32.04186d0 1d-3))
    (ok (approx= (stream-component-flow out :methanol) 16.02093d0 1d-3))
    (ok (mass-balance-closed-p balance)))

  (let ((bad (make-reaction
              "bad methanol"
              '((:co . -1d0) (:h2 . -1d0) (:methanol . 1d0)))))
    (ok (not (reaction-element-balanced-p bad)))
    (ok (not (reaction-balanced-p bad)))
    (ok (assoc "H" (reaction-element-residuals bad) :test #'string=)))

  (let ((lumped (make-reaction
                 "lumped"
                 '((:volatile-solids . -1d0) (:biochar . 1d0)))))
    (ok (handler-case
            (progn (reaction-element-residuals lumped) nil)
          (error () t))))

  (let* ((analysis (make-biomass-analysis :moisture 0.50d0
                                          :ash 0.10d0
                                          :volatile-matter 0.30d0
                                          :fixed-carbon 0.10d0
                                          :carbon 0.45d0))
         (stream (biomass-stream-from-analysis "feed" 100d0 analysis)))
    (ok (approx= (biomass-dry-fraction analysis) 0.5d0))
    (ok (approx= (biomass-combustible-fraction analysis) 0.4d0))
    (ok (approx= (stream-component-flow stream :water) 50d0))
    (ok (approx= (stream-component-flow stream :ash) 10d0)))

  (let* ((gas (make-stream "syngas"
                           '((:h2 . 4.03176d0)
                             (:co . 28.0101d0)
                             (:co2 . 4.40095d0)
                             (:inerts . 2.8d0))))
         (quality (gas-quality-from-stream gas)))
    (ok (approx= (h2-co-ratio quality) 2d0 1d-4))
    (ok (> (syngas-module quality) 1.7d0))
    (ok (> (co2-fraction quality) 0d0))
    (ok (> (methanol-theoretical-max quality) 20d0)))

  (let* ((wet (make-stream "wet" '((:water . 50d0)
                                   (:volatile-matter . 30d0)
                                   (:fixed-carbon . 20d0))))
         (duty (dryer-duty "dryer" wet 0.10d0)))
    (ok (> (heat-duty-value duty) 1d0))
    (multiple-value-bind (dried vapor)
        (dryer-output-stream "dryer" wet 0.10d0)
      (ok (< (stream-component-flow dried :water) 6d0))
      (ok (> (stream-component-flow vapor :water) 40d0))
      (ok (mass-balance-closed-p
           (make-mass-balance "dryer" (list wet) (list dried vapor))))))

  (let ((feed (make-stream "mixed" '((:methanol . 10d0)
                                    (:water . 5d0)
                                    (:biochar . 2d0)))))
    (multiple-value-bind (distillate bottoms)
        (distillation-cut "column" feed '(:methanol) :fraction 0.9d0)
      (ok (> (stream-component-flow distillate :methanol) 8d0))
      (ok (> (stream-component-flow bottoms :water) 4d0)))
    (multiple-value-bind (solids liquid)
        (solids-separator "filter" feed '(:biochar))
      (ok (approx= (stream-component-flow solids :biochar) 2d0))
      (ok (approx= (stream-component-flow liquid :biochar) 0d0))))

  (let* ((yield (make-uncertain-parameter :methanol-yield
                                          0.40d0 0.50d0 0.60d0
                                          :unit :fraction))
         (cost (make-uncertain-parameter :opex
                                         4d0 5d0 6d0
                                         :unit "kEUR/d"))
         (cases (uncertainty-cases (list yield cost))))
    (ok (= (length cases) 3))
    (ok (approx= (getf (getf (second cases) :values) :methanol-yield)
                 0.50d0))
    (ok (approx= (uncertain-parameter-value yield :optimistic) 0.60d0)))

  (format t "~&rosette-process-engineering: ~D assertions, 0 failures.~%"
          *assertions*)
  t)
