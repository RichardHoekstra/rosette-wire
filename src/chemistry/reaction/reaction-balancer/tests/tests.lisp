;;;; tests.lisp --- Tests for rosette-reaction-balancer.

(defpackage #:rosette-reaction-balancer/tests
  (:use #:cl #:rosette-reaction-balancer)
  (:local-nicknames (#:pe #:rosette-process-engineering))
  (:export #:run-all-tests))

(in-package #:rosette-reaction-balancer/tests)

(defvar *assertions* 0)

(defmacro ok (form)
  `(progn
     (incf *assertions*)
     (unless ,form
       (error "Assertion failed: ~S" ',form))))

(defun signless-stoich (reaction)
  (sort (mapcar (lambda (pair)
                  (cons (car pair) (round (cdr pair))))
                (pe:reaction-stoich reaction))
        #'string<
        :key (lambda (pair) (symbol-name (car pair)))))

(defun zero-residuals-p (residuals)
  (every (lambda (pair) (zerop (cdr pair))) residuals))

(defun run-all-tests ()
  (setf *assertions* 0)
  (let* ((balance (balance-reaction
                   "methanol synthesis"
                   '(:co :h2)
                   '(:methanol)
                   :note "demo note"))
         (reaction (reaction-balance-reaction balance))
         (plist (reaction-balance->plist balance)))
    (ok (reaction-balance-p balance))
    (ok (string= (reaction-balance-name balance) "methanol synthesis"))
    (ok (equal (reaction-balance-reactants balance) '(:co :h2)))
    (ok (equal (reaction-balance-products balance) '(:methanol)))
    (ok (equal (reaction-balance-elements balance) '("C" "H" "O")))
    (ok (equal (reaction-balance-coefficients balance)
               '(1 2 1)))
    (ok (equal (reaction-balance-stoich balance)
               '((:co . -1) (:h2 . -2) (:methanol . 1))))
    (ok (zero-residuals-p (reaction-balance-residuals balance)))
    (ok (string= (reaction-balance-note balance) "demo note"))
    (ok (equal (signless-stoich reaction)
               '((:CO . -1) (:H2 . -2) (:METHANOL . 1))))
    (ok (string= (pe:reaction-name reaction) "methanol synthesis"))
    (ok (string= (pe:reaction-note reaction) "demo note"))
    (ok (equal (signless-stoich reaction)
               (signless-stoich (reaction-balance-reaction balance))))
    (ok (reaction-balance-balanced-p balance))
    (ok (pe:reaction-balanced-p reaction))
    (ok (getf plist :balanced-p))
    (ok (equal (getf plist :name) "methanol synthesis"))
    (ok (equal (getf plist :reactants) '(:co :h2)))
    (ok (equal (getf plist :products) '(:methanol)))
    (ok (equal (getf plist :coefficients) '(1 2 1)))
    (ok (equal (getf plist :elements) '("C" "H" "O")))
    (ok (zero-residuals-p (getf plist :residuals)))
    (ok (equal (getf plist :note) "demo note"))
    (ok (pe:reaction-p (reaction-balance-reaction balance))))

  (let* ((balance (balance-reaction
                   "steam methane reforming"
                   '(:ch4 :water)
                   '(:co :h2))))
    (ok (equal (reaction-balance-coefficients balance)
               '(1 1 1 3)))
    (ok (equal (reaction-balance-stoich balance)
               '((:ch4 . -1) (:water . -1) (:co . 1) (:h2 . 3))))
    (ok (equal (reaction-balance-elements balance) '("C" "H" "O")))
    (ok (zero-residuals-p (reaction-balance-residuals balance)))
    (ok (reaction-balance-balanced-p balance))
    (ok (equal (mapcar #'car (reaction-balance-residuals balance))
               '("C" "H" "O"))))

  (let* ((balance (balance-reaction
                   "lime calcination"
                   '(:caco3)
                   '(:cao :co2))))
    (ok (equal (reaction-balance-coefficients balance)
               '(1 1 1)))
    (ok (equal (reaction-balance-stoich balance)
               '((:caco3 . -1) (:cao . 1) (:co2 . 1))))
    (ok (equal (reaction-balance-elements balance) '("C" "Ca" "O")))
    (ok (zero-residuals-p (reaction-balance-residuals balance)))
    (ok (reaction-balance-balanced-p balance)))

  (let* ((catalog
           (pe:component-catalog-with
            pe:*default-component-catalog*
            (list (pe:make-component :acetone
                                     :formula "C3H6O"
                                     :phase :liquid)
                  (pe:make-component :o2
                                     :formula "O2"
                                     :phase :gas))))
         (balance (balance-reaction
                   "acetone combustion"
                   '(:acetone :o2)
                   '(:co2 :water)
                   :catalog catalog)))
    (ok (equal (reaction-balance-coefficients balance)
               '(1 4 3 3)))
    (ok (equal (reaction-balance-elements balance) '("C" "H" "O")))
    (ok (zero-residuals-p (reaction-balance-residuals balance)))
    (ok (reaction-balance-balanced-p balance))
    (ok (pe:reaction-balanced-p (reaction-balance-reaction balance)
                                :catalog catalog)))

  (let* ((catalog
           (pe:component-catalog-with
            pe:*default-component-catalog*
            (list (pe:make-component :o2
                                     :formula "O2"
                                     :phase :gas))))
         (balance (balance-reaction
                   "water formation"
                   '("h2" :o2)
                   '("water")
                   :catalog catalog)))
    (ok (equal (reaction-balance-reactants balance) '(:H2 :O2)))
    (ok (equal (reaction-balance-products balance) '(:WATER)))
    (ok (equal (reaction-balance-coefficients balance) '(2 1 2)))
    (ok (equal (reaction-balance-stoich balance)
               '((:H2 . -2) (:O2 . -1) (:WATER . 2))))
    (ok (zero-residuals-p (reaction-balance-residuals balance)))
    (ok (reaction-balance-balanced-p balance)))

  (let ((failed-p nil))
    (handler-case
        (balance-reaction "unformulated solid"
                          '(:biochar)
                          '(:co2))
      (error ()
        (setf failed-p t)))
    (ok failed-p))

  (ok (handler-case
          (progn (balance-reaction "empty reactants" nil '(:co2)) nil)
        (error () t)))
  (ok (handler-case
          (progn (balance-reaction "empty products" '(:co) nil) nil)
        (error () t)))
  (ok (handler-case
          (progn (balance-reaction "duplicate side" '(:co :co) '(:co2)) nil)
        (error () t)))
  (ok (handler-case
          (progn (balance-reaction "both sides" '(:co) '(:co)) nil)
        (error () t)))
  (ok (handler-case
          (progn (balance-reaction "missing component" '(:missing) '(:co2)) nil)
        (error () t)))
  (ok (handler-case
          (progn (balance-reaction 42 '(:co) '(:co2)) nil)
        (error () t)))

  (format t "~&rosette-reaction-balancer: ~D assertions, 0 failures.~%"
          *assertions*)
  t)
