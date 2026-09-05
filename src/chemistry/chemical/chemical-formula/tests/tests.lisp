;;;; tests.lisp --- Tests for rosette-chemical-formula.

(defpackage #:rosette-chemical-formula/tests
  (:use #:cl #:rosette-chemical-formula)
  (:export #:run-all-tests))

(in-package #:rosette-chemical-formula/tests)

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
  (ok (= (length +periodic-table+) 118))
  (ok (= (length (element-symbols)) 118))
  (ok (= (length +atomic-weights+) 118))
  (ok (every #'chemical-element-p +periodic-table+))
  (ok (every #'stringp (element-symbols)))
  (ok (equal (mapcar #'car +atomic-weights+) (element-symbols)))
  (ok (equal (subseq (element-symbols) 0 4)
             '("H" "He" "Li" "Be")))
  (ok (equal (subseq (last (element-symbols) 4) 0)
             '("Mc" "Lv" "Ts" "Og")))
  (ok (= (chemical-element-atomic-number (find-element "C")) 6))
  (ok (= (chemical-element-atomic-number (find-element "carbon")) 6))
  (ok (= (chemical-element-atomic-number (find-element "CARBON")) 6))
  (ok (= (chemical-element-atomic-number (find-element :c)) 6))
  (ok (eq (find-element (find-element "C")) (find-element "C")))
  (ok (string= (chemical-element-symbol (find-element 79)) "Au"))
  (ok (string= (chemical-element-symbol (find-element "au")) "Au"))
  (ok (string= (chemical-element-name (find-element "oxygen")) "oxygen"))
  (ok (string= (chemical-element-name (find-element "OXYGEN")) "oxygen"))
  (ok (string= (chemical-element-name (find-element 118)) "oganesson"))
  (ok (eq (chemical-element-mass-basis (find-element :Og)) :representative))
  (ok (eq (chemical-element-mass-basis (find-element "Tc")) :representative))
  (ok (eq (chemical-element-mass-basis (find-element "Fe")) :standard))
  (ok (null (find-element "Xx" :errorp nil)))
  (ok (null (find-element 0 :errorp nil)))
  (ok (approx= (atomic-weight "uranium") 238.02891d0 1d-5))
  (ok (approx= (atomic-weight :H) 1.00794d0 1d-9))
  (ok (approx= (atomic-weight 8) 15.9994d0 1d-9))
  (ok (approx= (chemical-element-atomic-weight (find-element "Og"))
               294d0
               1d-9))
  (ok (approx= (cdr (assoc "Cl" +atomic-weights+ :test #'string=))
               (atomic-weight "chlorine")
               1d-9))
  (ok (every (lambda (element)
               (approx= (chemical-element-atomic-weight element)
                        (cdr (assoc (chemical-element-symbol element)
                                    +atomic-weights+
                                    :test #'string=))
                        1d-12))
             +periodic-table+))
  (ok (equal (parse-formula "CaCO3")
             '(("C" . 1) ("Ca" . 1) ("O" . 3))))
  (ok (equal (parse-formula "CH3OH")
             '(("C" . 1) ("H" . 4) ("O" . 1))))
  (ok (equal (parse-formula "C6H12O6")
             '(("C" . 6) ("H" . 12) ("O" . 6))))
  (ok (equal (parse-formula "NaCl")
             '(("Cl" . 1) ("Na" . 1))))
  (ok (equal (parse-formula "Fe2O3")
             '(("Fe" . 2) ("O" . 3))))
  (ok (equal (parse-formula "CO2CO")
             '(("C" . 2) ("O" . 3))))
  (ok (equal (parse-formula "He")
             '(("He" . 1))))
  (ok (equal (parse-formula "")
             nil))
  (ok (approx= (formula-molecular-weight "H2O") 18.01528d0 1d-5))
  (ok (approx= (formula-molecular-weight "Ar") 39.948d0 1d-5))
  (ok (approx= (formula-molecular-weight "UO2") 270.02771d0 1d-5))
  (ok (approx= (formula-molecular-weight "Og") 294d0 1d-5))
  (ok (approx= (formula-molecular-weight '(("H" . 2) ("O" . 1)))
               18.01528d0
               1d-5))
  (ok (approx= (formula-molecular-weight "H2"
                                          '(("H" . 2.5d0)))
               5d0
               1d-9))
  (ok (zerop (formula-molecular-weight nil)))
  (ok (handler-case
          (progn (parse-formula "Xx2") nil)
        (error () t)))
  (ok (handler-case
          (progn (parse-formula "h2") nil)
        (error () t)))
  (ok (handler-case
          (progn (parse-formula "C(OH)") nil)
        (error () t)))
  (ok (handler-case
          (progn (formula-molecular-weight '(("Xx" . 1))) nil)
        (error () t)))
  (ok (handler-case
          (progn (find-element "Xx") nil)
        (error () t)))
  (format t "~&rosette-chemical-formula: ~D assertions, 0 failures.~%"
          *assertions*)
  t)
