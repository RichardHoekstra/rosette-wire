;;;; tests.lisp --- rosette-sel4-verify: referee a corpus of Microkit .system shapes.

(defpackage #:rosette-sel4-verify/tests
  (:use #:cl #:rosette-sel4-verify)
  (:export #:run-all-tests))
(in-package #:rosette-sel4-verify/tests)

(defvar *fails* 0)
(defmacro expect (label form) `(if ,form (format t "  ok   ~a~%" ,label)
                                   (progn (incf *fails*) (format t "  FAIL ~a~%" ,label))))

;; --- corpus: real Microkit .system shapes (the ones the campaign shipped) ---
(defparameter *chain*    ; client@10 -> srvA@20 -> srvB@30  (rising priority: safe)
  "<system>
     <protection_domain name=\"srvB\" priority=\"30\"/><protection_domain name=\"srvA\" priority=\"20\"/>
     <protection_domain name=\"client\" priority=\"10\"/>
     <channel><end pd=\"client\" id=\"0\" pp=\"true\"/><end pd=\"srvA\" id=\"0\"/></channel>
     <channel><end pd=\"srvA\" id=\"1\" pp=\"true\"/><end pd=\"srvB\" id=\"0\"/></channel>
   </system>")
(defparameter *async*    ; notification only, no pp ends: no synchronous deadlock
  "<system>
     <protection_domain name=\"client\" priority=\"100\"/><protection_domain name=\"server\" priority=\"101\"/>
     <channel><end pd=\"client\" id=\"0\"/><end pd=\"server\" id=\"0\"/></channel>
   </system>")
(defparameter *cyclic*   ; a->b->c->a : a forbidden ppcall cycle
  "<system>
     <protection_domain name=\"a\" priority=\"10\"/><protection_domain name=\"b\" priority=\"20\"/>
     <protection_domain name=\"c\" priority=\"30\"/>
     <channel><end pd=\"a\" id=\"0\" pp=\"true\"/><end pd=\"b\" id=\"0\"/></channel>
     <channel><end pd=\"b\" id=\"1\" pp=\"true\"/><end pd=\"c\" id=\"0\"/></channel>
     <channel><end pd=\"c\" id=\"1\" pp=\"true\"/><end pd=\"a\" id=\"1\"/></channel>
   </system>")

(defun run-all-tests ()
  (setf *fails* 0)

  ;; --- parsing ---
  (let ((s (parse-system-string *chain*)))
    (expect "parse: 3 PDs"        (= 3 (length (system-pds s))))
    (expect "parse: 2 ppcall edges from pp=\"true\"" (= 2 (length (system-edges s))))
    (expect "parse: priorities"   (and (= 10 (pd-priority s :client)) (= 30 (pd-priority s :srvb)))))

  ;; --- deadlock-freedom: the exact (directed potential) invariant ---
  (let ((s (parse-system-string *chain*)))
    (multiple-value-bind (df order b1) (deadlock-free-p s)
      (expect "chain: deadlock-free"        df)
      (expect "chain: potential exists"     (and order t))
      (expect "chain: b_1 shadow = 0"       (= 0 b1))
      (expect "chain: priority-monotone"    (priority-monotone-p s))))
  (let ((s (parse-system-string *async*)))
    (expect "async: deadlock-free (no ppcall)" (deadlock-free-p s))
    (expect "async: no ppcall edges"           (null (system-edges s))))
  (let ((s (parse-system-string *cyclic*)))
    (multiple-value-bind (df order b1) (deadlock-free-p s)
      (expect "cyclic: NOT deadlock-free"   (not df))
      (expect "cyclic: potential obstructed" (null order))
      (expect "cyclic: b_1 = 1"             (= 1 b1))
      (expect "cyclic: priority rule fails" (not (priority-monotone-p s)))))

  ;; --- the honest refinement: a reconvergent DAG is deadlock-free even though
  ;;     undirected b_1 > 0 (the directed potential is exact; b_1 over-counts) ---
  (let ((s (make-system)))
    (add-pd s :p :priority 1) (add-pd s :q :priority 2) (add-pd s :r :priority 3)
    (add-ppcall s :p :q) (add-ppcall s :q :r) (add-ppcall s :p :r)   ; reconvergent
    (multiple-value-bind (df order b1) (deadlock-free-p s)
      (expect "reconvergent DAG: deadlock-free (exact)" (and df order))
      (expect "reconvergent DAG: priority-monotone"     (priority-monotone-p s))
      (expect "reconvergent DAG: b_1 shadow OVER-counts (>0, not a deadlock)" (> b1 0))))

  ;; --- information-flow: same shape (a level must not DROP along a flow) ---
  (let ((s (parse-system-string *chain* :levels '((:client . 0) (:srva . 1) (:srvb . 2)))))
    (expect "info-flow rising classification: secure" (info-flow-secure-p s)))
  (let ((s (make-system)))
    (add-pd s :secret :priority 1 :level 2) (add-pd s :public :priority 2 :level 0)
    (add-ppcall s :secret :public)                   ; level drops 2 -> 0 = leak
    (expect "info-flow secret->public: LEAK"          (not (info-flow-secure-p s)))
    (expect "referee locates the leak as a violation"
            (find :info-leak (verdict-violations (referee s)) :key #'car)))

  ;; --- end-to-end verdict ---
  (let ((v (referee (parse-system-string *chain*))))
    (expect "verdict chain: SAFE" (and (verdict-deadlock-free v) (verdict-info-flow-secure v))))
  (let ((v (referee (parse-system-string *cyclic*))))
    (expect "verdict cyclic: UNSAFE + violations listed"
            (and (not (verdict-deadlock-free v)) (verdict-violations v))))

  (if (zerop *fails*)
      (progn (format t "rosette-sel4-verify: ALL TESTS PASS~%") t)
      (error "rosette-sel4-verify: ~a test(s) failed" *fails*)))
