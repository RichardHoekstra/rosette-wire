;;;; tests.lisp --- rosette-lisp-codegen: compile + run recursive integer Lisp.

(defpackage #:rosette-lisp-codegen/tests
  (:use #:cl #:rosette-lisp-codegen)
  (:export #:run-all-tests))
(in-package #:rosette-lisp-codegen/tests)

(defvar *fails* 0)
(defun expect (label got want)
  (if (eql got want)
      (format t "  ok   ~a = ~a~%" label got)
      (progn (incf *fails*) (format t "  FAIL ~a: got ~a want ~a~%" label got want))))

;; reference defs used across tests
(defparameter *defs*
  '((fact (n)   (if (< n 2) 1 (* n (fact (- n 1)))))
    (fib  (n)   (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
    (gcd2 (a b) (if (= b 0) a (gcd2 b (% a b))))
    (ack  (m n) (if (= m 0) (+ n 1)
                    (if (= n 0) (ack (- m 1) 1)
                        (ack (- m 1) (ack m (- n 1))))))
    (evenp2 (n) (if (= n 0) 1 (oddp2 (- n 1))))
    (oddp2  (n) (if (= n 0) 0 (evenp2 (- n 1))))
    (sumto (n)  (if (= n 0) 0 (+ n (sumto (- n 1)))))))

(defun ev (main) (run (compile-program *defs* main)))

(defun run-all-tests ()
  (setf *fails* 0)

  ;; --- arithmetic & precedence (tagged-fixnum ops) ---
  (expect "2+3*4"        (ev '(+ 2 (* 3 4))) 14)
  (expect "(7-2)*(3+1)"  (ev '(* (- 7 2) (+ 3 1))) 20)
  (expect "17 % 5"       (ev '(% 17 5)) 2)
  (expect "20 / 6"       (ev '(/ 20 6)) 3)
  (expect "neg -7/2"     (ev '(/ (- 0 7) 2)) -3)        ; truncate toward zero
  (expect "neg -7%2"     (ev '(% (- 0 7) 2)) -1)        ; rem (srem) semantics

  ;; --- comparisons & if ---
  (expect "if 3<5"       (ev '(if (< 3 5) 10 20)) 10)
  (expect "if 5<3"       (ev '(if (< 5 3) 10 20)) 20)
  (expect "= true"       (ev '(if (= 4 4) 1 0)) 1)
  (expect "> false"      (ev '(if (> 2 9) 1 0)) 0)

  ;; --- recursion (the real test of CALL/RET) ---
  (expect "fact 0"  (ev '(fact 0)) 1)
  (expect "fact 5"  (ev '(fact 5)) 120)
  (expect "fact 10" (ev '(fact 10)) 3628800)
  (expect "fib 10"  (ev '(fib 10)) 55)
  (expect "fib 20"  (ev '(fib 20)) 6765)
  (expect "gcd 48 36" (ev '(gcd2 48 36)) 12)
  (expect "gcd 1071 462" (ev '(gcd2 1071 462)) 21)
  (expect "sumto 100" (ev '(sumto 100)) 5050)

  ;; --- nested / deep recursion: Ackermann ---
  (expect "ack 2 3" (ev '(ack 2 3)) 9)
  (expect "ack 3 3" (ev '(ack 3 3)) 61)

  ;; --- mutual recursion ---
  (expect "even 10" (ev '(evenp2 10)) 1)
  (expect "even 7"  (ev '(evenp2 7)) 0)
  (expect "odd 7"   (ev '(oddp2 7)) 1)

  ;; --- multi-arg + composition ---
  (expect "fib(gcd 48 36)+fact 4" (ev '(+ (fib (gcd2 48 36)) (fact 4))) (+ 144 24))

  (if (zerop *fails*)
      (progn (format t "rosette-lisp-codegen: ALL TESTS PASS~%") t)
      (error "rosette-lisp-codegen: ~a test(s) failed" *fails*)))
