;;;; tests.lisp --- rosette-lisp-objects: lowtags, precise GC, copying GC, closures.

(defpackage #:rosette-lisp-objects/tests
  (:use #:cl #:rosette-lisp-objects)
  (:export #:run-all-tests))
(in-package #:rosette-lisp-objects/tests)

(defvar *fails* 0)
(defmacro expect (label form)
  `(unless ,form (incf *fails*) (format t "  FAIL ~a~%" ,label)))

(defun mklist (heap &rest vals)
  (let ((acc +nil+)) (dolist (v (reverse vals) acc) (setf acc (heap-cons heap v acc)))))

(defun run-all-tests ()
  (setf *fails* 0)

  ;; --- immediates: round-trips + tags ---
  (dolist (v '(0 1 -1 42 -42 1000000 -1000000 536870911 -536870912))
    (expect (format nil "fixnum ~d round-trips" v) (= v (fixnum-val (mk-fixnum v)))))
  (expect "fixnum tag" (= +tag-fixnum+ (tag-of (mk-fixnum 7))))
  (expect "char round-trip" (= 65 (char-code-of (mk-char 65))))
  (expect "char tag" (charp (mk-char 65)))
  (expect "nil/true distinct" (not (= +nil+ +true+)))
  (expect "nilp" (nilp +nil+))
  (expect "truep" (truep +true+))
  (expect "singleton tag" (singletonp +nil+))

  ;; --- type dispatch ---
  (expect "objtype fixnum" (eq :fixnum (objtype (mk-fixnum 9))))
  (expect "objtype char" (eq :char (objtype (mk-char 90))))
  (expect "objtype singleton" (eq :singleton (objtype +nil+)))
  (expect "fixnum not pointer" (not (pointerp (mk-fixnum 9))))

  ;; --- cons / car / cdr / mutation ---
  (let* ((h (make-heap)) (lst (mklist h (mk-fixnum 1) (mk-fixnum 2) (mk-fixnum 3))))
    (expect "list head is cons" (consp* lst))
    (expect "cons is pointer" (pointerp lst))
    (expect "car" (= 1 (fixnum-val (obj-car h lst))))
    (expect "cadr" (= 2 (fixnum-val (obj-car h (obj-cdr h lst)))))
    (let ((len 0) (sum 0) (p lst))
      (loop until (nilp p) do (incf len) (incf sum (fixnum-val (obj-car h p))) (setf p (obj-cdr h p)))
      (expect "length 3" (= 3 len))
      (expect "sum 6" (= 6 sum)))
    (obj-set-car h lst (mk-fixnum 99))
    (expect "set-car" (= 99 (fixnum-val (obj-car h lst)))))

  ;; --- precise trace: garbage is exact ---
  (let* ((h (make-heap)) (root (mklist h (mk-fixnum 10) (mk-fixnum 20))))
    (heap-cons h (mk-fixnum 97) +nil+) (heap-cons h (mk-fixnum 98) +nil+) (heap-cons h (mk-fixnum 99) +nil+)
    (expect "trace marks exactly reachable" (= 2 (heap-live-count h root))))

  ;; --- cycles terminate ---
  (let* ((h (make-heap)) (a (heap-cons h (mk-fixnum 1) +nil+)) (b (heap-cons h (mk-fixnum 2) a)))
    (obj-set-cdr h a b)
    (expect "cycle both live" (= 2 (heap-live-count h a))))

  ;; --- copying GC: compaction + survival ---
  (let* ((h (make-heap)) (keep (mklist h (mk-fixnum 1) (mk-fixnum 2))))
    (mklist h (mk-fixnum 7) (mk-fixnum 8) (mk-fixnum 9))      ; garbage
    (expect "before: 10 words" (= 10 (heap-word-count h)))
    (multiple-value-bind (new roots) (heap-gc h (list keep))
      (expect "after gc: 4 words (2 live cells)" (= 4 (heap-word-count new)))
      (let ((k (first roots)))
        (expect "gc preserves car" (= 1 (fixnum-val (obj-car new k))))
        (expect "gc preserves cadr" (= 2 (fixnum-val (obj-car new (obj-cdr new k))))))))

  ;; --- flat closures + tracing through captures ---
  (let* ((h (make-heap)) (clo (heap-closure h 0 (list (mk-fixnum 3) (mk-fixnum 5)))))
    (expect "closure tag" (closurep clo))
    (expect "closure code" (= 0 (closure-code h clo)))
    (expect "closure ncaps" (= 2 (closure-ncaps h clo)))
    (expect "closure cap0" (= 3 (fixnum-val (closure-cap h clo 0))))
    (expect "closure cap1" (= 5 (fixnum-val (closure-cap h clo 1)))))
  (let* ((h (make-heap)) (box (heap-cons h (mk-fixnum 42) +nil+)) (clo (heap-closure h 1 (list box))))
    (heap-cons h (mk-fixnum 7) (mk-fixnum 8))                 ; garbage
    (let ((live (heap-trace h clo)))
      (expect "closure live" (gethash (ash clo -3) live))
      (expect "captured cell kept live via closure" (gethash (ash box -3) live))
      (expect "exactly closure + capture" (= 2 (hash-table-count live)))))

  (if (zerop *fails*)
      (progn (format t "rosette-lisp-objects: ALL TESTS PASS~%") t)
      (error "rosette-lisp-objects: ~a test(s) failed" *fails*)))
