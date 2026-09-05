;;;; selfeval.lisp --- STRETCH coincidence (d): the core evaluator, EXPRESSED
;;;; AS A CORE PROGRAM, run on an ENCODED core term -- metacircular self-eval.
;;;;
;;;; This is the (d) coincidence the base library STUBBED: running the core
;;;; evaluator, itself written in the core language, on an encoding of a core
;;;; term (a la rosette-partial-eval's Futamura-1).  We do the LARGEST HONEST
;;;; fragment rather than fake the whole thing.
;;;;
;;;; FRAGMENT COVERED (the "E" language): closed core EXPRESSIONS over a single
;;;; variable X, built from integer literals, the variable, the eight binary
;;;; operators (+ - * / % < > =), and IF.  This is the ground arithmetic-and-
;;;; conditional heart of the core term.
;;;;
;;;; ENCODING @e@ : a Goedel numbering into ONE integer.  The expression tree is
;;;; laid out in an arity-3 HEAP (node i -> children 3i+1, 3i+2, 3i+3) and the
;;;; heap is read off as the base-B integer  code = sum_i slot_i * B^i , where
;;;; each slot packs a tag (and, for a literal, its value):
;;;;     slot = tag + stored*16 ,  stored = value + LIT-OFFSET (literals only).
;;;; Tags: 1 = literal, 2 = variable, 3+opcode = binary op (opcode in 0..7),
;;;; 11 = if.  Decoding a slot needs only / , % and * -- exactly the core's own
;;;; arithmetic -- so the INTERPRETER for E is itself a CORE PROGRAM.
;;;;
;;;; THE INTERPRETER (SELF-INTERP-FUNS) is a set of core functions -- pw (place
;;;; value B^i), dg (heap digit), tg (tag), vl (literal value), ap (apply an
;;;; opcode), ev (the evaluator) -- using the core's BOUNDED recursion (ev
;;;; threads a FUEL counter = the tree height, decremented each descent, so it
;;;; is total on ground inputs, the same discipline as the existing SUM).  The
;;;; VARIABLE case is decided by the LAMBDA LAYER: a variable reads its value
;;;; through  (app (lam v :int x) 0)  -- a genuine closure capturing X -- so the
;;;; typed-lambda layer is LOAD-BEARING inside the self-interpreter.
;;;;
;;;; THE METACIRCULAR IDENTITY (gated):  eval_core(@e@) = eval_core(e) , i.e.
;;;;     (self-eval e x)  ==  (direct-eval-e e x)   for every E-term e, input x,
;;;; where SELF-EVAL runs the core's own PROGRAM-EVAL on the SELF-INTERP program
;;;; applied to the encoded @e@, and DIRECT-EVAL-E runs the core's TERM-EVAL on
;;;; e directly.  The interpreter reproduces the interpreted.
;;;;
;;;; NOT COVERED (honestly STUB): LET and first-order CALLs *inside* the
;;;; interpreted term, and the reflective fixpoint (the interpreter interpreting
;;;; an encoding of ITSELF).  Named clearly; not claimed.

(in-package #:rosette-core-term)

(defconstant +e-base+       67108864 "Heap digit base B = 2^26 (exceeds any slot).")
(defconstant +e-tag-base+   16       "Tag radix inside a slot.")
(defconstant +e-lit-offset+ 1048576  "Literal bias 2^20; interpreted literals lie in (-2^20, 2^20).")

(defparameter *e-ops* '("+" "-" "*" "/" "%" "<" ">" "=")
  "Opcodes 0..7 -- the *binops* order, so tag 3+opcode names the operator.")

(defun %e-opcode (head)
  (let ((nm (binop-name head)))
    (and nm (position nm *e-ops* :test #'string=))))

;;; --- @e@ : encode an E-term into the single integer CODE --------------------

(defun %e-slot (node)
  "The heap slot (a small non-negative integer) packing NODE's tag/value."
  (cond
    ((integerp node)
     (unless (< (abs node) +e-lit-offset+)
       (error "encode-e: literal ~S out of the interpreted range" node))
     (+ 1 (* (+ node +e-lit-offset+) +e-tag-base+)))       ; tag 1 = literal
    ((symbolp node) 2)                                     ; tag 2 = variable X
    ((and (consp node) (sym= (first node) "IF")) 11)       ; tag 11 = if
    ((and (consp node) (%e-opcode (first node)))
     (+ 3 (%e-opcode (first node))))                       ; tag 3+opcode = op
    (t (error "encode-e: node ~S is outside the E fragment (int|var|op|if)" node))))

(defun encode-e (e)
  "Encode an E-term E into (values CODE HEIGHT): CODE is the arity-3-heap
base-B Goedel integer, HEIGHT the tree height (= the FUEL the interpreter
needs).  Injective on the E fragment."
  (let ((tbl (make-hash-table)) (height 0))
    (labels ((walk (node i d)
               (setf height (max height d))
               (setf (gethash i tbl) (%e-slot node))
               (cond
                 ((or (integerp node) (symbolp node)) nil)
                 ((sym= (first node) "IF")
                  (walk (second node) (+ (* 3 i) 1) (1+ d))
                  (walk (third node)  (+ (* 3 i) 2) (1+ d))
                  (walk (fourth node) (+ (* 3 i) 3) (1+ d)))
                 (t                                        ; binary op
                  (walk (second node) (+ (* 3 i) 1) (1+ d))
                  (walk (third node)  (+ (* 3 i) 2) (1+ d))))))
      (walk e 0 1)
      (let ((code 0))
        (maphash (lambda (i s) (incf code (* s (expt +e-base+ i)))) tbl)
        (values code height)))))

(defun decode-tag (code i)
  "The tag stored at heap index I of CODE -- the host-side inverse of the tag
the core interpreter reads with (tg code i).  For inspection / negative gates."
  (mod (mod (floor code (expt +e-base+ i)) +e-base+) +e-tag-base+))

;;; --- the interpreter for E, AS A CORE PROGRAM -------------------------------

(defun self-interp-funs ()
  "The E-interpreter written in the core language.  ev is bounded-recursive
(FUEL = tree height); the variable case uses the lambda layer (a closure over
X).  Splices the encoding constants in as literals."
  (let ((b +e-base+) (tb +e-tag-base+) (off +e-lit-offset+))
    `((pw (k) (if (< k 1) 1 (* ,b (pw (- k 1)))))          ; place value B^k
      (dg (code i) (% (/ code (pw i)) ,b))                 ; heap digit at i
      (tg (code i) (% (dg code i) ,tb))                    ; tag of that slot
      (vl (code i) (- (/ (dg code i) ,tb) ,off))           ; literal value
      (ap (o a b)                                          ; apply opcode o
          (if (= o 0) (+ a b)
              (if (= o 1) (- a b)
                  (if (= o 2) (* a b)
                      (if (= o 3) (/ a b)
                          (if (= o 4) (% a b)
                              (if (= o 5) (< a b)
                                  (if (= o 6) (> a b)
                                      (= a b)))))))))
      (ev (code i x fuel)
          (if (< fuel 1) 0
              (let tag (tg code i)
                (if (= tag 1) (vl code i)                   ; literal
                    (if (= tag 2) (app (lam v :int x) 0)    ; variable via closure
                        (if (= tag 11)                      ; if
                            (if (ev code (+ (* 3 i) 1) x (- fuel 1))
                                (ev code (+ (* 3 i) 2) x (- fuel 1))
                                (ev code (+ (* 3 i) 3) x (- fuel 1)))
                            (ap (- tag 3)                   ; binary op
                                (ev code (+ (* 3 i) 1) x (- fuel 1))
                                (ev code (+ (* 3 i) 2) x (- fuel 1))))))))))))

;;; --- the metacircular identity: eval_core(@e@) = eval_core(e) ---------------

(defun self-eval (e xval)
  "Run the SELF-INTERP core program on the encoding @e@ at input XVAL, using the
core's OWN evaluator (PROGRAM-EVAL).  This is eval_core(@e@)."
  (multiple-value-bind (code height) (encode-e e)
    (program-eval (make-core-program
                   :funs (self-interp-funs)
                   :main (list 'ev code 0 xval height)))))

(defun direct-eval-e (e xval)
  "The DIRECT core evaluator on the E-term E at variable value XVAL -- the
oracle the self-interpreter must reproduce.  This is eval_core(e)."
  (term-eval e (list (cons "X" xval)) '()))

(defun self-eval-certificate (e inputs)
  "Certificate that the self-interpreter agrees with the direct evaluator on E
across INPUTS -- the metacircular identity eval_core(@e@)=eval_core(e).  PASSED
is the verdict; the witness re-decides independently."
  (let* ((rows (mapcar (lambda (x)
                         (list :x x :self (self-eval e x) :direct (direct-eval-e e x)))
                       inputs))
         (ok (every (lambda (r) (eql (getf r :self) (getf r :direct))) rows))
         (cert (make-certificate :name :core-term-metacircular-self-eval
                                 :kind :proof :claim :self-eval-agrees
                                 :payload (list :rows rows :agree ok) :passed ok)))
    (setf (certificate-witness cert)
          (make-lean-witness
           :core-term-self-eval-re-runs
           (lambda ()
             (every (lambda (x) (eql (self-eval e x) (direct-eval-e e x))) inputs))))
    cert))
