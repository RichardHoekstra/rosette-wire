;;;; selfeval-f.lisp --- growing the metacircular frontier: the core evaluator,
;;;; AS A CORE PROGRAM, run on encoded core programs of the *F fragment* --
;;;; E extended with `let`, MULTIPLE variables, and first-order BOUNDED-RECURSIVE
;;;; CALLS.  These are exactly the constructs the interpreter's OWN body uses, so
;;;; the object language now CONTAINS the metalanguage's machinery.
;;;;
;;;; This closes two of the three stubs the base self-interpreter named:
;;;; `let` and first-order CALLs *inside the interpreted term* are now covered.
;;;; The third -- the REFLECTIVE FIXPOINT (the interpreter interpreting an
;;;; encoding of ITSELF) -- remains STUB, and this file MAKES THE OBSTRUCTION
;;;; FALSIFIABLE rather than hand-waved: see the two demonstrated walls at the
;;;; bottom (`f-code-fits-as-value-p`, `f-heap-max-index`).
;;;;
;;;; ================= the F fragment (interpreted language) ===================
;;;; A closed program is  <one recursive first-order function g> + <main>, over:
;;;;   <int>              integer literal
;;;;   <sym>              a variable, resolved to an ABSOLUTE de-Bruijn SLOT
;;;;   (OP a b)           OP in + - * / % < > =
;;;;   (if c t e)         conditional
;;;;   (let x e b)        bind x = e in b   (x occupies the next slot)
;;;;   (call a ...)       call the function g (self-recursion => genuine calls)
;;;; Values are :int; totality is preserved by a FUEL budget on `ev` (no `fix`),
;;;; the same discipline as the E interpreter's height-fuel and the object-level
;;;; `sum`.  The single recursive g plus `let`/multivar is enough to interpret
;;;; sum / factorial / power / Euclid-gcd through the self-interpreter.
;;;;
;;;; ================= encoding (arity-3 heap, extended tags) ==================
;;;; Same Goedel scheme as `encode-e`: an arity-3 heap (node i -> 3i+1,3i+2,3i+3)
;;;; flattened to a base-B positional integer, each slot = tag + payload*16.
;;;;   tag 1  = literal   (payload = value + LIT-OFFSET)
;;;;   tag 2  = variable  (payload = absolute slot index)
;;;;   tag 3+op = binop   (op in 0..7)
;;;;   tag 11 = if    tag 12 = let    tag 13 = call g
;;;; ENVIRONMENT is itself an integer, positional in the SAME base:
;;;;   env = sum_j (val_j + OFF) * B^j ;  lookup slot j = (env / B^j mod B) - OFF.
;;;; `let` extends env at slot=depth; a call builds a fresh env of the arg values
;;;; at slots 0..arity-1.  Decoding uses only / % * + - -- the core's own math --
;;;; so the whole interpreter (pw/dg/tg/pl/vl/lk/ap/ev) is a CORE PROGRAM.
;;;;
;;;; ================= the identity, gated ====================================
;;;;   (self-eval-f g main vars inputs fuel) == (direct-eval-f g main vars inputs)
;;;; i.e. `program-eval` running the interpreter-as-a-core-program on the encoded
;;;; program reproduces `term-eval` on the program directly, now for programs
;;;; that USE let, several variables, and recursion.

(in-package #:rosette-core-term)

;;; --- reuse the E encoding constants (B, tag radix, literal bias) ------------

(defun f-opcode (head)
  (let ((nm (binop-name head))) (and nm (position nm *e-ops* :test #'string=))))

(defun %f-slot (node cenv)
  "The heap slot (a small non-negative integer) packing NODE's tag/payload,
given the compile-time environment CENV (alist SYMBOL-NAME -> absolute slot)."
  (cond
    ((integerp node)
     (unless (< (abs node) +e-lit-offset+)
       (error "encode-f: literal ~S out of the interpreted range" node))
     (+ 1 (* (+ node +e-lit-offset+) +e-tag-base+)))          ; tag 1 = literal
    ((symbolp node)
     (let ((c (assoc (symbol-name node) cenv :test #'string=)))
       (unless c (error "encode-f: unbound variable ~S" node))
       (+ 2 (* (cdr c) +e-tag-base+))))                       ; tag 2 = var@slot
    ((and (consp node) (sym= (first node) "IF")) 11)          ; tag 11 = if
    ((and (consp node) (sym= (first node) "LET")) 12)         ; tag 12 = let
    ((and (consp node) (sym= (first node) "CALL")) 13)        ; tag 13 = call g
    ((and (consp node) (f-opcode (first node)))
     (+ 3 (f-opcode (first node))))                           ; tag 3+op = binop
    (t (error "encode-f: node ~S is outside the F fragment" node))))

(defun %encode-f (node cenv nextslot)
  "Encode NODE with compile-env CENV and NEXTSLOT (= the runtime DEPTH at NODE,
i.e. the number of binders in scope).  Returns (values CODE HEIGHT)."
  (let ((tbl (make-hash-table)) (height 0))
    (labels ((walk (nd i d ce ns)
               (setf height (max height d))
               (setf (gethash i tbl) (%f-slot nd ce))
               (cond
                 ((or (integerp nd) (symbolp nd)) nil)
                 ((sym= (first nd) "IF")
                  (walk (second nd) (+ (* 3 i) 1) (1+ d) ce ns)
                  (walk (third nd)  (+ (* 3 i) 2) (1+ d) ce ns)
                  (walk (fourth nd) (+ (* 3 i) 3) (1+ d) ce ns))
                 ((sym= (first nd) "LET")            ; (let x e b): x -> slot ns
                  (walk (third nd) (+ (* 3 i) 1) (1+ d) ce ns)
                  (walk (fourth nd) (+ (* 3 i) 2) (1+ d)
                        (cons (cons (symbol-name (second nd)) ns) ce) (1+ ns)))
                 ((sym= (first nd) "CALL")           ; (call a ...): args as kids
                  (loop for a in (rest nd) for k from 1
                        do (walk a (+ (* 3 i) k) (1+ d) ce ns)))
                 (t                                  ; binary op
                  (walk (second nd) (+ (* 3 i) 1) (1+ d) ce ns)
                  (walk (third nd)  (+ (* 3 i) 2) (1+ d) ce ns)))))
      (walk node 0 1 cenv nextslot)
      (let ((code 0))
        (maphash (lambda (i s) (incf code (* s (expt +e-base+ i)))) tbl)
        (values code height)))))

(defun encode-f (term var-names)
  "Encode an F-term TERM whose free variables are VAR-NAMES (symbols bound to
absolute slots 0..k-1).  Returns (values CODE HEIGHT).  Injective on F-terms."
  (%encode-f term
             (loop for v in var-names for i from 0 collect (cons (symbol-name v) i))
             (length var-names)))

;;; --- the F-interpreter, AS A CORE PROGRAM ----------------------------------

(defun self-interp-funs-f ()
  "The F-interpreter written in the core language.  `ev` is bounded-recursive
(a FUEL budget guards termination -- no `fix`); the environment is an integer
decoded with the core's own arithmetic.  Splices the encoding constants in as
literals.  Signature of ev:
    (ev code i env depth fuel gcode garity gh)
 CODE = body being evaluated, I = heap index, ENV = integer env, DEPTH = next
 free slot, GCODE/GARITY/GH = the single callable function g."
  (let ((b +e-base+) (tb +e-tag-base+) (off +e-lit-offset+))
    `((pw (k) (if (< k 1) 1 (* ,b (pw (- k 1)))))            ; place value B^k
      (dg (code i) (% (/ code (pw i)) ,b))                  ; heap digit at i
      (tg (code i) (% (dg code i) ,tb))                     ; tag of that slot
      (pl (code i) (/ (dg code i) ,tb))                     ; payload of slot
      (vl (code i) (- (pl code i) ,off))                    ; literal value
      (lk (env j) (- (% (/ env (pw j)) ,b) ,off))           ; env lookup slot j
      (ap (o a b)                                           ; apply opcode o
          (if (= o 0) (+ a b) (if (= o 1) (- a b) (if (= o 2) (* a b)
          (if (= o 3) (/ a b) (if (= o 4) (% a b) (if (= o 5) (< a b)
          (if (= o 6) (> a b) (= a b)))))))))
      (ev (code i env depth fuel gcode garity gh)
          (if (< fuel 1) 0
            (let tag (tg code i)
              (if (= tag 1) (vl code i)                       ; literal
              (if (= tag 2) (lk env (pl code i))             ; variable @ slot
              (if (= tag 11)                                 ; if
                  (if (ev code (+ (* 3 i) 1) env depth (- fuel 1) gcode garity gh)
                      (ev code (+ (* 3 i) 2) env depth (- fuel 1) gcode garity gh)
                      (ev code (+ (* 3 i) 3) env depth (- fuel 1) gcode garity gh))
              (if (= tag 12)                                 ; let: bind @ depth
                  (let v (ev code (+ (* 3 i) 1) env depth (- fuel 1) gcode garity gh)
                    (ev code (+ (* 3 i) 2)
                        (+ env (* (+ v ,off) (pw depth)))
                        (+ depth 1) (- fuel 1) gcode garity gh))
              (if (= tag 13)                                 ; call g (arity<=3)
                  (let a0 (ev code (+ (* 3 i) 1) env depth (- fuel 1) gcode garity gh)
                  (let a1 (if (< garity 2) 0 (ev code (+ (* 3 i) 2) env depth (- fuel 1) gcode garity gh))
                  (let a2 (if (< garity 3) 0 (ev code (+ (* 3 i) 3) env depth (- fuel 1) gcode garity gh))
                    (ev gcode 0
                        (+ (+ (+ a0 ,off) (* (+ a1 ,off) ,b))
                           (if (< garity 3) 0 (* (+ a2 ,off) (* ,b ,b))))
                        garity (- fuel 1) gcode garity gh))))
                  (ap (- tag 3)                              ; binary op
                      (ev code (+ (* 3 i) 1) env depth (- fuel 1) gcode garity gh)
                      (ev code (+ (* 3 i) 2) env depth (- fuel 1) gcode garity gh))))))))))
      )))

;;; --- the metacircular identity on F: eval_core(#p) = eval_core(p) ----------

(defun self-eval-f (g main main-vars inputs fuel)
  "Run the F-interpreter (as a core program, via PROGRAM-EVAL) on the encoding
of the F-program (G . MAIN).  G = (name (params...) body) or NIL; MAIN is an
F-term over MAIN-VARS (bound to INPUTS at slots 0..k-1); FUEL bounds recursion.
This is eval_core(#p) -- the interpreter running on the encoded program."
  (multiple-value-bind (mcode) (encode-f main main-vars)
    (multiple-value-bind (gcode gh garity)
        (if g
            (multiple-value-bind (c h) (encode-f (third g) (second g))
              (values c h (length (second g))))
            (values 0 0 0))
      (let ((env0 (loop for x in inputs for i from 0
                        sum (* (+ x +e-lit-offset+) (expt +e-base+ i)))))
        (program-eval
         (make-core-program
          :funs (self-interp-funs-f)
          :main (list 'ev mcode 0 env0 (length main-vars) fuel gcode garity gh)))))))

(defun self-eval-f-raw (maincode main-vars inputs fuel
                        &optional (gcode 0) (garity 0) (gh 0))
  "Run the F-interpreter on a RAW main-code integer MAINCODE (already encoded),
binding INPUTS to slots 0..k-1.  Exposed so a negative gate can feed a TAMPERED
encoding (e.g. MAINCODE+1) and show it diverges from the direct evaluator."
  (let ((env0 (loop for x in inputs for i from 0
                    sum (* (+ x +e-lit-offset+) (expt +e-base+ i)))))
    (program-eval
     (make-core-program
      :funs (self-interp-funs-f)
      :main (list 'ev maincode 0 env0 (length main-vars) fuel gcode garity gh)))))

(defun %f->core (term)
  "Translate an F-term (which uses `call` for the single function g) into a real
core term (call -> a named g call)."
  (cond ((atom term) term)
        ((sym= (first term) "CALL") (cons 'g (mapcar #'%f->core (rest term))))
        (t (cons (first term) (mapcar #'%f->core (rest term))))))

(defun direct-eval-f (g main main-vars inputs)
  "The DIRECT core evaluator on the F-program -- the oracle the F self-
interpreter must reproduce.  This is eval_core(p)."
  (let ((funs (when g (list (list 'g (second g) (%f->core (third g)))))))
    (term-eval (%f->core main)
               (loop for v in main-vars for x in inputs
                     collect (cons (symbol-name v) x))
               (program-signature (make-core-program :funs funs :main 0)))))

(defun self-eval-f-certificate (g main main-vars input-rows fuel)
  "Certificate that the F self-interpreter agrees with the direct evaluator on
the F-program across INPUT-ROWS (a list of input-lists) -- the metacircular
identity eval_core(#p)=eval_core(p) on the F fragment.  PASSED is the verdict;
the witness re-decides independently (RUNTIME-VERIFY re-runs the whole tower)."
  (let* ((rows (mapcar (lambda (in)
                         (list :in in
                               :self (self-eval-f g main main-vars in fuel)
                               :direct (direct-eval-f g main main-vars in)))
                       input-rows))
         (ok (every (lambda (r) (eql (getf r :self) (getf r :direct))) rows))
         (cert (make-certificate :name :core-term-metacircular-self-eval-f
                                 :kind :proof :claim :self-eval-f-agrees
                                 :payload (list :rows rows :agree ok) :passed ok)))
    (setf (certificate-witness cert)
          (make-lean-witness
           :core-term-self-eval-f-re-runs
           (lambda ()
             (every (lambda (in)
                      (eql (self-eval-f g main main-vars in fuel)
                           (direct-eval-f g main main-vars in)))
                    input-rows))))
    cert))

;;; --- the reflective-fixpoint WALL, made falsifiable ------------------------
;;; The full reflective fixpoint (the interpreter on an encoding of ITSELF) is
;;; NOT reached.  Rather than assert that as a bare "stub", these two functions
;;; EXHIBIT the two structural obstructions, so the tests can gate them:
;;;
;;;  (a) VALUE-CAPACITY wall.  An interpreter written IN the object language can
;;;      only receive arguments that are object VALUES, |v| < LIT-OFFSET.  But a
;;;      program's Goedel code is a multi-digit base-B integer far exceeding that
;;;      (>= B > OFF for any term with more than one slot).  So #p cannot be fed
;;;      to an in-language interpreter as an argument value.
;;;  (b) EXPONENTIAL-INDEX wall.  The arity-3 positional heap gives a node at
;;;      depth d the index ~3^d, so #p has ~26*3^depth bits: encoding a DEEP
;;;      program (like the interpreter itself) is not merely large but
;;;      intractable to even form.

(defun f-code-fits-as-value-p (term var-names)
  "T iff the Goedel code of TERM would fit as an object VALUE (|code| < the
literal capacity).  For any non-trivial program this is NIL -- the value-
capacity wall that blocks feeding #p to an in-language interpreter."
  (< (nth-value 0 (encode-f term var-names)) +e-lit-offset+))

(defun f-heap-max-index (term var-names)
  "The maximum arity-3 heap index used to encode TERM -- WITHOUT forming the
(possibly astronomically large) code integer.  Grows ~3^depth, so #(deep
program) has ~26*this bits: the exponential-index wall.  Exposed so the tests
can witness the super-linear blow-up that makes the reflective fixpoint
intractable in this encoding."
  (declare (ignore var-names))
  (labels ((mx (nd i)
               (let ((m i))
                 (when (consp nd)
                   (cond
                     ((sym= (first nd) "IF")
                      (setf m (max m (mx (second nd) (+ (* 3 i) 1))
                                   (mx (third nd) (+ (* 3 i) 2))
                                   (mx (fourth nd) (+ (* 3 i) 3)))))
                     ((sym= (first nd) "LET")
                      (setf m (max m (mx (third nd) (+ (* 3 i) 1))
                                   (mx (fourth nd) (+ (* 3 i) 2)))))
                     ((sym= (first nd) "CALL")
                      (loop for a in (rest nd) for k from 1
                            do (setf m (max m (mx a (+ (* 3 i) k))))))
                     ((f-opcode (first nd))
                      (setf m (max m (mx (second nd) (+ (* 3 i) 1))
                                   (mx (third nd) (+ (* 3 i) 2)))))))
                 m)))
    (mx term 0)))
