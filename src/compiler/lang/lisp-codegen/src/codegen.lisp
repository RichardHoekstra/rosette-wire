;;;; codegen.lisp --- a small integer Lisp -> stack-machine bytecode + a VM.
;;;;
;;;; Source language (one expression per function body):
;;;;   <int>                  integer literal
;;;;   <sym>                  reference to a parameter of the enclosing function
;;;;   (if c then else)
;;;;   (OP a b)               OP in + - * / % < > =   (binary)
;;;;   (f a ...)              call a defined function (recursion allowed)
;;;;
;;;; VM values are rosette-lisp-objects TAGGED FIXNUMS (value*8, tag 000).  Under
;;;; that representation ADD/SUB/MOD are raw word ops, MUL=(a*b)/8, DIV=(a/b)*8,
;;;; and signed comparison is monotonic -- so this bytecode runs UNCHANGED on the
;;;; native VM (rosette-lisp-codegen/native), which lowers the same fetch loop to
;;;; machine code through rosette-gpu-kernel-dsl's WHILE.

(in-package #:rosette-lisp-codegen)

;;; mk-fixnum / fixnum-val are imported from rosette-lisp-objects (see package.lisp):
;;; VM values are tagged fixnums of the shared object machine.

;;; --- the ISA ----------------------------------------------------------------
(defconstant +op-halt+  0)
(defconstant +op-pushc+ 1)   ; push constant (a tagged fixnum)
(defconstant +op-loadl+ 2)   ; push local[arg]  (local = operand-stack[fp+arg])
(defconstant +op-add+   3)
(defconstant +op-sub+   4)
(defconstant +op-mul+   5)
(defconstant +op-div+   6)
(defconstant +op-mod+   7)
(defconstant +op-lt+    8)
(defconstant +op-gt+    9)
(defconstant +op-eq+    10)
(defconstant +op-jmp+   11)  ; pc = arg
(defconstant +op-jz+    12)  ; v = pop ; if (fixnum-val v)=0 then pc = arg
(defconstant +op-call+  13)  ; call function #arg (entry/arity from the tables)
(defconstant +op-ret+   14)

(defun sym= (s name) (and (symbolp s) (string= (symbol-name s) name)))

(defun binop-opcode (sym)
  ;; compare by NAME so operator symbols match regardless of the caller's package
  ;; (`%`, `<`, etc. are not CL symbols and would not be EQ across packages)
  (cond ((sym= sym "+") +op-add+) ((sym= sym "-") +op-sub+) ((sym= sym "*") +op-mul+)
        ((sym= sym "/") +op-div+) ((sym= sym "%") +op-mod+)
        ((sym= sym "<") +op-lt+)  ((sym= sym ">") +op-gt+)  ((sym= sym "=") +op-eq+)
        (t nil)))

;;; --- compiler ---------------------------------------------------------------
(defstruct program code entry fn-entry fn-arity names)

(defvar *fn-index*)      ; name -> function index
(defvar *label-ctr*)
(defun genlabel () (incf *label-ctr*))

(defun compile-expr (e env)
  "Compile expression E (ENV = ordered list of in-scope parameter names) to a
list of items.  Each item is (:label id) or (opcode arg).  Leaves exactly one
value on the operand stack."
  (cond
    ((integerp e) (list (list +op-pushc+ (mk-fixnum e))))
    ((symbolp e)
     (let ((slot (position e env)))
       (unless slot (error "rosette-lisp-codegen: unbound variable ~S" e))
       (list (list +op-loadl+ slot))))
    ((consp e)
     (let ((head (first e)))
       (cond
         ((sym= head "IF")
          (let ((l-else (genlabel)) (l-end (genlabel)))
            (append (compile-expr (second e) env)
                    (list (list +op-jz+ l-else))
                    (compile-expr (third e) env)
                    (list (list +op-jmp+ l-end))
                    (list (list :label l-else))
                    (compile-expr (fourth e) env)
                    (list (list :label l-end)))))
         ((binop-opcode head)
          (append (compile-expr (second e) env)
                  (compile-expr (third e) env)
                  (list (list (binop-opcode head) 0))))
         (t                                              ; function call
          (let ((idx (gethash head *fn-index*)))
            (unless idx (error "rosette-lisp-codegen: call to undefined function ~S" head))
            (append (mapcan (lambda (a) (compile-expr a env)) (rest e))
                    (list (list +op-call+ idx))))))))
    (t (error "rosette-lisp-codegen: bad expression ~S" e))))

(defun assemble (items ndefs)
  "Two-pass assembly of ITEMS to an int vector.  Returns
(values CODE FN-ENTRY MAIN-ENTRY); each real instruction occupies two ints."
  (let ((addr 0) (labels (make-hash-table)) (fn-entry (make-array ndefs)) (main 0))
    (dolist (it items)                                   ; pass 1: addresses
      (case (first it)
        (:label      (setf (gethash (second it) labels) addr))
        (:fn-entry   (setf (aref fn-entry (second it)) addr))
        (:main-entry (setf main addr))
        (t (incf addr 2))))
    (let ((code (make-array addr)) (i 0))                ; pass 2: emit
      (dolist (it items)
        (case (first it)
          ((:label :fn-entry :main-entry))
          (t (let ((op (first it)) (arg (second it)))
               (setf (aref code i) op)
               (setf (aref code (1+ i))
                     (if (or (= op +op-jmp+) (= op +op-jz+)) (gethash arg labels) arg))
               (incf i 2)))))
      (values code fn-entry main))))

(defun compile-program (defs main-expr)
  "DEFS = list of (name (param ...) body-expr).  MAIN-EXPR = a closed expression
that may call the DEFS.  Returns a PROGRAM."
  (let ((*fn-index* (make-hash-table)) (*label-ctr* 0)
        (names (mapcar #'first defs)))
    (loop for d in defs for i from 0 do (setf (gethash (first d) *fn-index*) i))
    (let ((arity (make-array (length defs)))
          (items '()))
      (loop for d in defs for i from 0 do
        (setf (aref arity i) (length (second d)))
        (setf items (append items
                            (list (list :fn-entry i))
                            (compile-expr (third d) (second d))
                            (list (list +op-ret+ 0)))))
      (setf items (append items
                          (list (list :main-entry))
                          (compile-expr main-expr '())
                          (list (list +op-halt+ 0))))
      (multiple-value-bind (code fn-entry main) (assemble items (length defs))
        (make-program :code code :entry main :fn-entry fn-entry
                      :fn-arity arity :names names)))))

;;; --- reference VM (the oracle) ----------------------------------------------
;;; Two stacks: an operand stack (sp) and a call stack (csp).  CALL saves
;;; (ret-pc, old-fp); RET collapses the frame to (fp), pushes the return value,
;;; and restores fp/pc.  Locals (the args) are operand-stack[fp+i].
(defun run (program &key (op-size 4096) (call-size 1024))
  "Execute PROGRAM on the reference VM; return the integer result."
  (let ((code (program-code program)) (fe (program-fn-entry program))
        (fa (program-fn-arity program))
        (ops (make-array op-size)) (cs (make-array call-size))
        (pc (program-entry program)) (sp 0) (fp 0) (csp 0))
    (flet ((tv (w) (fixnum-val w)))
      (loop
        (let ((op (aref code pc)) (arg (aref code (1+ pc))))
          (cond
            ((= op +op-halt+)  (return (tv (aref ops (1- sp)))))
            ((= op +op-pushc+) (setf (aref ops sp) arg) (incf sp) (incf pc 2))
            ((= op +op-loadl+) (setf (aref ops sp) (aref ops (+ fp arg))) (incf sp) (incf pc 2))
            ;; Arithmetic is CANONICAL: untag both operands (fixnum-val), compute
            ;; on values, retag (mk-fixnum).  Representation-exact for the signed
            ;; encoding, and mirrored by the native VM (untag=/8, retag=*8 on i32).
            ((= op +op-add+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (+ (tv (aref ops (1- sp))) (tv (aref ops sp))))) (incf pc 2))
            ((= op +op-sub+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (- (tv (aref ops (1- sp))) (tv (aref ops sp))))) (incf pc 2))
            ((= op +op-mul+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (* (tv (aref ops (1- sp))) (tv (aref ops sp))))) (incf pc 2))
            ((= op +op-div+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (truncate (tv (aref ops (1- sp))) (tv (aref ops sp))))) (incf pc 2))
            ((= op +op-mod+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (rem (tv (aref ops (1- sp))) (tv (aref ops sp))))) (incf pc 2))
            ((= op +op-lt+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (if (< (tv (aref ops (1- sp))) (tv (aref ops sp))) 1 0))) (incf pc 2))
            ((= op +op-gt+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (if (> (tv (aref ops (1- sp))) (tv (aref ops sp))) 1 0))) (incf pc 2))
            ((= op +op-eq+) (decf sp) (setf (aref ops (1- sp)) (mk-fixnum (if (= (tv (aref ops (1- sp))) (tv (aref ops sp))) 1 0))) (incf pc 2))
            ((= op +op-jmp+) (setf pc arg))
            ((= op +op-jz+) (decf sp) (if (zerop (tv (aref ops sp))) (setf pc arg) (incf pc 2)))
            ((= op +op-call+)
             (let ((k (aref fa arg)) (entry (aref fe arg)))
               (setf (aref cs csp) (+ pc 2) (aref cs (1+ csp)) fp)
               (incf csp 2) (setf fp (- sp k) pc entry)))
            ((= op +op-ret+)
             (let ((rv (aref ops (1- sp))))
               (decf csp 2)
               (let ((retpc (aref cs csp)) (oldfp (aref cs (1+ csp))))
                 (setf sp fp (aref ops sp) rv) (incf sp)
                 (setf fp oldfp pc retpc))))
            (t (error "rosette-lisp-codegen: bad opcode ~A at pc ~A" op pc))))))))

;;; --- disassembler -----------------------------------------------------------
(defun disassemble-program (program &optional (stream *standard-output*))
  (let ((code (program-code program)) (mn #(HALT PUSHC LOADL ADD SUB MUL DIV MOD LT GT EQ JMP JZ CALL RET)))
    (loop for i from 0 below (length code) by 2
          for op = (aref code i) for arg = (aref code (1+ i)) do
      (format stream "~4D  ~6A ~@[~A~]~%" i (aref mn op)
              (cond ((= op +op-pushc+) (format nil "~A" (fixnum-val arg)))
                    ((member op (list +op-add+ +op-sub+ +op-mul+ +op-div+ +op-mod+
                                      +op-lt+ +op-gt+ +op-eq+ +op-halt+ +op-ret+)) nil)
                    (t arg))))))
