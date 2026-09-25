;;;; eshkol-numeric.lisp --- ProgramIR/front-door numeric dialect -> Eshkol backend.
;;;;
;;;; docs/interop.md previously stated that "rational/float AD" are outside the
;;;; admitted front-door/eshkol gate.  This file extends that gate with a second,
;;;; independently checked dialect that sits beside the exact-integer one in
;;;; eshkol.lisp rather than inside it: EXACT RATIONALS, IEEE DOUBLES under an
;;;; explicit ulp-bounded numeric regime, and forward-mode AD via Eshkol's
;;;; `derivative`/`gradient` builtins on scalar float functions.
;;;;
;;;; It follows eshkol.lisp's own shape: a grammar/admission step, a direct
;;;; evaluator, an independently written kernel-VM evaluator (a distinct oracle,
;;;; not a shared call path -- the same "one term, two written meanings"
;;;; discipline core-term/term.lisp and front-door/terms.lisp already use), a
;;;; deterministic Eshkol emitter, and a JIT/AOT gate over rosette-isolated-worker.
;;;; The exact-integer dialect in eshkol.lisp is untouched: this is an addition,
;;;; not a rewrite.

(in-package #:rosette-front-door/eshkol)

;;; -------------------------------------------------------------------------
;;; Numeric-dialect grammar and type discipline.
;;;
;;;   <int>                 exact integer literal                 :int
;;;   <ratio>                exact rational literal (n/d, d/=1)    :rational
;;;   <double-float>         IEEE double literal                   :float
;;;   <sym>                  variable
;;;   (if c t e)             conditional (c=0 is false, as eshkol.lisp)
;;;   (let x e b)            bind x = e in b
;;;   (+ a b) (- a b) (* a b) (/ a b)     generic arithmetic
;;;   (< a b) (> a b) (= a b)             comparison -> :int 0/1
;;;   (derivative fname x)                forward-mode d(fname)/dx at x -> :float
;;;   (gradient fname (x0 x1 ...) k)      forward-mode d(fname)/dxk       -> :float
;;;
;;; Numeric type promotion is the whole point of the "explicit regime": :int
;;; freely promotes to :rational or to :float (both represent an integer
;;; magnitude used in these tests exactly), but :rational and :float NEVER
;;; implicitly mix -- an exact and an inexact operand together is refused, so
;;; a program cannot silently launder precision loss through arithmetic. That
;;; is the numeric-dialect analogue of eshkol.lisp's own explicit boolean
;;; boxing: nothing is coerced without the program saying so.
;;; -------------------------------------------------------------------------

(define-condition numeric-dialect-refusal (error)
  ((stage :initarg :stage :reader numeric-dialect-refusal-stage)
   (input :initarg :input :reader numeric-dialect-refusal-input)
   (reason :initarg :reason :reader numeric-dialect-refusal-reason))
  (:report (lambda (condition stream)
             (format stream "rosette-front-door/eshkol numeric dialect refused ~A at ~A: ~A"
                     (numeric-dialect-refusal-input condition)
                     (numeric-dialect-refusal-stage condition)
                     (numeric-dialect-refusal-reason condition)))))

(defun %numeric-refuse (stage input control &rest arguments)
  (error 'numeric-dialect-refusal
         :stage stage :input input
         :reason (apply #'format nil control arguments)))

(defun %num-name= (object name)
  (and (symbolp object) (string= (symbol-name object) name)))

(defun %num-proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun numeric-literal-type (value)
  "Classify a CL numeric literal into the dialect's three-way type universe.
Returns NIL for anything that is not an admitted literal (e.g. NaN/infinity,
complex, single-float)."
  (cond
    ((integerp value) :int)
    ((and (rationalp value) (not (integerp value))) :rational)
    ((typep value 'double-float)
     (if (or (sb-ext:float-nan-p value) (sb-ext:float-infinity-p value))
         nil
         :float))
    (t nil)))

(defun %numeric-join (left right term)
  "The dialect's explicit promotion lattice for a binary operator's operand
types. :INT freely promotes into either exact or inexact; :RATIONAL and
:FLOAT never implicitly mix -- this is where a program that tries to launder
precision loss through arithmetic is refused (see docs/interop.md)."
  (cond
    ((and (eq left :int) (eq right :int)) :int)
    ((and (member left '(:int :rational)) (member right '(:int :rational)))
     :rational)
    ((and (member left '(:int :float)) (member right '(:int :float)))
     :float)
    (t (%numeric-refuse :typecheck term
                         "cannot mix exact ~(~A~) and inexact ~(~A~) operands without an explicit conversion"
                         left right))))

(defstruct (numeric-program
            (:constructor %make-numeric-program (&key funs main)))
  "FUNS = list of (name (param ...) body-term), each body over :FLOAT params
only (the domain forward-mode AD is defined on here). MAIN is the closed
result term."
  (funs nil :type list)
  (main nil :type t))

(defun %numeric-lookup-fun (funs name term)
  (or (assoc (symbol-name name) funs :key #'symbol-name :test #'string=)
      (%numeric-refuse :typecheck term "call to undefined function ~A" name)))

(defun infer-numeric-type (term env funs)
  "Infer TERM's dialect type under ENV (alist symbol-name -> type) and FUNS
(NUMERIC-PROGRAM-FUNS). Signals NUMERIC-DIALECT-REFUSAL on any admission
failure -- unbound variable, mixed exact/inexact operands, a DERIVATIVE or
GRADIENT target outside the admitted scalar-float shape, or an out-of-range
GRADIENT component index."
  (cond
    ((numberp term)
     (or (numeric-literal-type term)
         (%numeric-refuse :typecheck term "not an admitted numeric literal (NaN/infinity/complex/single-float are refused)")))
    ((symbolp term)
     (let ((cell (assoc (symbol-name term) env :test #'string=)))
       (unless cell (%numeric-refuse :typecheck term "unbound variable ~A" term))
       (cdr cell)))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((%num-name= head "IF")
          (unless (= (length term) 4) (%numeric-refuse :typecheck term "IF wants 3 args"))
          (unless (eq (infer-numeric-type (second term) env funs) :int)
            (%numeric-refuse :typecheck term "IF condition must be :int"))
          (let ((tt (infer-numeric-type (third term) env funs))
                (te (infer-numeric-type (fourth term) env funs)))
            (unless (eq tt te)
              (%numeric-refuse :typecheck term "IF branches must have the same type, got ~S and ~S" tt te))
            tt))
         ((%num-name= head "LET")
          (unless (= (length term) 4) (%numeric-refuse :typecheck term "LET wants (let x e b)"))
          (unless (symbolp (second term)) (%numeric-refuse :typecheck term "LET variable must be a symbol"))
          (let ((vt (infer-numeric-type (third term) env funs)))
            (infer-numeric-type (fourth term)
                                 (acons (symbol-name (second term)) vt env) funs)))
         ((member (symbol-name head) '("+" "-" "*") :test #'string=)
          (unless (= (length term) 3) (%numeric-refuse :typecheck term "~A wants 2 args" head))
          (%numeric-join (infer-numeric-type (second term) env funs)
                          (infer-numeric-type (third term) env funs)
                          term))
         ((%num-name= head "/")
          (unless (= (length term) 3) (%numeric-refuse :typecheck term "/ wants 2 args"))
          (when (eql (third term) 0)
            (%numeric-refuse :typecheck term "exact division by a literal zero denominator is refused"))
          (let ((jt (%numeric-join (infer-numeric-type (second term) env funs)
                                    (infer-numeric-type (third term) env funs)
                                    term)))
            (if (eq jt :int) :rational jt)))
         ((member (symbol-name head) '("<" ">" "=") :test #'string=)
          (unless (= (length term) 3) (%numeric-refuse :typecheck term "~A wants 2 args" head))
          (%numeric-join (infer-numeric-type (second term) env funs)
                          (infer-numeric-type (third term) env funs)
                          term)
          :int)
         ((%num-name= head "DERIVATIVE")
          (unless (= (length term) 3) (%numeric-refuse :typecheck term "DERIVATIVE wants (derivative fname x)"))
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry)))
            (unless (= (length params) 1)
              (%numeric-refuse :typecheck term "DERIVATIVE target must take exactly one parameter"))
            (unless (eq (infer-numeric-type body
                                             (list (cons (symbol-name (first params)) :float)) funs)
                        :float)
              (%numeric-refuse :typecheck term "DERIVATIVE target body must have type :float")))
          (unless (eq (infer-numeric-type (third term) env funs) :float)
            (%numeric-refuse :typecheck term "DERIVATIVE point must have type :float"))
          :float)
         ((%num-name= head "GRADIENT")
          (unless (= (length term) 4) (%numeric-refuse :typecheck term "GRADIENT wants (gradient fname (x...) k)"))
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry))
                 (points (third term)) (k (fourth term)))
            (unless (>= (length params) 2)
              (%numeric-refuse :typecheck term "GRADIENT target must take at least two parameters"))
            (unless (eq (infer-numeric-type
                         body (mapcar (lambda (p) (cons (symbol-name p) :float)) params) funs)
                        :float)
              (%numeric-refuse :typecheck term "GRADIENT target body must have type :float"))
            (unless (and (%num-proper-list-p points) (= (length points) (length params)))
              (%numeric-refuse :typecheck term "GRADIENT needs one point per parameter"))
            (dolist (p points)
              (unless (eq (infer-numeric-type p env funs) :float)
                (%numeric-refuse :typecheck term "GRADIENT point components must have type :float")))
            (unless (integerp k)
              (%numeric-refuse :typecheck term "GRADIENT component index must be a literal integer"))
            (unless (< -1 k (length params))
              (%numeric-refuse :typecheck term
                                "GRADIENT component index ~D is out of range for a ~D-parameter function"
                                k (length params))))
          :float)
         (t (%numeric-refuse :typecheck term "outside the numeric dialect (unknown head ~A)" head)))))
    (t (%numeric-refuse :typecheck term "not a numeric-dialect term"))))

(defun %parse-numeric-surface (surface)
  "Validate SURFACE (zero or more (DEFUN-NUMERIC name (params) body), exactly
one (MAIN-NUMERIC term)) and return a type-checked NUMERIC-PROGRAM."
  (unless (%num-proper-list-p surface)
    (%numeric-refuse :parse surface "the surface must be a proper list of forms"))
  (let ((funs '()) (main nil) (main-seen-p nil))
    (dolist (form surface)
      (unless (and (%num-proper-list-p form) (consp form) (symbolp (first form)))
        (%numeric-refuse :parse form "each program form must be a headed proper list"))
      (cond
        ((%num-name= (first form) "DEFUN-NUMERIC")
         (unless (= (length form) 4) (%numeric-refuse :parse form "DEFUN-NUMERIC wants NAME, params, BODY"))
         (destructuring-bind (head name params body) form
           (declare (ignore head))
           (unless (symbolp name) (%numeric-refuse :parse form "a function name must be a symbol"))
           (unless (and (%num-proper-list-p params) params (every #'symbolp params))
             (%numeric-refuse :parse form "DEFUN-NUMERIC parameters must be a non-empty proper symbol list"))
           (push (list name (copy-list params) (copy-tree body)) funs)))
        ((%num-name= (first form) "MAIN-NUMERIC")
         (unless (= (length form) 2) (%numeric-refuse :parse form "MAIN-NUMERIC wants exactly one term"))
         (when main-seen-p (%numeric-refuse :parse form "the program must have exactly one MAIN-NUMERIC"))
         (setf main-seen-p t main (copy-tree (second form))))
        (t (%numeric-refuse :parse form "unknown numeric surface head ~A" (first form)))))
    (unless main-seen-p (%numeric-refuse :parse surface "the program has no MAIN-NUMERIC"))
    (setf funs (nreverse funs))
    ;; Every function body is checked over its own :float parameter env now,
    ;; so a malformed helper is refused even if MAIN never calls DERIVATIVE
    ;; or GRADIENT on it.
    (dolist (entry funs)
      (destructuring-bind (name params body) entry
        (declare (ignore name))
        (unless (eq (infer-numeric-type
                     body (mapcar (lambda (p) (cons (symbol-name p) :float)) params) funs)
                    :float)
          (%numeric-refuse :typecheck body "DEFUN-NUMERIC body must have type :float"))))
    (let ((main-type (infer-numeric-type main '() funs)))
      (unless (member main-type '(:int :rational :float))
        (%numeric-refuse :typecheck main "MAIN-NUMERIC must return a numeric-dialect type")))
    (%make-numeric-program :funs funs :main main)))

;;; -------------------------------------------------------------------------
;;; Coincidence 1: the direct evaluator, dual-number aware.
;;;
;;; DERIVATIVE/GRADIENT run forward-mode AD by lifting the evaluator to dual
;;; numbers (value . epsilon): the standard technique, exact for this term
;;; language's +,-,*,/ (see e.g. the sibling rosette-hyper-dual package's
;;; jet-carrier arithmetic for the same idea at higher order).
;;; -------------------------------------------------------------------------

(defstruct (dual (:constructor make-dual (value epsilon)))
  (value 0d0 :type double-float)
  (epsilon 0d0 :type double-float))

(defun %lift-dual (x) (if (dual-p x) x (make-dual (coerce x 'double-float) 0d0)))

(defun dual+ (a b) (let ((a (%lift-dual a)) (b (%lift-dual b)))
                      (make-dual (+ (dual-value a) (dual-value b))
                                 (+ (dual-epsilon a) (dual-epsilon b)))))
(defun dual- (a b) (let ((a (%lift-dual a)) (b (%lift-dual b)))
                      (make-dual (- (dual-value a) (dual-value b))
                                 (- (dual-epsilon a) (dual-epsilon b)))))
(defun dual* (a b) (let ((a (%lift-dual a)) (b (%lift-dual b)))
                      (make-dual (* (dual-value a) (dual-value b))
                                 (+ (* (dual-epsilon a) (dual-value b))
                                    (* (dual-value a) (dual-epsilon b))))))
(defun dual/ (a b) (let ((a (%lift-dual a)) (b (%lift-dual b)))
                      (make-dual (/ (dual-value a) (dual-value b))
                                 (/ (- (* (dual-epsilon a) (dual-value b))
                                       (* (dual-value a) (dual-epsilon b)))
                                    (* (dual-value b) (dual-value b))))))

(defun %numeric-check-denominator (value term stage)
  "Refuse a zero denominator that a term COMPUTED (the literal case is refused
at type-check). Eshkol raises on an exact zero and returns a non-finite double
for an inexact one; neither is a value this dialect's readout admits, so both
evaluators refuse it the same way instead of letting CL signal
DIVISION-BY-ZERO."
  (when (zerop value)
    (%numeric-refuse stage term "division by a computed zero denominator is refused")))

(defun eval-numeric-term (term env funs)
  "The direct evaluator. ENV binds symbol-name -> a CL number OR (during
forward-mode AD) a DUAL. FUNS is NUMERIC-PROGRAM-FUNS."
  (cond
    ((numberp term) term)
    ((symbolp term) (cdr (assoc (symbol-name term) env :test #'string=)))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((%num-name= head "IF")
          (if (zerop (eval-numeric-term (second term) env funs))
              (eval-numeric-term (fourth term) env funs)
              (eval-numeric-term (third term) env funs)))
         ((%num-name= head "LET")
          (let ((v (eval-numeric-term (third term) env funs)))
            (eval-numeric-term (fourth term)
                                (cons (cons (symbol-name (second term)) v) env) funs)))
         ((member (symbol-name head) '("+" "-" "*" "/") :test #'string=)
          (let ((a (eval-numeric-term (second term) env funs))
                (b (eval-numeric-term (third term) env funs)))
            (when (%num-name= head "/")
              (%numeric-check-denominator (if (dual-p b) (dual-value b) b) term :eval))
            (if (or (dual-p a) (dual-p b))
                (cond ((%num-name= head "+") (dual+ a b))
                      ((%num-name= head "-") (dual- a b))
                      ((%num-name= head "*") (dual* a b))
                      (t (dual/ a b)))
                (cond ((%num-name= head "+") (+ a b))
                      ((%num-name= head "-") (- a b))
                      ((%num-name= head "*") (* a b))
                      (t (/ a b))))))
         ((member (symbol-name head) '("<" ">" "=") :test #'string=)
          (let ((a (eval-numeric-term (second term) env funs))
                (b (eval-numeric-term (third term) env funs)))
            (when (dual-p a) (setf a (dual-value a)))
            (when (dual-p b) (setf b (dual-value b)))
            (if (funcall (cond ((%num-name= head "<") #'<)
                                ((%num-name= head ">") #'>)
                                (t #'=))
                         a b)
                1 0)))
         ((%num-name= head "DERIVATIVE")
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry))
                 (x0 (coerce (eval-numeric-term (third term) env funs) 'double-float))
                 (seeded (list (cons (symbol-name (first params)) (make-dual x0 1d0)))))
            (dual-epsilon (eval-numeric-term body seeded funs))))
         ((%num-name= head "GRADIENT")
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry))
                 (points (mapcar (lambda (p) (coerce (eval-numeric-term p env funs) 'double-float))
                                  (third term)))
                 (k (fourth term))
                 (seeded (loop for p in params for x in points for i from 0
                               collect (cons (symbol-name p)
                                             (make-dual x (if (= i k) 1d0 0d0))))))
            (dual-epsilon (eval-numeric-term body seeded funs))))
         (t (%numeric-refuse :eval term "outside the numeric dialect")))))
    (t (%numeric-refuse :eval term "not a numeric-dialect term"))))

(defun numeric-program-eval (program)
  "Type-check and directly evaluate PROGRAM's MAIN-NUMERIC term."
  ;; Re-check the whole program, helpers included: MAIN may call DERIVATIVE or
  ;; GRADIENT on a DEFUN-NUMERIC, which a MAIN-only surface cannot resolve.
  (%parse-numeric-surface
   (append (mapcar (lambda (entry) (cons 'defun-numeric entry))
                   (numeric-program-funs program))
           (list (list 'main-numeric (numeric-program-main program)))))
  (eval-numeric-term (numeric-program-main program) '() (numeric-program-funs program)))

;;; -------------------------------------------------------------------------
;;; Coincidence 2: the kernel-VM oracle, written independently.
;;;
;;; This is NOT eval-numeric-term wearing a different name: it walks kernel
;;; values -- plain CL integers, ratios and doubles, which carry their own type,
;;; and (:DUAL v e) lists during AD -- and dispatches on %KERNEL-TAG, mirroring the explicit tag discipline rosette-core-term's
;;; self-interpreters (selfeval.lisp, selfeval-f.lisp) use for their own
;;; cross-checked oracle. Two differently written walks landing on the same
;;; answer is the actual coincidence being gated, exactly as eshkol.lisp gates
;;; direct-eval against Eshkol's own JIT/AOT.
;;; -------------------------------------------------------------------------

(defun %kernel-tag (value)
  (cond
    ((and (consp value) (eq (first value) :dual)) :dual)
    ((integerp value) :int)
    ((rationalp value) :rat)
    ((typep value 'double-float) :flt)
    (t (%numeric-refuse :kernel-eval value "kernel VM: untagged value"))))

(defun %kernel-num (value)
  "Collapse a kernel tagged value back to a plain CL number (:DUAL -> its
value component, for use inside comparisons/conditionals)."
  (if (and (consp value) (eq (first value) :dual)) (second value) value))

(defun %kernel-const (v) v)                       ; ints/ratios/doubles self-tag
(defun %kernel-dual (v e) (list :dual v e))

(defun %kernel-binop (op a b)
  (let ((ta (%kernel-tag a)) (tb (%kernel-tag b)))
    (if (or (eq ta :dual) (eq tb :dual))
        (let ((av (if (eq ta :dual) (second a) a)) (ae (if (eq ta :dual) (third a) 0d0))
              (bv (if (eq tb :dual) (second b) b)) (be (if (eq tb :dual) (third b) 0d0)))
          (%kernel-dual
           (funcall op av bv)
           (cond ((eq op #'+) (+ ae be))
                 ((eq op #'-) (- ae be))
                 ((eq op #'*) (+ (* ae bv) (* av be)))
                 (t (/ (- (* ae bv) (* av be)) (* bv bv))))))
        (funcall op a b))))

(defun kernel-eval-numeric-term (term env funs)
  "The independent kernel-VM walk. ENV binds symbol-name -> a kernel tagged
value (a plain CL number, or a (:DUAL value epsilon) list during AD)."
  (cond
    ((numberp term) (%kernel-const term))
    ((symbolp term) (cdr (assoc (symbol-name term) env :test #'string=)))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((%num-name= head "IF")
          (if (zerop (%kernel-num (kernel-eval-numeric-term (second term) env funs)))
              (kernel-eval-numeric-term (fourth term) env funs)
              (kernel-eval-numeric-term (third term) env funs)))
         ((%num-name= head "LET")
          (let ((v (kernel-eval-numeric-term (third term) env funs)))
            (kernel-eval-numeric-term (fourth term)
                                       (cons (cons (symbol-name (second term)) v) env) funs)))
         ((member (symbol-name head) '("+" "-" "*" "/") :test #'string=)
          (let ((a (kernel-eval-numeric-term (second term) env funs))
                (b (kernel-eval-numeric-term (third term) env funs)))
            (when (%num-name= head "/")
              (%numeric-check-denominator (%kernel-num b) term :kernel-eval))
            (%kernel-binop (cond ((%num-name= head "+") #'+) ((%num-name= head "-") #'-)
                                  ((%num-name= head "*") #'*) (t #'/))
                           a b)))
         ((member (symbol-name head) '("<" ">" "=") :test #'string=)
          (let ((a (%kernel-num (kernel-eval-numeric-term (second term) env funs)))
                (b (%kernel-num (kernel-eval-numeric-term (third term) env funs))))
            (if (funcall (cond ((%num-name= head "<") #'<) ((%num-name= head ">") #'>) (t #'=)) a b)
                1 0)))
         ((%num-name= head "DERIVATIVE")
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry))
                 (x0 (coerce (%kernel-num (kernel-eval-numeric-term (third term) env funs)) 'double-float))
                 (seeded (list (cons (symbol-name (first params)) (%kernel-dual x0 1d0)))))
            (third (kernel-eval-numeric-term body seeded funs))))
         ((%num-name= head "GRADIENT")
          (let* ((entry (%numeric-lookup-fun funs (second term) term))
                 (params (second entry)) (body (third entry))
                 (points (mapcar (lambda (p) (coerce (%kernel-num (kernel-eval-numeric-term p env funs)) 'double-float))
                                  (third term)))
                 (k (fourth term))
                 (seeded (loop for p in params for x in points for i from 0
                               collect (cons (symbol-name p) (%kernel-dual x (if (= i k) 1d0 0d0))))))
            (third (kernel-eval-numeric-term body seeded funs))))
         (t (%numeric-refuse :kernel-eval term "outside the numeric dialect")))))
    (t (%numeric-refuse :kernel-eval term "not a numeric-dialect term"))))

(defun numeric-kernel-run (program)
  "Run PROGRAM's MAIN-NUMERIC term through the independent kernel-VM walk."
  (%kernel-num (kernel-eval-numeric-term (numeric-program-main program) '() (numeric-program-funs program))))

(defun numeric-gate (surface)
  "Compare the direct evaluator with the kernel-VM oracle on SURFACE.
Returns three values: PASSED-P, DIRECT-VALUE, KERNEL-VALUE."
  (let* ((program (%parse-numeric-surface surface))
         (direct (eval-numeric-term (numeric-program-main program) '() (numeric-program-funs program)))
         (kernel (numeric-kernel-run program)))
    (values (eql direct kernel) direct kernel)))

;;; -------------------------------------------------------------------------
;;; A stated ulp regime for the :FLOAT readouts, in the style of
;;; rosette-scalar-core's +DEFAULT-TOLERANCE+ / rosette-hyper-dual's jet
;;; tolerance transport, made explicit here as units in the last place because
;;; that is the natural unit for comparing two IEEE-754 double computations
;;; rather than a magnitude-relative epsilon.
;;; -------------------------------------------------------------------------

(defun double-ulp (x)
  "One unit in the last place of the IEEE double X."
  (let ((x (abs (coerce x 'double-float))))
    (if (zerop x)
        least-positive-double-float
        (multiple-value-bind (significand exponent sign) (decode-float x)
          (declare (ignore significand sign))
          (scale-float 1d0 (- exponent 53))))))

(defconstant +default-ulp-bound+ 2
  "Two ulps of slack: enough to cross a differently ordered but IEEE-correctly
rounded evaluation of the SAME expression, tight enough to catch a genuinely
different numeric regime (a different rounding mode, an extra intermediate
truncation, or a wrong operator).")

(defun within-ulp-p (observed reference &optional (max-ulps +default-ulp-bound+))
  "T iff OBSERVED and REFERENCE (both doubles) differ by at most MAX-ULPS units
in the last place of REFERENCE. This is the numeric regime DERIVATIVE-AT-ULP-
BOUND and the cross-checks against real Eshkol below are stated against."
  (let ((observed (coerce observed 'double-float))
        (reference (coerce reference 'double-float)))
    (<= (abs (- observed reference))
        (* max-ulps (double-ulp reference)))))

;;; -------------------------------------------------------------------------
;;; Emission: deterministic Eshkol source for an admitted numeric-dialect
;;; program. Reuses the receipt-marker/schema shape from eshkol.lisp so the
;;; JIT/AOT gate below is a drop-in sibling of GATE-ESHKOL.
;;; -------------------------------------------------------------------------

(defparameter +numeric-receipt-marker+ "ROSETTE-FRONT-DOOR-ESHKOL-NUMERIC-RESULT ")
(defconstant +numeric-receipt-schema+ :rosette-front-door-eshkol-numeric-result/v1)

(defun %numeric-mangle (symbol namespace)
  (%mangle-identifier symbol namespace))

(defun %format-double (value)
  "Render a CL double-float as a plain decimal Eshkol reads back as the SAME
double: no exponent/type marker, so Eshkol's reader parses it as a double,
not a rational or a single-float. ~F does not truncate a double's significant digits."
  (format nil "~F" value))

(defun %emit-numeric-literal (value stream)
  (let ((type (numeric-literal-type value)))
    (ecase type
      (:int (format stream "~D" value))
      (:rational (format stream "~D/~D" (numerator value) (denominator value)))
      (:float (write-string (%format-double value) stream)))))

(defun %emit-numeric-term (term stream)
  (labels ((emit (node)
             (cond
               ((numberp node) (%emit-numeric-literal node stream))
               ((symbolp node) (write-string (%numeric-mangle node :value) stream))
               ((consp node)
                (let ((head (first node)))
                  (cond
                    ((%num-name= head "IF")
                     (write-string "(if (= " stream) (emit (second node))
                     (write-string " 0) " stream) (emit (fourth node))
                     (write-char #\Space stream) (emit (third node)) (write-char #\) stream))
                    ((%num-name= head "LET")
                     (format stream "(let ((~A " (%numeric-mangle (second node) :value))
                     (emit (third node)) (write-string ")) " stream) (emit (fourth node))
                     (write-char #\) stream))
                    ((member (symbol-name head) '("+" "-" "*" "/") :test #'string=)
                     (format stream "(~A " (symbol-name head)) (emit (second node))
                     (write-char #\Space stream) (emit (third node)) (write-char #\) stream))
                    ((member (symbol-name head) '("<" ">" "=") :test #'string=)
                     (format stream "(if (~A " (symbol-name head)) (emit (second node))
                     (write-char #\Space stream) (emit (third node))
                     (write-string ") 1 0)" stream))
                    ((%num-name= head "DERIVATIVE")
                     (format stream "(derivative ~A " (%numeric-mangle (second node) :function))
                     (emit (third node)) (write-char #\) stream))
                    ((%num-name= head "GRADIENT")
                     (format stream "(vector-ref (gradient ~A (list "
                             (%numeric-mangle (second node) :function))
                     (loop for p in (third node) for first = t then nil
                           do (unless first (write-char #\Space stream)) (emit p))
                     (format stream ")) ~D)" (fourth node)))
                    (t (%numeric-refuse :emit node "outside the numeric dialect")))))
               (t (%numeric-refuse :emit node "unsupported numeric term")))))
    (emit term)))

(defun %emit-numeric-module-body (program)
  (with-output-to-string (stream)
    (write-line ";; Generated from the validated rosette-front-door numeric dialect (rational/float/AD)." stream)
    (dolist (definition (numeric-program-funs program))
      (destructuring-bind (name parameters body) definition
        (format stream "(define (~A" (%numeric-mangle name :function))
        (dolist (parameter parameters) (format stream " ~A" (%numeric-mangle parameter :value)))
        (write-string ") " stream)
        (%emit-numeric-term body stream)
        (write-line ")" stream)))
    (write-string "(define rosette_numeric_result " stream)
    (%emit-numeric-term (numeric-program-main program) stream)
    (write-line ")" stream)))

(defun %numeric-main-type (program)
  "The admitted dialect type of PROGRAM's MAIN-NUMERIC term."
  (infer-numeric-type (numeric-program-main program) '() (numeric-program-funs program)))

;;; A :FLOAT readout must reach the gate as the exact double Eshkol computed,
;;; not as its decimal DISPLAY: through 1.3.3 Eshkol prints (+ 0.1 0.2) as
;;; 0.3, which the ulp regime would then accept one ulp away. Nor can it go through Eshkol's
;;; INEXACT->EXACT on the value itself: through 1.3.3 it converts over a fixed
;;; 2^52 denominator (0.1 comes back as 450359962737049/2^52); 1.3.5 converts
;;; exactly, and the readout below is correct on both. The helper below instead scales |x| by
;;; powers of two -- exact in IEEE arithmetic, subnormals included -- into an
;;; integer mantissa m in [2^52, 2^53) and prints (:dyadic m e), x = m*2^e.
;;; INEXACT->EXACT is then applied only to that integer-valued double.
;;; Non-finite values print :UNREPRESENTABLE and are refused at acceptance.

(defparameter +numeric-dyadic-helper+
  "(define (rosette_dyadic_down a e) (if (>= a 9007199254740992.0) (rosette_dyadic_down (/ a 2.0) (+ e 1)) (begin (display \"(:dyadic \") (display (inexact->exact a)) (display \" \") (display e) (display \")\"))))
(define (rosette_dyadic_up a e) (if (< a 4503599627370496.0) (rosette_dyadic_up (* a 2.0) (- e 1)) (rosette_dyadic_down a e)))
(define (rosette_display_double x) (if (= x x) (if (= x 0.0) (display \"(:dyadic 0 0)\") (if (= x (* x 2.0)) (display \":unrepresentable\") (if (< x 0.0) (begin (display \"(:negative \") (rosette_dyadic_up (- 0.0 x) 0) (display \")\")) (rosette_dyadic_up x 0)))) (display \":unrepresentable\")))
")

(defun emit-eshkol-numeric-source (surface)
  "Admit SURFACE through the numeric dialect and emit deterministic Eshkol
source. Returns (values SOURCE PROGRAM-ID PROGRAM), matching the shape of
eshkol.lisp's EMIT-ESHKOL-SOURCE."
  (let* ((program (%parse-numeric-surface surface))
         (main-type (%numeric-main-type program))
         (body (concatenate 'string
                            (if (eq main-type :float) +numeric-dyadic-helper+ "")
                            (%emit-numeric-module-body program)))
         (program-id (cid:content-id-long (list :rosette-front-door-eshkol-numeric-source/v1 body))))
    (values
     (with-output-to-string (stream)
       (write-string body stream)
       (format stream "(display ~S)~%" +numeric-receipt-marker+)
       (format stream "(display ~S)~%"
               (format nil "(:schema ~S :ok t :program-id ~S :value-type ~S :value "
                       +numeric-receipt-schema+ program-id main-type))
       (write-line (if (eq main-type :float)
                       "(rosette_display_double rosette_numeric_result)"
                       "(display rosette_numeric_result)")
                   stream)
       (write-line "(display \")\")" stream)
       (write-line "(newline)" stream))
     program-id
     program)))

(defun %decode-numeric-readout (value type)
  "Decode an accepted receipt VALUE of admitted TYPE into a CL number, or NIL
when it is not a well-formed readout of that type. A :FLOAT readout must be
the exact (:DYADIC m e) encoding with m in [2^52, 2^53) (or 0 0), optionally
wrapped in (:NEGATIVE ...); a decimal float literal is refused."
  (flet ((dyadic (form)
           (and (consp form) (eq (first form) :dyadic)
                (%num-proper-list-p form) (= (length form) 3)
                (integerp (second form)) (integerp (third form))
                (let ((m (second form)) (e (third form)))
                  (when (or (and (zerop m) (zerop e))
                            (and (<= (expt 2 52) m) (< m (expt 2 53))
                                 (<= -1126 e 971)))
                    (coerce (* m (expt 2 e)) 'double-float))))))
    (ecase type
      (:int (and (integerp value) value))
      (:rational (and (rationalp value) value))
      (:float (if (and (consp value) (eq (first value) :negative)
                       (%num-proper-list-p value) (= (length value) 2))
                  (let ((magnitude (dyadic (second value))))
                    (and magnitude (- magnitude)))
                  (dyadic value))))))

;;; -------------------------------------------------------------------------
;;; JIT/AOT gate: the same worker/receipt machinery GATE-ESHKOL uses, so the
;;; numeric dialect gets the same direct/kernel/JIT/AOT four-way agreement --
;;; only the receipt acceptance predicate and the value comparison differ,
;;; because a :FLOAT readout is compared under the stated ulp regime instead
;;; of EQL.
;;; -------------------------------------------------------------------------

(defstruct (eshkol-numeric-gate-report
            (:constructor %make-eshkol-numeric-gate-report
                (&key program-id toolchain-command direct-value kernel-value
                      jit-value aot-value jit-envelope aot-compile-envelope
                      aot-envelope passed)))
  program-id toolchain-command direct-value kernel-value jit-value aot-value
  jit-envelope aot-compile-envelope aot-envelope
  (passed nil :type boolean))

(defun %numeric-accept-result (form program-id value-type)
  "Accept FORM only if it is this program's receipt and its :VALUE decodes as
a readout of the admitted VALUE-TYPE. Returns FORM with :VALUE replaced by the
decoded CL number."
  (and (%exact-plist-keys-p form '(:schema :ok :program-id :value-type :value))
       (eq +numeric-receipt-schema+ (getf form :schema))
       (eq t (getf form :ok))
       (stringp (getf form :program-id))
       (string= program-id (getf form :program-id))
       (eq value-type (getf form :value-type))
       (let ((decoded (%decode-numeric-readout (getf form :value) value-type)))
         (and decoded
              (list :schema (getf form :schema) :ok t
                    :program-id (getf form :program-id)
                    :value-type value-type :value decoded)))))

(defun %numeric-values-agree-p (direct kernel jit aot)
  "Rational/int readouts must be bit-for-bit EQL; a :FLOAT readout is compared
under +DEFAULT-ULP-BOUND+ against the direct evaluator (the reference)."
  (flet ((agree (a b) (if (typep direct 'double-float) (within-ulp-p a b) (eql a b))))
    (and (agree kernel direct) (agree jit direct) (agree aot direct))))

(defun %run-numeric-jit (source-path program-id value-type command policy)
  (worker:run-isolated-command
   (append (list "env" "ESHKOL_JIT_CACHE=0" "ESHKOL_JIT_COMPILE_THREADS=1"
                 (format nil "ESHKOL_TIMEOUT_MS=~D" (%timeout-ms policy)))
           command
           (list "--no-stdlib" "--strict-types" "--optimize" "0"
                 "--run" (uiop:native-namestring source-path)))
   :policy policy :marker +numeric-receipt-marker+
   :accept (lambda (form) (%numeric-accept-result form program-id value-type))
   :metadata (list :backend :eshkol-numeric :mode :jit :program-id program-id)))

(defun %run-numeric-aot (output-path program-id value-type policy aot-run-prefix)
  (worker:run-isolated-command
   (append aot-run-prefix (list (uiop:native-namestring output-path)))
   :policy policy :marker +numeric-receipt-marker+
   :accept (lambda (form) (%numeric-accept-result form program-id value-type))
   :metadata (list :backend :eshkol-numeric :mode :aot-run :program-id program-id)))

(defun %gate-eshkol-numeric (surface &key eshkol-command aot-run-prefix (policy (%default-policy)))
  (multiple-value-bind (source program-id program) (emit-eshkol-numeric-source surface)
    (let ((command (%command eshkol-command))
          (value-type (%numeric-main-type program))
          (aot-prefix (%argv-prefix aot-run-prefix "AOT-RUN-PREFIX")))
      (multiple-value-bind (available availability-envelope)
          (eshkol-available-p :eshkol-command command :policy policy)
        (unless available
          (error 'eshkol-unavailable :stage :availability
                 :status (envelope:tool-envelope-status availability-envelope)
                 :envelope availability-envelope)))
      (multiple-value-bind (direct-ok direct kernel) (numeric-gate surface)
        (declare (ignore direct-ok))
        (let* ((workspace (%make-temporary-workspace))
               (nonce (%workspace-nonce))
               (source-path (merge-pathnames (format nil "source-~A.esk" nonce) workspace))
               (output-path (merge-pathnames (format nil "artifact-~A" nonce) workspace))
               (completed-p nil))
          (unwind-protect
               (progn
                 (with-open-file (source-stream source-path :direction :output
                                   :if-exists :error :if-does-not-exist :create
                                   :external-format :utf-8)
                   (write-string source source-stream))
                 (%make-source-container-readable source-path)
                 (let* ((jit (%require-success :jit (%run-numeric-jit source-path program-id value-type command policy)))
                        (aot-compile (%require-success :aot-compile
                                       (%compile-aot source-path output-path program-id command policy))))
                   (unless (probe-file output-path)
                     (error 'eshkol-backend-error :stage :aot-compile :status :missing-artifact :envelope aot-compile))
                   (let* ((aot (%require-success :aot-run (%run-numeric-aot output-path program-id value-type policy aot-prefix)))
                          (jit-value (%receipt-value jit)) (aot-value (%receipt-value aot))
                          (passed (%numeric-values-agree-p direct kernel jit-value aot-value)))
                     (setf completed-p t)
                     (%make-eshkol-numeric-gate-report
                      :program-id program-id :toolchain-command command
                      :direct-value direct :kernel-value kernel
                      :jit-value jit-value :aot-value aot-value
                      :jit-envelope jit :aot-compile-envelope aot-compile :aot-envelope aot
                      :passed passed))))
            (unless (%cleanup-workspace workspace source-path output-path)
              (if completed-p
                  (error 'eshkol-backend-error :stage :cleanup :status :residue
                         :detail (uiop:native-namestring workspace))
                  (warn "Eshkol numeric workspace cleanup left residue at ~A"
                        (uiop:native-namestring workspace))))))))))

(defun gate-eshkol-numeric (surface &key eshkol-command aot-run-prefix (policy (%default-policy)))
  "Compile an admitted rational/float/AD SURFACE through Eshkol JIT and AOT.
Returns an ESHKOL-NUMERIC-GATE-REPORT: PASSED is true when the direct
evaluator, kernel-VM oracle, JIT, and AOT readouts agree (bit-for-bit for
:int/:rational, within +DEFAULT-ULP-BOUND+ ulps for :float)."
  (%gate-eshkol-numeric surface :eshkol-command eshkol-command
                                 :aot-run-prefix aot-run-prefix :policy policy))
