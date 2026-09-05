;;;; terms.lisp --- honest shared STLC and exact expression-core bridge.

(in-package #:rosette-front-door)

;;; -------------------------------------------------------------------------
;;; One closed simply-typed lambda fragment, three meanings.
;;; -------------------------------------------------------------------------

(defun %arrow-type-p (type)
  (and (consp type)
       (= (length type) 3)
       (%symbol-name= (first type) "->")))

(defun %same-type-p (left right)
  (cond
    ((and (eq left :int) (eq right :int)) t)
    ((and (%arrow-type-p left) (%arrow-type-p right))
     (and (%same-type-p (second left) (second right))
          (%same-type-p (third left) (third right))))
    (t nil)))

(defun %supported-type-p (type)
  (or (eq type :int)
      (and (%arrow-type-p type)
           (%supported-type-p (second type))
           (%supported-type-p (third type)))))

(defun %lookup-bound-type (name environment term)
  (let ((cell (assoc (symbol-name name) environment :test #'string=)))
    (or (cdr cell)
        (%refuse :shared-term term "free variable ~A" name))))

(defun %infer-shared-type (term environment)
  (cond
    ((integerp term)
     (when (minusp term)
       (%refuse :shared-term term
                "BLC readout admits non-negative Church numerals only"))
     :int)
    ((symbolp term) (%lookup-bound-type term environment term))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((%symbol-name= head "LAM")
          (unless (= (length term) 4)
            (%refuse :shared-term term "LAM wants binder, type, and body"))
          (let ((binder (second term)) (domain (third term)))
            (unless (symbolp binder)
              (%refuse :shared-term term "LAM binder must be a symbol"))
            (unless (%supported-type-p domain)
              (%refuse :shared-term term "unsupported lambda domain ~S" domain))
            (list '-> domain
                  (%infer-shared-type
                   (fourth term)
                   (acons (symbol-name binder) domain environment)))))
         ((%symbol-name= head "APP")
          (unless (= (length term) 3)
            (%refuse :shared-term term "APP wants a function and an argument"))
          (let ((function-type (%infer-shared-type (second term) environment))
                (argument-type (%infer-shared-type (third term) environment)))
            (unless (%arrow-type-p function-type)
              (%refuse :shared-term term "APP function does not have arrow type"))
            (unless (%same-type-p (second function-type) argument-type)
              (%refuse :shared-term term "APP argument type does not match ~S"
                       (second function-type)))
            (third function-type)))
         ((%symbol-name= head "LET")
          (unless (= (length term) 4)
            (%refuse :shared-term term "LET wants binder, value, and body"))
          (unless (symbolp (second term))
            (%refuse :shared-term term "LET binder must be a symbol"))
          (let ((value-type (%infer-shared-type (third term) environment)))
            (%infer-shared-type
             (fourth term)
             (acons (symbol-name (second term)) value-type environment))))
         (t
          (%refuse :shared-term term
                   "outside the shared fragment (only literal/variable/LAM/APP/LET)")))))
    (t (%refuse :shared-term term "not a term"))))

(defun %validate-domain (integer-domain)
  (unless (and (%proper-list-p integer-domain)
               integer-domain
               (every #'integerp integer-domain)
               (= (length integer-domain)
                  (length (remove-duplicates integer-domain :test #'eql))))
    (%refuse :ccc integer-domain
             "INTEGER-DOMAIN must be a non-empty list of distinct integers"))
  (copy-list integer-domain))

(defun %ccc-type (type integer-domain)
  (cond
    ((eq type :int) (list :base :front-door-int integer-domain))
    ((%arrow-type-p type)
     (list :arr (%ccc-type (second type) integer-domain)
           (%ccc-type (third type) integer-domain)))
    (t (%refuse :ccc type "unsupported type"))))

(defun %de-bruijn-index (symbol environment stage term)
  (or (position (symbol-name symbol) environment :test #'string=)
      (%refuse stage term "free variable ~A" symbol)))

(defun %to-ccc (term environment type-environment integer-domain)
  (cond
    ((integerp term)
     (unless (member term integer-domain :test #'eql)
       (%refuse :ccc term "literal is outside INTEGER-DOMAIN"))
     (list :const term (%ccc-type :int integer-domain)))
    ((symbolp term)
     (list :var (%de-bruijn-index term environment :ccc term)))
    ((%symbol-name= (first term) "LAM")
     (list :lam (%ccc-type (third term) integer-domain)
           (%to-ccc (fourth term)
                    (cons (symbol-name (second term)) environment)
                    (acons (symbol-name (second term))
                           (third term) type-environment)
                    integer-domain)))
    ((%symbol-name= (first term) "APP")
     (list :app (%to-ccc (second term) environment type-environment
                         integer-domain)
           (%to-ccc (third term) environment type-environment
                    integer-domain)))
    ((%symbol-name= (first term) "LET")
     ;; LET x=e in b is exactly (lambda x.b) e in this pure fragment.
     (let ((value-type (%infer-shared-type (third term) type-environment)))
       (list :app
             (list :lam (%ccc-type value-type integer-domain)
                   (%to-ccc (fourth term)
                            (cons (symbol-name (second term)) environment)
                            (acons (symbol-name (second term)) value-type
                                   type-environment)
                            integer-domain))
             (%to-ccc (third term) environment type-environment
                      integer-domain))))
    (t (%refuse :ccc term "outside the shared fragment"))))

(defun core-term->ccc (term &key integer-domain)
  "Translate a closed shared-fragment core TERM to rosette-ccc's FinSet STLC.

INTEGER-DOMAIN is mandatory and finite. It makes the chosen FinSet model
explicit; a literal or result outside it is refused rather than wrapped."
  (let ((domain (%validate-domain integer-domain)))
    (unless (%same-type-p :int (%infer-shared-type term '()))
      (%refuse :ccc term "a closed gauge term must return :INT"))
    (%to-ccc term '() '() domain)))

(defun %to-blc (term environment)
  (cond
    ((integerp term) (rosette-blc:church-numeral term))
    ((symbolp term)
     (rosette-blc:tvar (%de-bruijn-index term environment :blc term)))
    ((%symbol-name= (first term) "LAM")
     (rosette-blc:tlam
      (%to-blc (fourth term)
               (cons (symbol-name (second term)) environment))))
    ((%symbol-name= (first term) "APP")
     (rosette-blc:tapp (%to-blc (second term) environment)
                   (%to-blc (third term) environment)))
    ((%symbol-name= (first term) "LET")
     (rosette-blc:tapp
      (rosette-blc:tlam
       (%to-blc (fourth term)
                (cons (symbol-name (second term)) environment)))
      (%to-blc (third term) environment)))
    (t (%refuse :blc term "outside the shared fragment"))))

(defun core-term->blc (term)
  "Erase types from a closed shared-fragment core TERM into BLC."
  (unless (%same-type-p :int (%infer-shared-type term '()))
    (%refuse :blc term "a closed gauge term must return :INT"))
  (%to-blc term '()))

;;; -------------------------------------------------------------------------
;;; The extensional (FinSet CCC) gauge is enumerative by design: rosette-ccc denotes
;;; an arrow type (:-> A B) as the WHOLE exponential object B^A = all functions
;;; A -> B, which is what makes the higher-order categorical laws decidable by
;;; EQUAL.  Its cost is therefore |B|^|A| -- and, being a function of the term's
;;; type and the domain size, it is a LOCATED wall we can price a priori rather
;;; than a hang we discover.  |:int| = |domain|; |(-> A B)| = |B|^|A|.
;;; -------------------------------------------------------------------------

(defun %type-card (type n)
  "Cardinality of the FinSet object for TYPE over an N-element integer domain."
  (cond ((eq type :int) n)
        ((%arrow-type-p type)
         (expt (%type-card (third type) n) (%type-card (second type) n)))
        (t 1)))

(defun ccc-gauge-cost (term &key integer-domain)
  "The largest FinSet exponential object the CCC gauge would materialize to
denote TERM over INTEGER-DOMAIN -- the magnitude of the extensional gauge's
located wall.  Pure; never denotes anything, so it is always cheap to ask."
  (let ((domain (%validate-domain integer-domain)))
    (let ((n (length domain)) (worst 1))
      (labels ((walk (tm env)
                 (let ((ty (ignore-errors (%infer-shared-type tm env))))
                   (when (and ty (%arrow-type-p ty))
                     (setf worst (max worst (%type-card ty n))))
                   (when (consp tm)
                     (cond
                       ((%symbol-name= (first tm) "LAM")
                        (walk (fourth tm)
                              (acons (symbol-name (second tm)) (third tm) env)))
                       ((%symbol-name= (first tm) "APP")
                        (walk (second tm) env) (walk (third tm) env))
                       ((%symbol-name= (first tm) "LET")
                        (let ((vt (ignore-errors (%infer-shared-type (third tm) env))))
                          (walk (third tm) env)
                          (when vt
                            (walk (fourth tm)
                                  (acons (symbol-name (second tm)) vt env))))))))))
        (walk term '())
        worst))))

(defstruct (term-gauge-report
            (:constructor %make-term-gauge-report
                (&key term ccc-term blc-term core-value ccc-value blc-value
                      blc-length ccc-cost ccc-skipped passed certificate)))
  term
  ccc-term
  blc-term
  core-value
  ccc-value                             ; :WALL when the extensional gauge is skipped
  blc-value
  blc-length
  ccc-cost                              ; |B|^|A| the CCC gauge would materialize
  (ccc-skipped nil :type boolean)       ; T when ccc-cost exceeded the budget
  (passed nil :type boolean)
  certificate)

(defun %compute-term-gauges (term integer-domain &optional budget)
  (let* ((domain (%validate-domain integer-domain))
         (type (%infer-shared-type term '())))
    (unless (%same-type-p type :int)
      (%refuse :shared-term term "the closed term must return :INT, got ~S" type))
    (let* ((cost (ccc-gauge-cost term :integer-domain domain))
           (skip-ccc (and budget (> cost budget)))
           (program (rosette-core-term:make-core-program :funs nil
                                                     :main (copy-tree term)))
           (core-value (rosette-core-term:program-eval program))
           (ccc-term (%to-ccc term '() '() domain))
           (blc-term (%to-blc term '()))
           ;; the ccc AST is cheap; only its DENOTATION materializes B^A.
           (ccc-value (if skip-ccc :wall
                          (rosette-ccc:mapply (rosette-ccc:denote '() ccc-term) :*)))
           (blc-value (rosette-blc-reduce:reduce-church blc-term)))
      (unless (member core-value domain :test #'eql)
        (%refuse :ccc term "core result ~A is outside INTEGER-DOMAIN" core-value))
      (values core-value ccc-value blc-value ccc-term blc-term
              (rosette-blc:blc-length blc-term) cost skip-ccc))))

(defun %verify-term-gauge-certificate (certificate)
  (handler-case
      (let* ((payload (rosette-proof-witness:certificate-payload certificate))
             (term (getf payload :term))
             (domain (getf payload :integer-domain))
             (skipped (getf payload :ccc-skipped))
             ;; re-skip deterministically at the recorded threshold.
             (budget (when skipped (max 0 (1- (getf payload :ccc-cost))))))
        (multiple-value-bind (core ccc blc ccc-term blc-term length cost re-skip)
            (%compute-term-gauges term domain budget)
          (and (eql core (getf payload :core-value))
               (eql blc (getf payload :blc-value))
               (equal ccc-term (getf payload :ccc-term))
               (equal blc-term (getf payload :blc-term))
               (= length (getf payload :blc-length))
               (eql cost (getf payload :ccc-cost))
               (eq (and re-skip t) (and skipped t))
               (eql core blc)
               (if skipped
                   t                     ; extensional gauge is a located wall, not re-run
                   (and (eql ccc (getf payload :ccc-value))
                        (eql core ccc))))))
    (error () nil)))

(defun gauge-term (term &key integer-domain budget)
  "Read one closed typed-lambda TERM three ways and certify agreement.

The readouts are rosette-core-term evaluation (operational), rosette-ccc FinSet
denotation (extensional), and BLC Church reduction (intensional). The BLC bit
length is measured from the same erased term. The result's certificate re-runs
the meanings.

:BUDGET, when given, caps the extensional gauge: if the exponential object it
would materialize (|B|^|A|, = CCC-GAUGE-COST) exceeds BUDGET, that gauge is
reported as a LOCATED WALL -- skipped (CCC-VALUE :WALL, CCC-SKIPPED T, CCC-COST
the magnitude) and agreement is gated on the operational and BLC gauges only.
Without :BUDGET the full three-way triangulation runs, exactly as before."
  (let ((domain (%validate-domain integer-domain)))
    (multiple-value-bind (core ccc blc ccc-term blc-term length cost skipped)
        (%compute-term-gauges term domain budget)
      (let* ((passed (if skipped (eql core blc) (and (eql core ccc) (eql core blc))))
             (certificate
               (rosette-proof-witness:make-certificate
                :name :front-door-term-gauge
                :kind :proof
                :claim (if skipped
                           :shared-term-agreement-ccc-walled
                           :shared-term-three-way-agreement)
                :payload (list :term (copy-tree term)
                               :integer-domain (copy-list domain)
                               :ccc-term (copy-tree ccc-term)
                               :blc-term (copy-tree blc-term)
                               :core-value core
                               :ccc-value ccc
                               :blc-value blc
                               :blc-length length
                               :ccc-cost cost
                               :ccc-skipped skipped)
                :passed passed
                :metadata '(:meanings (:core-eval :finset-ccc :blc-reduction)))))
        (setf (rosette-proof-witness:certificate-witness certificate)
              (rosette-proof-witness:make-lean-witness
               :front-door-term-gauge-re-runs
               (lambda () (%verify-term-gauge-certificate certificate))))
        (%make-term-gauge-report
         :term (copy-tree term)
         :ccc-term ccc-term
         :blc-term blc-term
         :core-value core
         :ccc-value ccc
         :blc-value blc
         :blc-length length
         :ccc-cost cost
         :ccc-skipped skipped
         :passed passed
         :certificate certificate)))))

;;; -------------------------------------------------------------------------
;;; expression-core bridge: exact integer polynomial syntax, nothing implicit.
;;; -------------------------------------------------------------------------

(defun %binding (name bindings node)
  (let ((cell (find-if
               (lambda (entry)
                 (and (consp entry)
                      (symbolp (car entry))
                      (string= (symbol-name name) (symbol-name (car entry)))))
               bindings)))
    (unless cell
      (%refuse :cf-expression node "unbound expression variable ~A" name))
    (unless (integerp (cdr cell))
      (%refuse :cf-expression cell "variable bindings must be exact integers"))
    (cdr cell)))

(defun %exact-integer-constant (value node)
  (unless (realp value)
    (%refuse :cf-expression node "constant is not real"))
  (handler-case
      (multiple-value-bind (integer remainder) (truncate value)
        (unless (zerop remainder)
          (%refuse :cf-expression node "constant ~S is not an exact integer" value))
        integer)
    (arithmetic-error ()
      (%refuse :cf-expression node "constant ~S is not finite" value))))

(defun cf-expression->core-term (expression &key (bindings '()))
  "Lower an admitted exact expression-core polynomial to a closed core term.

Admitted nodes are :CONST, :VAR, :ADD, :SUB, and :MUL. Constants and variable
bindings must be exact integers. Division, transcendental/special functions,
and piecewise selection are refused because their floating semantics are not
the integer core semantics."
  (labels ((lower-node (node)
             (unless (typep node 'rosette-expression-core:cf-node)
               (%refuse :cf-expression node "expected an expression-core CF-NODE"))
             (let ((operator (rosette-expression-core:cf-node-op node))
                   (arguments (rosette-expression-core:cf-node-args node)))
               (case operator
                 (:const (%exact-integer-constant (first arguments) node))
                 (:var (%binding (first arguments) bindings node))
                 (:add (list '+ (lower-node (first arguments))
                             (lower-node (second arguments))))
                 (:sub (list '- (lower-node (first arguments))
                             (lower-node (second arguments))))
                 (:mul (list '* (lower-node (first arguments))
                             (lower-node (second arguments))))
                 (otherwise
                  (%refuse :cf-expression node
                           "operator ~S has no exact integer-core admission"
                           operator))))))
    (lower-node expression)))

(defun run-cf-expression (expression &key (bindings '()))
  "Admit an exact CF expression and send the resulting term through RUN."
  (run (list (list 'main
                   (cf-expression->core-term expression :bindings bindings)))))
