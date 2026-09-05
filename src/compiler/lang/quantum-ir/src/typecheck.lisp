;;;; typecheck.lisp --- branch-sensitive linear quantum admission.

(in-package #:rosette-quantum-ir)

(defstruct (%signature (:constructor %make-signature))
  name kind parameters result-types body path)

(defstruct (%check-context (:constructor %make-check-context))
  (diagnostics nil :type list)
  (resources nil :type list)
  (signatures (make-hash-table :test #'eq) :type hash-table))

(defstruct (%block-outcome (:constructor %make-block-outcome))
  (terminated-p nil :type boolean)
  (types nil :type list))

(defun %copy-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %emit (context kind path control &rest arguments)
  (push (%make-diagnostic :kind kind
                          :path (copy-list path)
                          :message (apply #'format nil control arguments))
        (%check-context-diagnostics context))
  nil)

(defun %parse-types (context object path)
  (cond
    ((not (%proper-list-p object))
     (%emit context :syntax path "result types are not a proper list: ~S" object)
     nil)
    (t
     (loop for type in object
           for index from 0
           unless (%value-type-p type)
             do (%emit context :type (append path (list index))
                       "invalid value type ~S; expected :BIT, :REAL, or (:QSTATE positive-integer)"
                       type)
           collect type))))

(defun %parse-parameters (context object path)
  (let ((seen (make-hash-table :test #'eq))
        (parameters '()))
    (cond
      ((not (%proper-list-p object))
       (%emit context :syntax path "parameter list is not proper: ~S" object))
      (t
       (loop for parameter in object
             for index from 0
             for here = (append path (list index)) do
         (cond
           ((or (not (%proper-list-p parameter)) (/= (length parameter) 2))
            (%emit context :syntax here
                   "parameter must have shape (NAME TYPE), got ~S" parameter))
           (t
            (let ((name (first parameter)) (type (second parameter)))
              (unless (%resource-name-p name)
                (%emit context :syntax here "invalid parameter name ~S" name))
              (when (gethash name seen)
                (%emit context :redefinition here
                       "parameter ~S is declared more than once" name))
              (unless (%value-type-p type)
                (%emit context :type here "invalid parameter type ~S" type))
              (when (and (%resource-name-p name)
                         (not (gethash name seen)))
                (setf (gethash name seen) t)
                (push (list name type) parameters))))))))
    (nreverse parameters)))

(defun %parameterized-program-declaration-p (declaration)
  "Recognize the unambiguous parameterized :PROGRAM surface.

The old form is (:PROGRAM NAME (RESULT-TYPE...) BODY...).  The parameterized
form inserts ((NAME TYPE)...) before the result list.  A parameter list is
recognized only when every entry starts with an ordinary SSA name and the
following form is a valid result-type list.  This keeps old zero-result and
(:QSTATE n)-result programs unambiguous."
  (and (eq (first declaration) :program)
       (>= (length declaration) 4)
       (%proper-list-p (third declaration))
       (every (lambda (parameter)
                (and (%proper-list-p parameter)
                     (= (length parameter) 2)
                     (%resource-name-p (first parameter))))
              (third declaration))
       (%proper-list-p (fourth declaration))
       (every #'%value-type-p (fourth declaration))))

(defun %declaration-components (declaration)
  "Return KIND, NAME, PARAMETERS-FORM, RESULTS-FORM, BODY for valid syntax.

This is the one positional decoder shared by admission and semantic-tape
lowering; it does not create a second source representation."
  (let* ((kind (first declaration))
         (parameterized-program-p
           (%parameterized-program-declaration-p declaration)))
    (values kind
            (second declaration)
            (cond ((eq kind :circuit) (third declaration))
                  (parameterized-program-p (third declaration))
                  (t nil))
            (cond ((eq kind :circuit) (fourth declaration))
                  (parameterized-program-p (fourth declaration))
                  (t (third declaration)))
            (cond ((eq kind :circuit) (cddddr declaration))
                  (parameterized-program-p (cddddr declaration))
                  (t (cdddr declaration))))))

(defun %register-declarations (context declarations)
  (let ((seen (make-hash-table :test #'eq))
        (ordered '()))
    (loop for declaration in declarations
          for index from 0
          for path = (list :declaration index) do
      (cond
        ((or (not (%proper-list-p declaration)) (null declaration))
         (%emit context :syntax path "declaration is not a nonempty proper list: ~S"
                declaration))
        ((not (member (first declaration) '(:circuit :program)))
         (%emit context :syntax path "unknown declaration head ~S"
                (first declaration)))
        (t
         (let* ((kind (first declaration))
                (minimum (if (eq kind :circuit) 4 3)))
           (if (< (length declaration) minimum)
               (%emit context :syntax path "~S declaration is incomplete" kind)
               (multiple-value-bind (decoded-kind name parameter-form result-form body)
                   (%declaration-components declaration)
                 (declare (ignore decoded-kind))
                 (let* ((parameters
                          (if parameter-form
                              (%parse-parameters context parameter-form
                                                 (append path '(:parameters)))
                              nil))
                        (result-types
                          (%parse-types context result-form
                                        (append path '(:results)))))
                   (cond
                     ((not (and (symbolp name) name))
                      (%emit context :syntax (append path '(:name))
                             "invalid declaration name ~S" name))
                     ((gethash name seen)
                      (%emit context :redefinition (append path '(:name))
                             "declaration name ~S is not unique" name))
                     (t
                      (setf (gethash name seen) t)
                      (let ((signature
                              (%make-signature :name name :kind kind
                                               :parameters parameters
                                               :result-types result-types
                                               :body body :path path)))
                        (setf (gethash name (%check-context-signatures context))
                              signature)
                        (push signature ordered)))))))))))
    (nreverse ordered)))

(defun %lookup-value (context environment defined name path)
  (multiple-value-bind (type live-p) (gethash name environment)
    (cond
      (live-p type)
      ((gethash name defined)
       (%emit context
              (if (%quantum-type-p (gethash name defined))
                  :quantum-contraction
                  :undefined)
              path "quantum value ~S has already been consumed" name)
       nil)
      (t
       (%emit context :undefined path "value ~S is not defined here" name)
       nil))))

(defun %consume-value (context environment defined name path)
  (let ((type (%lookup-value context environment defined name path)))
    (when (%quantum-type-p type)
      (remhash name environment))
    type))

(defun %resource-profile-name (type)
  (if (%quantum-type-p type) :linear :unrestricted))

(defun %resource-transition (path)
  (case (car (last path))
    (:bit-output :measurement-classical-outcome)
    (:state-output :measurement-quantum-state)
    ((:left-output :right-output) :structural-copy-output)
    (:parameter :parameter)
    (otherwise :ordinary-definition)))

(defun %resource-derivations (name profile)
  (let ((identity (rosette-substructural:prove (list name) name :logic :linear)))
    (ecase profile
      (:linear (list identity))
      (:unrestricted
       (list identity
             (rosette-substructural:prove (list name :unused) name
                                      :logic :unrestricted)
             (rosette-substructural:prove
              (list name) (list :tensor name name)
              :logic :unrestricted))))))

(defun %record-resource-certificate (context name type path)
  (let* ((profile-name (%resource-profile-name type))
         (profile (rosette-substructural:logic-structural-profile profile-name)))
    (push (%make-resource-certificate
           :name name
           :type (copy-tree type)
           :profile profile-name
           :structural-rules
           (rosette-substructural:structural-profile-rules profile)
           :defined-at (copy-list path)
           :transition (%resource-transition path)
           :derivations (%resource-derivations name profile-name))
          (%check-context-resources context))))

(defun verify-resource-certificate (certificate)
  "Fail closed unless CERTIFICATE exactly replays its canonical assignment."
  (and (resource-certificate-p certificate)
       (%resource-name-p (resource-certificate-name certificate))
       (%value-type-p (resource-certificate-type certificate))
       (%proper-list-p (resource-certificate-defined-at certificate))
       (consp (resource-certificate-defined-at certificate))
       (let* ((name (resource-certificate-name certificate))
              (type (resource-certificate-type certificate))
              (expected (%resource-profile-name type))
              (profile
                (handler-case
                    (rosette-substructural:logic-structural-profile expected)
                  (error () nil)))
              (expected-transition
                (%resource-transition
                 (resource-certificate-defined-at certificate)))
              (expected-derivations
                (handler-case
                    (%resource-derivations name expected)
                  (error () nil))))
         (and profile
              expected-derivations
              (eq expected (resource-certificate-profile certificate))
              (equal (rosette-substructural:structural-profile-rules profile)
                     (resource-certificate-structural-rules certificate))
              (eq expected-transition
                  (resource-certificate-transition certificate))
              (equalp expected-derivations
                      (resource-certificate-derivations certificate))
              (every (lambda (derivation)
                       (and (rosette-substructural:deriv-p derivation)
                            (rosette-substructural:legal-in-p derivation expected)))
                     (resource-certificate-derivations certificate))
              (case (resource-certificate-transition certificate)
                (:measurement-classical-outcome (eq expected :unrestricted))
                (:measurement-quantum-state (eq expected :linear))
                (:structural-copy-output (eq expected :unrestricted))
                ((:parameter :ordinary-definition) t)
                (otherwise nil))))))

(defun %define-value (context environment defined name type path)
  (cond
    ((not (%resource-name-p name))
     (%emit context :syntax path "invalid SSA value name ~S" name))
    ((gethash name defined)
     (%emit context :redefinition path "SSA value ~S is defined more than once" name))
    ((not (%value-type-p type))
     (%emit context :type path "cannot define ~S with invalid type ~S" name type))
    (t
     (setf (gethash name environment) type
           (gethash name defined) type)
     (%record-resource-certificate context name type path)
     type)))

(defun %check-gate (context gate environment defined path)
  "Validate GATE and return its target arity, or NIL after a diagnostic."
  (cond
    ((member gate '(:h :x :y :z :s :t)) 1)
    ((member gate '(:cnot :cz :swap)) 2)
    ((and (%proper-list-p gate)
          (= (length gate) 2)
          (member (first gate) '(:rx :ry :rz)))
     (let ((angle (second gate)))
       (cond
         ((realp angle) 1)
         ((%resource-name-p angle)
          (let ((type (%lookup-value context environment defined angle
                                     (append path '(:angle)))))
            (if (eq type :real)
                1
                (progn
                  (when type
                    (%emit context :type (append path '(:angle))
                           "rotation angle ~S has type ~S, not :REAL" angle type))
                  nil))))
         (t
          (%emit context :type (append path '(:angle))
                 "rotation angle ~S is neither a real numeric literal nor a live :REAL value"
                 angle)
          nil))))
    ((and (consp gate) (member (first gate) '(:rx :ry :rz)))
     (%emit context :type path
            "parameterized gate must have shape (:RX ANGLE), (:RY ANGLE), or (:RZ ANGLE), got ~S"
            gate))
    (t
     (%emit context :type path "unsupported gate ~S" gate))))

(defun %check-targets (context type targets path &key arity (allow-empty nil))
  (let ((ok t) (seen '()))
    (unless (%proper-list-p targets)
      (%emit context :syntax path "targets are not a proper list: ~S" targets)
      (return-from %check-targets nil))
    (when (and (not allow-empty) (null targets))
      (%emit context :target path "at least one qubit target is required")
      (setf ok nil))
    (when (and arity (/= (length targets) arity))
      (%emit context :target path "operation wants ~D target~:P, got ~D"
             arity (length targets))
      (setf ok nil))
    (loop for target in targets
          for index from 0
          for here = (append path (list index)) do
      (cond
        ((not (integerp target))
         (%emit context :target here "target ~S is not an integer" target)
         (setf ok nil))
        ((or (minusp target)
             (and (%quantum-type-p type) (>= target (second type))))
         (%emit context :target here "target ~S is outside quantum state type ~S"
                target type)
         (setf ok nil))
        ((member target seen)
         (%emit context :target here "target ~D is repeated" target)
         (setf ok nil))
        (t (push target seen))))
    ok))

(defun %live-quantum-values (environment)
  (let ((result '()))
    (maphash (lambda (name type)
               (when (%quantum-type-p type) (push (cons name type) result)))
             environment)
    (nreverse result)))

(defun %check-return (context form environment defined path)
  (let ((types '()))
    (loop for value in (rest form)
          for index from 0
          for type = (%consume-value context environment defined value
                                     (append path (list :value index)))
          when type do (push type types))
    (dolist (entry (%live-quantum-values environment))
      (%emit context :quantum-weakening path
             "quantum value ~S of type ~S reaches return unused; use :DISCARD explicitly"
             (car entry) (cdr entry)))
    (%make-block-outcome :terminated-p t :types (nreverse types))))

(declaim (ftype function %check-block))

(defun %check-case (context form environment defined path)
  (unless (= (length form) 4)
    (%emit context :syntax path
           ":CASE wants BIT, (:ZERO ...), and (:ONE ...), got ~S" form)
    (return-from %check-case (%make-block-outcome :terminated-p nil)))
  (let ((selector-type (%lookup-value context environment defined (second form)
                                      (append path '(:selector))))
        (branches (cddr form))
        (seen-labels '())
        (outcomes '()))
    (when (and selector-type (not (eq selector-type :bit)))
      (%emit context :type (append path '(:selector))
             ":CASE selector ~S has type ~S, not :BIT" (second form) selector-type))
    (dolist (branch branches)
      (let ((label (and (consp branch) (first branch))))
        (cond
          ((or (not (%proper-list-p branch))
               (not (member label '(:zero :one))))
           (%emit context :syntax path
                  "case branch must be (:ZERO ...) or (:ONE ...), got ~S" branch))
          ((member label seen-labels)
           (%emit context :syntax path "duplicate case branch ~S" label))
          (t
           (push label seen-labels)
           (push (%check-block context (rest branch)
                               (%copy-table environment) (%copy-table defined)
                               (append path (list label)))
                 outcomes)))))
    (unless (and (member :zero seen-labels) (member :one seen-labels))
      (%emit context :syntax path ":CASE requires exactly :ZERO and :ONE branches"))
    (setf outcomes (nreverse outcomes))
    (cond
      ((or (/= (length outcomes) 2)
           (not (every #'%block-outcome-terminated-p outcomes)))
       (%make-block-outcome :terminated-p nil))
      ((not (equal (%block-outcome-types (first outcomes))
                   (%block-outcome-types (second outcomes))))
       (%emit context :branch-shape path
              "case branches return different resource shapes ~S and ~S"
              (%block-outcome-types (first outcomes))
              (%block-outcome-types (second outcomes)))
       (%make-block-outcome :terminated-p t
                            :types (%block-outcome-types (first outcomes))))
      (t (first outcomes)))))

(defun %check-call (context form environment defined path)
  (unless (= (length form) 4)
    (%emit context :syntax path ":CALL wants (OUT...), NAME, and (ARG...), got ~S" form)
    (return-from %check-call nil))
  (destructuring-bind (head outputs name arguments) form
    (declare (ignore head))
    (unless (%proper-list-p outputs)
      (%emit context :syntax (append path '(:outputs))
             "call outputs are not a proper list: ~S" outputs)
      (setf outputs nil))
    (unless (%proper-list-p arguments)
      (%emit context :syntax (append path '(:arguments))
             "call arguments are not a proper list: ~S" arguments)
      (setf arguments nil))
    (let ((signature (gethash name (%check-context-signatures context))))
      (cond
        ((null signature)
         (%emit context :signature path "called circuit ~S is not defined" name))
        ((not (eq (%signature-kind signature) :circuit))
         (%emit context :signature path "~S names a closed program, not a circuit" name))
        (t
         (let ((parameter-types
                 (mapcar #'second (%signature-parameters signature)))
               (result-types (%signature-result-types signature)))
           (unless (= (length arguments) (length parameter-types))
             (%emit context :signature (append path '(:arguments))
                    "circuit ~S wants ~D argument~:P, got ~D"
                    name (length parameter-types) (length arguments)))
           (loop for argument in arguments
                 for index from 0
                 for expected = (nth index parameter-types) do
             (let ((actual
                     (cond
                       ((realp argument) :real)
                       ((numberp argument)
                        (%emit context :type
                               (append path (list :argument index))
                               "numeric call argument ~S is not a real number"
                               argument))
                       (t
                        (%consume-value
                         context environment defined argument
                         (append path (list :argument index)))))))
               (when (and expected actual (not (equal expected actual)))
                 (if (realp argument)
                     (%emit context :type
                            (append path (list :argument index))
                            "numeric literal ~S may be passed only to a :REAL formal; circuit ~S expects ~S"
                            argument name expected)
                     (%emit context :type
                            (append path (list :argument index))
                            "argument ~S has type ~S; circuit ~S expects ~S"
                            argument actual name expected)))))
           (unless (= (length outputs) (length result-types))
             (%emit context :signature (append path '(:outputs))
                    "circuit ~S returns ~D value~:P, but call binds ~D"
                    name (length result-types) (length outputs)))
           (loop for output in outputs
                 for type in result-types
                 for index from 0
                 do (%define-value context environment defined output type
                                   (append path (list :output index)))))))))
  nil)

(defun %check-statement (context form environment defined path)
  (cond
    ((or (not (%proper-list-p form)) (null form))
     (%emit context :syntax path "statement is not a nonempty proper list: ~S" form))
    (t
     (case (first form)
       (:prepare
        (if (/= (length form) 4)
            (%emit context :syntax path ":PREPARE wants OUT, N, and BASIS-INDEX")
            (destructuring-bind (head output n index) form
              (declare (ignore head))
              (unless (and (integerp n) (plusp n))
                (%emit context :type path "prepare width must be positive, got ~S" n))
              (unless (and (integerp index) (not (minusp index))
                           (integerp n) (plusp n) (< index (expt 2 n)))
                (%emit context :type path "basis index ~S is outside width ~S" index n))
              (when (and (integerp n) (plusp n))
                (%define-value context environment defined output (list :qstate n)
                               (append path '(:output)))))))
       (:apply
        (if (< (length form) 5)
            (%emit context :syntax path ":APPLY wants OUT, IN, GATE, and target(s)")
            (destructuring-bind (head output input gate &rest targets) form
              (declare (ignore head))
              (let ((type (%consume-value context environment defined input
                                          (append path '(:input))))
                    (arity (%check-gate context gate environment defined
                                        (append path '(:gate)))))
                (unless (%quantum-type-p type)
                  (%emit context :type path ":APPLY input ~S is not quantum" input))
                (when (and type arity)
                  (%check-targets context type targets (append path '(:targets))
                                  :arity arity)
                  (%define-value context environment defined output type
                                 (append path '(:output))))))))
       (:measure
        (if (/= (length form) 5)
            (%emit context :syntax path
                   ":MEASURE wants BIT-OUT, STATE-OUT, IN, and TARGET")
            (destructuring-bind (head bit-output state-output input target) form
              (declare (ignore head))
              (let ((type (%consume-value context environment defined input
                                          (append path '(:input)))))
                (unless (%quantum-type-p type)
                  (%emit context :type path ":MEASURE input ~S is not quantum" input))
                (when (%quantum-type-p type)
                  (%check-targets context type (list target)
                                  (append path '(:target)) :arity 1)
                  (%define-value context environment defined bit-output :bit
                                 (append path '(:bit-output)))
                  (%define-value context environment defined state-output type
                                 (append path '(:state-output))))))))
       (:copy
        (if (/= (length form) 4)
            (%emit context :syntax path
                   ":COPY wants LEFT-OUT, RIGHT-OUT, and INPUT")
            (destructuring-bind (head left-output right-output input) form
              (declare (ignore head))
              (let ((type
                      (%lookup-value context environment defined input
                                     (append path '(:input)))))
                (when type
                  (let* ((profile-name (%resource-profile-name type))
                         (profile
                           (rosette-substructural:logic-structural-profile
                            profile-name)))
                    (if (rosette-substructural:structural-profile-contraction-p
                         profile)
                        (progn
                          (%define-value
                           context environment defined left-output type
                           (append path '(:left-output)))
                          (%define-value
                           context environment defined right-output type
                           (append path '(:right-output))))
                        (%emit
                         context
                         (if (%quantum-type-p type)
                             :quantum-contraction
                             :structural-contraction)
                         path
                         ":COPY is not licensed by the ~S profile of ~S"
                         profile-name input))))))))
       (:case (return-from %check-statement
                (%check-case context form environment defined path)))
       (:discard
        (cond
          ((= (length form) 2)
           (let ((type (%consume-value context environment defined (second form)
                                       (append path '(:input)))))
             (unless (%quantum-type-p type)
               (%emit context :type path ":DISCARD input is not quantum"))))
          ((>= (length form) 4)
           (destructuring-bind (head output input &rest targets) form
             (declare (ignore head))
             (let ((type (%consume-value context environment defined input
                                         (append path '(:input)))))
               (unless (%quantum-type-p type)
                 (%emit context :type path ":DISCARD input is not quantum"))
               (when (and (%quantum-type-p type)
                          (%check-targets context type targets
                                          (append path '(:targets))))
                 (let ((remaining (- (second type) (length targets))))
                   (if (plusp remaining)
                       (%define-value context environment defined output
                                      (list :qstate remaining)
                                      (append path '(:output)))
                       (%emit context :type path
                              "discarding every qubit has no output; use (:DISCARD ~S)"
                              input)))))))
          (t (%emit context :syntax path
                    ":DISCARD is (:DISCARD IN) or (:DISCARD OUT IN TARGET...)"))))
       (:channel
        (if (< (length form) 4)
            (%emit context :syntax path
                   ":CHANNEL wants OUT, IN, CHANNEL-NAME, and optional targets")
            (destructuring-bind (head output input channel &rest targets) form
              (declare (ignore head))
              (let ((type (%consume-value context environment defined input
                                          (append path '(:input)))))
                (unless (%quantum-type-p type)
                  (%emit context :type path ":CHANNEL input is not quantum"))
                (unless (and (symbolp channel) channel)
                  (%emit context :type path "invalid channel name ~S" channel))
                (when (%quantum-type-p type)
                  (%check-targets context type targets (append path '(:targets))
                                  :allow-empty t)
                  (%define-value context environment defined output type
                                 (append path '(:output))))))))
       (:reset
        (if (< (length form) 4)
            (%emit context :syntax path ":RESET wants OUT, IN, and target(s)")
            (destructuring-bind (head output input &rest targets) form
              (declare (ignore head))
              (let ((type (%consume-value context environment defined input
                                          (append path '(:input)))))
                (unless (%quantum-type-p type)
                  (%emit context :type path ":RESET input is not quantum"))
                (when (%quantum-type-p type)
                  (%check-targets context type targets (append path '(:targets)))
                  (%define-value context environment defined output type
                                 (append path '(:output))))))))
       (:call (%check-call context form environment defined path))
       (:return (return-from %check-statement
                  (%check-return context form environment defined path)))
       (otherwise
        (%emit context :syntax path "unknown statement head ~S" (first form))))))
  nil)

(defun %check-block (context statements environment defined path)
  (let ((outcome nil))
    (loop for statement in statements
          for index from 0
          for here = (append path (list :statement index)) do
      (if outcome
          (%emit context :syntax here "statement follows terminating :RETURN or :CASE")
          (setf outcome (%check-statement context statement environment defined here))))
    (or outcome
        (progn
          (%emit context :syntax path "block has no terminating :RETURN or :CASE")
          (%make-block-outcome :terminated-p nil)))))

(defun %check-definition (context signature)
  (let* ((before (length (%check-context-diagnostics context)))
         (environment (make-hash-table :test #'eq))
         (defined (make-hash-table :test #'eq)))
    (dolist (parameter (%signature-parameters signature))
      (%define-value context environment defined (first parameter) (second parameter)
                     (append (%signature-path signature) (list :parameter (first parameter)))))
    (let ((outcome (%check-block context (%signature-body signature)
                                 environment defined
                                 (append (%signature-path signature) '(:body)))))
      (when (and (%block-outcome-terminated-p outcome)
                 (not (equal (%block-outcome-types outcome)
                             (%signature-result-types signature))))
        (%emit context :signature (append (%signature-path signature) '(:results))
               "declared result types ~S do not match returned resource shape ~S"
               (%signature-result-types signature) (%block-outcome-types outcome))))
    (list :name (%signature-name signature)
          :kind (%signature-kind signature)
          :parameter-types (mapcar #'second (%signature-parameters signature))
          :result-types (copy-tree (%signature-result-types signature))
          :ok-p (= before (length (%check-context-diagnostics context))))))

(defun %statement-calls (statements)
  (let ((calls '()))
    (labels ((walk (forms)
               (dolist (form forms)
                 (when (consp form)
                   (cond
                     ((and (eq (first form) :call) (>= (length form) 3))
                      (push (third form) calls))
                     ((eq (first form) :case)
                      (dolist (branch (cddr form))
                        (when (consp branch)
                          (walk (rest branch))))))))))
      (walk statements))
    (nreverse calls)))

(defun %check-call-cycles (context signatures)
  (let ((marks (make-hash-table :test #'eq))
        (reported (make-hash-table :test #'eq)))
    (labels ((visit (signature stack)
               (let ((name (%signature-name signature)))
                 (case (gethash name marks)
                   (:black nil)
                   (:gray
                    (unless (gethash name reported)
                      (setf (gethash name reported) t)
                      (%emit context :call-cycle (%signature-path signature)
                             "recursive circuit call cycle reaches ~S through ~S"
                             name (reverse (cons name stack)))))
                   (otherwise
                    (setf (gethash name marks) :gray)
                    (dolist (target (%statement-calls (%signature-body signature)))
                      (let ((next (gethash target (%check-context-signatures context))))
                        (when (and next (eq (%signature-kind next) :circuit))
                          (visit next (cons name stack)))))
                    (setf (gethash name marks) :black))))))
      (dolist (signature signatures)
        (when (eq (%signature-kind signature) :circuit)
          (visit signature nil))))))

(defun %check-quantum-unit (carrier &key kind-override)
  "Internal checker for an already normalized :QUANTUM-UNIT carrier."
  (let* ((context (%make-check-context))
         (form (handler-case (rosette-program-ir:program->lisp carrier)
                 (error (condition)
                   (%emit context :syntax nil "ProgramIR projection failed: ~A" condition)
                   nil))))
    (cond
      ((or (not (%proper-list-p form)) (not (eq (first form) :quantum-unit)))
       (%emit context :syntax nil "root must be :QUANTUM-UNIT, got ~S"
              (and (consp form) (first form))))
      ((null (rest form))
       (%emit context :syntax nil ":QUANTUM-UNIT contains no declarations")))
    (let* ((signatures (if (and (consp form) (eq (first form) :quantum-unit))
                           (%register-declarations context (rest form))
                           nil))
           (summaries (mapcar (lambda (signature)
                                (%check-definition context signature))
                              signatures))
           (definitions (remove :circuit summaries :key (lambda (x) (getf x :kind))
                                :test-not #'eq))
           (programs (remove :program summaries :key (lambda (x) (getf x :kind))
                             :test-not #'eq)))
      (%check-call-cycles context signatures)
      (let* ((errors (nreverse (%check-context-diagnostics context)))
             (kind (or kind-override
                       (cond ((and definitions programs) :mixed-unit)
                             (programs :closed-unit)
                             (definitions :open-unit)
                             (t :empty-unit)))))
        (%make-program-report :ok-p (null errors) :kind kind
                              :definitions definitions :programs programs
                              :resources (nreverse (%check-context-resources context))
                              :errors errors)))))
