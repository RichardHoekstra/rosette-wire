;;;; interpreter.lisp --- deterministic execution of immutable quantum tapes.

(in-package #:rosette-quantum-vm)

(defstruct (%bound-real
            (:constructor %make-bound-real (value root))
            (:conc-name %bound-real-)
            (:copier nil))
  value root)

(defstruct (%gate-step
            (:constructor %make-gate-step
                (index opcode targets angle parameter-root before after path))
            (:conc-name %gate-step-)
            (:copier nil))
  index opcode targets angle parameter-root before after path)

(defstruct (%vm-context
            (:constructor %make-vm-context
                (&key tape prototype shift-index shift-delta))
            (:conc-name %context-))
  tape prototype
  (gate-index 0 :type fixnum)
  (trace nil :type list)
  shift-index shift-delta
  (call-stack nil :type list))

(defstruct (execution-result
            (:constructor %make-execution-result
                (state tape bindings trace gate-count receipt fingerprint))
            (:conc-name %execution-)
            (:copier nil))
  (state nil :read-only t)
  (tape nil :read-only t)
  (bindings nil :read-only t)
  (trace nil :read-only t)
  (gate-count 0 :type fixnum :read-only t)
  (receipt nil :read-only t)
  (fingerprint "" :type string :read-only t))

(defun execution-result-state (result) (%execution-state result))
(defun execution-result-gate-count (result) (%execution-gate-count result))
(defun execution-result-parameter-occurrences (result)
  "Fresh occurrence records with stable depth-first gate indices and paths."
  (loop for step in (%execution-trace result)
        when (%gate-step-parameter-root step)
          collect (list :index (%gate-step-index step)
                        :path (copy-tree (%gate-step-path step))
                        :opcode (%gate-step-opcode step)
                        :parameter (%gate-step-parameter-root step))))
(defun execution-result-bindings (result) (copy-tree (%execution-bindings result)))
(defun execution-result-receipt (result) (copy-tree (%execution-receipt result)))
(defun execution-result-fingerprint (result) (copy-seq (%execution-fingerprint result)))
(defun execution-result-tape-fingerprint (result)
  (rosette-quantum-ir:quantum-tape-fingerprint (%execution-tape result)))

(defun %proper-list-p (object)
  ;; Floyd's tortoise-and-hare walk distinguishes a finite dotted tail from a
  ;; cycle without allocating or recursively printing the admitted object.
  ;; Signal here on a cycle: callers otherwise tend to put the rejected value
  ;; in obstruction details, forcing the diagnostic projection to traverse it.
  (loop with slow = object
        with fast = object
        do (cond
             ((null fast) (return t))
             ((atom fast) (return nil))
             ((null (cdr fast)) (return t))
             ((atom (cdr fast)) (return nil)))
           (setf slow (cdr slow)
                 fast (cddr fast))
           (when (eq slow fast)
             (%obstruct :circular-input
                        :details '(:input-shape :circular-list)))))

(defun %binding-entry-value (entry)
  ;; Proper two-element rows and dotted pairs remain valid.  A circular row
  ;; must be stopped before the malformed-entry wall can retain it in DETAILS.
  (%proper-list-p entry)
  (cond
    ((and (consp (cdr entry)) (null (cddr entry))) (second entry))
    ((atom (cdr entry)) (cdr entry))
    (t (%obstruct :malformed-binding :details (list :entry entry)))))

(defun %normalize-bindings (tape bindings)
  (unless (%proper-list-p bindings)
    (%obstruct :malformed-binding :details (list :bindings bindings)))
  (let ((seen (make-hash-table :test #'eq))
        (supplied (make-hash-table :test #'eq))
        (parameters (rosette-quantum-ir:quantum-tape-parameters tape)))
    (dolist (entry bindings)
      (unless (and (consp entry) (symbolp (car entry)) (car entry))
        (when (consp entry)
          (%proper-list-p entry))
        (%obstruct :malformed-binding :details (list :entry entry)))
      (when (gethash (car entry) seen)
        (%obstruct :malformed-binding
                   :details (list :duplicate (car entry))))
      (setf (gethash (car entry) seen) t
            (gethash (car entry) supplied) (%binding-entry-value entry)))
    (maphash
     (lambda (name ignored)
       (declare (ignore ignored))
       (unless (assoc name parameters :test #'eq)
         (%obstruct :malformed-binding :details (list :unknown name))))
     seen)
    (loop for (name type) in parameters collect
      (progn
        (unless (eq type :real)
          (%obstruct :unsupported-input
                     :details (list :parameter name :type type)))
        (multiple-value-bind (value present-p) (gethash name supplied)
          (unless present-p
            (%obstruct :malformed-binding :details (list :missing name)))
          (unless (or (realp value) (%hd-p value))
            (%obstruct :malformed-binding
                       :details (list :parameter name :value value)))
          (cons name (if (%hd-p value) value (%coerce-real value))))))))

(defun %canonical-bindings (bindings)
  (loop for (name . value) in bindings
        collect (list name (%scalar-content value))))

(defun %prototype-for-bindings (bindings)
  (or (loop for entry in bindings
            for value = (cdr entry)
            when (%hd-p value) return value)
      0d0))

(defun %environment-for-parameters (parameters arguments)
  (unless (= (length parameters) (length arguments))
    (%obstruct :malformed-call
               :details (list :expected (length parameters)
                              :actual (length arguments))))
  (let ((environment (make-hash-table :test #'eq)))
    (loop for (name type) in parameters
          for value in arguments do
      (unless (or (and (eq type :real) (%bound-real-p value))
                  (and (consp type) (eq (first type) :qstate)
                       (quantum-state-p value)
                       (= (second type) (%state-n-qubits value))))
        (%obstruct :malformed-call
                   :details (list :formal name :type type :value value)))
      (setf (gethash name environment) value))
    environment))

(defun %env-get (environment name path)
  (multiple-value-bind (value present-p) (gethash name environment)
    (unless present-p
      (%obstruct :malformed-tape :path path :details (list :undefined name)))
    value))

(defun %consume-input (environment name path)
  (let ((value (%env-get environment name path)))
    (when (quantum-state-p value) (remhash name environment))
    value))

(defun %resolve-real (environment object path)
  (cond
    ((realp object) (values (%coerce-real object) nil))
    ((symbolp object)
     (let ((binding (%env-get environment object path)))
       (unless (%bound-real-p binding)
         (%obstruct :malformed-tape :path path
                    :details (list :expected :real :got binding)))
       (values (%bound-real-value binding) (%bound-real-root binding))))
    (t (%obstruct :malformed-tape :path path
                  :details (list :rotation-angle object)))))

(defun %subroutine (tape name path)
  (or (find name (rosette-quantum-ir:quantum-tape-subroutines tape)
            :key (lambda (record) (getf record :name)) :test #'eq)
      (%obstruct :malformed-call :path path :details (list :unknown name))))

(defun %resolve-call-argument (environment input type path)
  (cond
    ((eq type :real)
     (cond
       ((realp input) (%make-bound-real (%coerce-real input) nil))
       ((symbolp input)
        (let ((value (%env-get environment input path)))
          (unless (%bound-real-p value)
            (%obstruct :malformed-call :path path
                       :details (list :argument input :expected :real)))
          value))
       (t (%obstruct :malformed-call :path path
                     :details (list :argument input :expected :real)))))
    ((and (consp type) (eq (first type) :qstate))
     (%consume-input environment input path))
    (t (%obstruct :unsupported-input :path path
                  :details (list :argument input :type type)))))

(defun %gate-path (context operation)
  (list :calls (reverse (copy-list (%context-call-stack context)))
        :operation (rosette-quantum-ir:quantum-tape-op-path operation)))

(defun %execute-gate (context environment operation)
  (let* ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
         (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (outputs (getf operands :outputs))
         (inputs (getf operands :inputs))
         (parameters (getf operands :parameters))
         (targets (getf operands :targets))
         (path (%gate-path context operation))
         (index (%context-gate-index context))
         (before (%consume-input environment (first inputs) path))
         angle root matrix after)
    (unless (quantum-state-p before)
      (%obstruct :malformed-tape :path path
                 :details (list :expected :qstate :got before)))
    (cond
      ((member opcode '(:rx :ry :rz))
       (unless (= (length parameters) 1)
         (%obstruct :malformed-tape :path path
                    :details (list :parameters parameters)))
       (multiple-value-setq (angle root)
         (%resolve-real environment (first parameters) path))
       (when (eql index (%context-shift-index context))
         (setf angle (%s+ angle (%context-shift-delta context))))
       (unless (= (length targets) 1)
         (%obstruct :malformed-tape :path path :details (list :targets targets)))
       (setf matrix (%gate-matrix opcode angle)
             after (%apply-one-matrix before matrix (first targets))))
      ((member opcode '(:h :x :y :z :s :t))
       (unless (= (length targets) 1)
         (%obstruct :malformed-tape :path path :details (list :targets targets)))
       (setf after (%apply-one-matrix before (%gate-matrix opcode)
                                      (first targets))))
      ((member opcode '(:cnot :cz :swap))
       (unless (= (length targets) 2)
         (%obstruct :malformed-tape :path path :details (list :targets targets)))
       (setf after (%apply-two-gate before opcode
                                    (first targets) (second targets))))
      (t (%obstruct :unsupported-opcode :path path
                    :details (list :opcode opcode))))
    (unless (= (length outputs) 1)
      (%obstruct :malformed-tape :path path :details (list :outputs outputs)))
    (setf (gethash (first outputs) environment) after)
    (push (%make-gate-step index opcode (copy-list targets) angle root
                           before after path)
          (%context-trace context))
    (incf (%context-gate-index context))))

(defun %execute-call (context environment operation)
  (let* ((operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (outputs (getf operands :outputs))
         (input-names (getf operands :inputs))
         (name (first (getf operands :parameters)))
         (path (%gate-path context operation))
         (subroutine (%subroutine (%context-tape context) name path))
         (formals (getf subroutine :parameters)))
    (unless (= (length input-names) (length formals))
      (%obstruct :malformed-call :path path
                 :details (list :expected (length formals)
                                :actual (length input-names))))
    (when (member name (%context-call-stack context) :test #'eq)
      (%obstruct :recursive-call :path path :details (list :circuit name)))
    (let* ((arguments
             (loop for input in input-names
                   for formal in formals
                   for type = (second formal)
                   collect (%resolve-call-argument environment input type path)))
           (local (%environment-for-parameters formals arguments))
           (old-stack (%context-call-stack context)))
      (unwind-protect
           (progn
             (setf (%context-call-stack context) (cons name old-stack))
             (multiple-value-bind (returned-p values)
                 (%execute-operations context local (getf subroutine :operations))
               (unless returned-p
                 (%obstruct :malformed-call :path path
                            :details (list :missing-return name)))
               (unless (= (length outputs) (length values))
                 (%obstruct :malformed-call :path path
                            :details (list :outputs outputs :values values)))
               (loop for output in outputs for value in values do
                 (setf (gethash output environment) value))))
        (setf (%context-call-stack context) old-stack)))))

(defun %execute-operation (context environment operation)
  (let ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
        (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
        (path (%gate-path context operation)))
    (case opcode
      (:prepare
       (let ((outputs (getf operands :outputs))
             (parameters (getf operands :parameters)))
         (unless (and (= (length outputs) 1) (= (length parameters) 2))
           (%obstruct :malformed-tape :path path :details operands))
         (setf (gethash (first outputs) environment)
               (%basis-state (first parameters) (second parameters)
                             (%context-prototype context))))
       (values nil nil))
      ((:h :x :y :z :s :t :rx :ry :rz :cnot :cz :swap)
       (%execute-gate context environment operation)
       (values nil nil))
      (:call
       (%execute-call context environment operation)
       (values nil nil))
      (:return
       (values t (loop for input in (getf operands :inputs)
                       collect (%consume-input environment input path))))
      ((:measure :branch :channel :reset :discard)
       (%obstruct :effect-boundary :path path :details (list :opcode opcode)))
      (otherwise
       (%obstruct :unsupported-opcode :path path :details (list :opcode opcode))))))

(defun %execute-operations (context environment operations)
  (dolist (operation operations (values nil nil))
    (multiple-value-bind (returned-p values)
        (%execute-operation context environment operation)
      (when returned-p (return (values t values))))))

(defun %trace-content (trace)
  (loop for step in trace collect
    (list :index (%gate-step-index step)
          :opcode (%gate-step-opcode step)
          :targets (copy-list (%gate-step-targets step))
          :parameter (%gate-step-parameter-root step)
          :angle (and (%gate-step-angle step)
                      (%scalar-content (%gate-step-angle step)))
          :path (copy-tree (%gate-step-path step)))))

(defun %build-execution-result (state tape bindings trace gate-count)
  (let* ((receipt
           (list :version 1
                 :tape (rosette-quantum-ir:quantum-tape-fingerprint tape)
                 :entry (rosette-quantum-ir:quantum-tape-entry tape)
                 :bindings (%canonical-bindings bindings)
                 :gates (%trace-content trace)
                 :gate-count gate-count
                 :state (%state-content state)))
         (fingerprint (%stable-fingerprint receipt)))
    (%make-execution-result state tape (copy-list bindings) trace gate-count
                            receipt fingerprint)))

(defun %execute-quantum-tape (tape bindings &key shift-index shift-delta)
  (unless (rosette-quantum-ir:quantum-tape-p tape)
    (%obstruct :malformed-tape :details (list :tape tape)))
  (let ((result-types (rosette-quantum-ir:quantum-tape-result-types tape)))
    (unless (and (= (length result-types) 1)
                 (consp (first result-types))
                 (eq (first (first result-types)) :qstate))
      (%obstruct :unsupported-result :details (list :types result-types))))
  (let* ((normalized (%normalize-bindings tape bindings))
         (arguments (loop for (name . value) in normalized
                          collect (%make-bound-real value name)))
         (environment
           (%environment-for-parameters
            (rosette-quantum-ir:quantum-tape-parameters tape) arguments))
         (context (%make-vm-context :tape tape
                                    :prototype (%prototype-for-bindings normalized)
                                    :shift-index shift-index
                                    :shift-delta shift-delta)))
    (multiple-value-bind (returned-p values)
        (%execute-operations context environment
                             (rosette-quantum-ir:quantum-tape-operations tape))
      (unless returned-p
        (%obstruct :malformed-tape :details '(:missing-return t)))
      (unless (and (= (length values) 1) (quantum-state-p (first values)))
        (%obstruct :unsupported-result :details (list :values values)))
      (let ((trace (nreverse (%context-trace context))))
        (%build-execution-result (first values) tape normalized trace
                                 (%context-gate-index context))))))

(defun execute-quantum-tape (tape bindings)
  "Execute immutable TAPE at top-level real BINDINGS.

BINDINGS is an alist.  PREPARE, all fixed gates, RX/RY/RZ, CALL, and RETURN
are exact.  Measurement, branches, channels, reset, and discard signal a
QUANTUM-VM-OBSTRUCTION rather than being sampled or approximated."
  (%execute-quantum-tape tape bindings))
