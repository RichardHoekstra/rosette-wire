;;;; specialize.lisp --- state-free partial evaluation back to qIR ProgramIR.

(in-package #:rosette-quantum-vm)

(defstruct (%specialization-context
            (:constructor %make-specialization-context (tape shifts))
            (:conc-name %specialization-))
  tape shifts
  (seen-shifts (make-hash-table :test #'eql) :type hash-table)
  (gate-index 0 :type fixnum)
  (state-index 0 :type fixnum)
  (statements nil :type list)
  (occurrences nil :type list)
  (call-stack nil :type list))

(defun %normalize-occurrence-shifts (occurrence-shifts)
  (unless (%proper-list-p occurrence-shifts)
    (%obstruct :malformed-occurrence-shift
               :details (list :shifts occurrence-shifts)))
  (let ((table (make-hash-table :test #'eql)))
    (dolist (entry occurrence-shifts table)
      (unless (consp entry)
        (%obstruct :malformed-occurrence-shift :details (list :entry entry)))
      (let ((index (car entry)) (delta (%binding-entry-value entry)))
        (unless (and (integerp index) (not (minusp index)) (realp delta))
          (%obstruct :malformed-occurrence-shift
                     :details (list :entry entry)))
        (multiple-value-bind (ignored present-p) (gethash index table)
          (declare (ignore ignored))
          (when present-p
            (%obstruct :malformed-occurrence-shift
                       :details (list :duplicate index))))
        (setf (gethash index table) (%coerce-real delta))))))

(defun %fresh-specialized-state (context)
  (prog1 (make-symbol (format nil "QVM-Q~D"
                              (%specialization-state-index context)))
    (incf (%specialization-state-index context))))

(defun %specialization-path (context operation)
  (list :calls (reverse (copy-list (%specialization-call-stack context)))
        :operation (rosette-quantum-ir:quantum-tape-op-path operation)))

(defun %specialization-state-input (environment name path)
  (let ((value (%consume-input environment name path)))
    (unless (and (symbolp value) (not (%bound-real-p value)))
      (%obstruct :unsupported-shape :path path
                 :details (list :input name :value value)))
    value))

(defun %specialization-call-argument (environment input type path)
  (cond
    ((eq type :real)
     (cond
       ((realp input) (%make-bound-real (%coerce-real input) nil))
       ((symbolp input)
        (let ((value (%env-get environment input path)))
          (unless (%bound-real-p value)
            (%obstruct :unsupported-shape :path path
                       :details (list :argument input :type type)))
          value))
       (t (%obstruct :unsupported-shape :path path
                     :details (list :argument input :type type)))))
    ((and (consp type) (eq (first type) :qstate))
     (%specialization-state-input environment input path))
    (t (%obstruct :unsupported-shape :path path
                  :details (list :argument input :type type)))))

(defun %specialization-environment-for-parameters (parameters arguments path)
  (unless (= (length parameters) (length arguments))
    (%obstruct :unsupported-shape :path path
               :details (list :parameters parameters :arguments arguments)))
  (let ((environment (make-hash-table :test #'eq)))
    (loop for (name type) in parameters
          for value in arguments do
      (unless (or (and (eq type :real) (%bound-real-p value))
                  (and (consp type) (eq (first type) :qstate)
                       (symbolp value) (not (%bound-real-p value))))
        (%obstruct :unsupported-shape :path path
                   :details (list :formal name :type type :value value)))
      (setf (gethash name environment) value))
    environment))

(defun %emit-specialized-gate (context environment operation)
  (let* ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
         (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (outputs (getf operands :outputs))
         (inputs (getf operands :inputs))
         (parameters (getf operands :parameters))
         (targets (getf operands :targets))
         (path (%specialization-path context operation))
         (index (%specialization-gate-index context))
         (input (%specialization-state-input environment (first inputs) path))
         (output (%fresh-specialized-state context))
         gate angle root)
    (unless (and (= (length inputs) 1) (= (length outputs) 1))
      (%obstruct :unsupported-shape :path path :details operands))
    (if (member opcode '(:rx :ry :rz))
        (progn
          (unless (= (length parameters) 1)
            (%obstruct :unsupported-shape :path path :details operands))
          (multiple-value-setq (angle root)
            (%resolve-real environment (first parameters) path))
          (setf angle (%s-value angle))
          (multiple-value-bind (delta shifted-p)
              (gethash index (%specialization-shifts context))
            (when shifted-p
              (unless root
                (%obstruct :malformed-occurrence-shift :path path
                           :details (list :non-parameter-index index)))
              (setf angle (+ angle delta)
                    (gethash index (%specialization-seen-shifts context)) t)))
          (setf gate (list opcode angle))
          (when root
            (push (list :index index
                        :path (copy-tree path)
                        :opcode opcode
                        :parameter root
                        :angle angle
                        :targets (copy-list targets))
                  (%specialization-occurrences context))))
        (setf gate opcode))
    (push (append (list :apply output input gate) (copy-list targets))
          (%specialization-statements context))
    (setf (gethash (first outputs) environment) output)
    (incf (%specialization-gate-index context))))

(declaim (ftype function %specialize-operations))

(defun %specialize-call (context environment operation)
  (let* ((operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (outputs (getf operands :outputs))
         (inputs (getf operands :inputs))
         (name (first (getf operands :parameters)))
         (path (%specialization-path context operation))
         (subroutine (%subroutine (%specialization-tape context) name path))
         (formals (getf subroutine :parameters)))
    (unless (= (length inputs) (length formals))
      (%obstruct :unsupported-shape :path path
                 :details (list :circuit name :inputs inputs
                                :formals formals)))
    (when (member name (%specialization-call-stack context) :test #'eq)
      (%obstruct :recursive-call :path path :details (list :circuit name)))
    (let* ((arguments
             (loop for input in inputs
                   for formal in formals
                   collect (%specialization-call-argument
                            environment input (second formal) path)))
           (local (%specialization-environment-for-parameters
                   formals arguments path))
           (old-stack (%specialization-call-stack context)))
      (unwind-protect
           (progn
             (setf (%specialization-call-stack context) (cons name old-stack))
             (multiple-value-bind (returned-p values)
                 (%specialize-operations context local
                                         (getf subroutine :operations))
               (unless returned-p
                 (%obstruct :unsupported-shape :path path
                            :details (list :missing-return name)))
               (unless (= (length outputs) (length values))
                 (%obstruct :unsupported-shape :path path
                            :details (list :outputs outputs :values values)))
               (loop for output in outputs for value in values do
                 (setf (gethash output environment) value))))
        (setf (%specialization-call-stack context) old-stack)))))

(defun %specialize-operation (context environment operation)
  (let ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
        (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
        (path (%specialization-path context operation)))
    (case opcode
      (:prepare
       (let ((outputs (getf operands :outputs))
             (parameters (getf operands :parameters))
             (output (%fresh-specialized-state context)))
         (unless (and (= (length outputs) 1) (= (length parameters) 2))
           (%obstruct :unsupported-shape :path path :details operands))
         ;; Crucially, emit PREPARE syntax; never call %BASIS-STATE here.
         (push (list :prepare output (first parameters) (second parameters))
               (%specialization-statements context))
         (setf (gethash (first outputs) environment) output))
       (values nil nil))
      ((:h :x :y :z :s :t :rx :ry :rz :cnot :cz :swap)
       (%emit-specialized-gate context environment operation)
       (values nil nil))
      (:call
       (%specialize-call context environment operation)
       (values nil nil))
      (:return
       (values t
               (loop for input in (getf operands :inputs)
                     collect (%consume-input environment input path))))
      ((:measure :branch :channel :reset :discard)
       (%obstruct :effect-boundary :path path :details (list :opcode opcode)))
      (otherwise
       (%obstruct :unsupported-opcode :path path :details (list :opcode opcode))))))

(defun %specialize-operations (context environment operations)
  (dolist (operation operations (values nil nil))
    (multiple-value-bind (returned-p values)
        (%specialize-operation context environment operation)
      (when returned-p (return (values t values))))))

(defun %unseen-occurrence-shifts (context)
  (let ((unseen nil))
    (maphash
     (lambda (index delta)
       (declare (ignore delta))
       (unless (gethash index (%specialization-seen-shifts context))
         (push index unseen)))
     (%specialization-shifts context))
    (sort unseen #'<)))

(defun %report-content (report)
  (loop for diagnostic in (rosette-quantum-ir:program-report-errors report)
        collect (list :kind (rosette-quantum-ir:diagnostic-kind diagnostic)
                      :path (rosette-quantum-ir:diagnostic-path diagnostic)
                      :message (rosette-quantum-ir:diagnostic-message diagnostic))))

;;; -------------------------------------------------------------------------
;;; Effect-preserving specialization
;;;
;;; The ordinary specializer below deliberately projects a unitary tape to one
;;; call-free state path.  Effectful programs need the same binding and stable
;;; occurrence machinery, but their MEASURE/BRANCH structure must remain in the
;;; ProgramIR denotation.  This walker consumes the existing immutable tape,
;;; inlines calls that have ordinary returns, and emits the admitted effect
;;; syntax without allocating a state or selecting a branch.

(defun %copy-specialization-environment (environment)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (name value) (setf (gethash name copy) value))
             environment)
    copy))

(defun %effect-specialization-value (environment object path)
  (cond ((realp object) (%make-bound-real (%coerce-real object) nil))
        ((symbolp object) (%env-get environment object path))
        (t (%obstruct :unsupported-shape :path path
                      :details (list :value object)))))

(defun %effect-specialization-symbol (environment object path)
  (let ((value (%effect-specialization-value environment object path)))
    (unless (and (symbolp value) (not (%bound-real-p value)))
      (%obstruct :unsupported-shape :path path
                 :details (list :expected :ssa-value :got value)))
    value))

(defun %effect-call-argument (environment input type path)
  (if (eq type :real)
      (multiple-value-bind (value root) (%resolve-real environment input path)
        (%make-bound-real value root))
      (%effect-specialization-symbol environment input path)))

(defun %effect-call-environment (parameters arguments path)
  (unless (= (length parameters) (length arguments))
    (%obstruct :unsupported-shape :path path
               :details (list :parameters parameters :arguments arguments)))
  (let ((environment (make-hash-table :test #'eq)))
    (loop for (name type) in parameters
          for value in arguments do
      (unless (if (eq type :real)
                  (%bound-real-p value)
                  (and (symbolp value) (not (%bound-real-p value))))
        (%obstruct :unsupported-shape :path path
                   :details (list :formal name :type type :value value)))
      (setf (gethash name environment) value))
    environment))

(defun %effect-specialized-gate
    (context environment operation input output)
  (let* ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
         (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (parameters (getf operands :parameters))
         (targets (getf operands :targets))
         (path (%specialization-path context operation))
         (index (%specialization-gate-index context))
         gate angle root)
    (if (member opcode '(:rx :ry :rz))
        (progn
          (unless (= (length parameters) 1)
            (%obstruct :unsupported-shape :path path :details operands))
          (multiple-value-setq (angle root)
            (%resolve-real environment (first parameters) path))
          (setf angle (%s-value angle))
          (multiple-value-bind (delta shifted-p)
              (gethash index (%specialization-shifts context))
            (when shifted-p
              (unless root
                (%obstruct :malformed-occurrence-shift :path path
                           :details (list :non-parameter-index index)))
              (setf angle (+ angle delta)
                    (gethash index (%specialization-seen-shifts context)) t)))
          (setf gate (list opcode angle))
          (when root
            (push (list :index index
                        :path (copy-tree path)
                        :opcode opcode
                        :parameter root
                        :angle angle
                        :targets (copy-list targets))
                  (%specialization-occurrences context))))
        (setf gate opcode))
    (incf (%specialization-gate-index context))
    (append (list :apply output input gate) (copy-list targets))))

(declaim (ftype function %specialize-effect-operations))

(defun %specialize-effect-call (context environment operation)
  (let* ((operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (outputs (getf operands :outputs))
         (inputs (getf operands :inputs))
         (name (first (getf operands :parameters)))
         (path (%specialization-path context operation))
         (subroutine (%subroutine (%specialization-tape context) name path))
         (formals (getf subroutine :parameters)))
    (unless (= (length inputs) (length formals))
      (%obstruct :unsupported-shape :path path
                 :details (list :circuit name :inputs inputs :formals formals)))
    (when (member name (%specialization-call-stack context) :test #'eq)
      (%obstruct :recursive-call :path path :details (list :circuit name)))
    (let* ((arguments
             (loop for input in inputs
                   for formal in formals
                   collect (%effect-call-argument
                            environment input (second formal) path)))
           (local (%effect-call-environment formals arguments path))
           (old-stack (%specialization-call-stack context)))
      (unwind-protect
           (progn
             (setf (%specialization-call-stack context) (cons name old-stack))
             (multiple-value-bind (statements returned-p values)
                 (%specialize-effect-operations
                  context local (getf subroutine :operations) nil)
               (unless returned-p
                 (%obstruct :unsupported-shape :path path
                            :details (list :missing-return name)))
               (unless (= (length outputs) (length values))
                 (%obstruct :unsupported-shape :path path
                            :details (list :outputs outputs :values values)))
               (loop for output in outputs for value in values do
                 (setf (gethash output environment) value))
               statements))
        (setf (%specialization-call-stack context) old-stack)))))

(defun %specialize-effect-operation
    (context environment operation emit-return-p)
  "Return emitted statements, terminal-p, and returned values for OPERATION."
  (let* ((opcode (rosette-quantum-ir:quantum-tape-op-opcode operation))
         (operands (rosette-quantum-ir:quantum-tape-op-operands operation))
         (path (%specialization-path context operation)))
    (case opcode
      (:prepare
       (let* ((outputs (getf operands :outputs))
              (parameters (getf operands :parameters))
              (output (%fresh-specialized-state context)))
         (unless (and (= (length outputs) 1) (= (length parameters) 2))
           (%obstruct :unsupported-shape :path path :details operands))
         (setf (gethash (first outputs) environment) output)
         (values (list (list :prepare output
                             (first parameters) (second parameters)))
                 nil nil)))
      ((:h :x :y :z :s :t :rx :ry :rz :cnot :cz :swap)
       (let* ((inputs (getf operands :inputs))
              (outputs (getf operands :outputs)))
         (unless (and (= (length inputs) 1) (= (length outputs) 1))
           (%obstruct :unsupported-shape :path path :details operands))
         (let ((input (%effect-specialization-symbol
                       environment (first inputs) path))
               (output (%fresh-specialized-state context)))
           (setf (gethash (first outputs) environment) output)
           (values (list (%effect-specialized-gate
                          context environment operation input output))
                   nil nil))))
      (:measure
       (let ((inputs (getf operands :inputs))
             (outputs (getf operands :outputs))
             (targets (getf operands :targets)))
         (unless (and (= (length inputs) 1) (= (length outputs) 2)
                      (= (length targets) 1))
           (%obstruct :unsupported-shape :path path :details operands))
         (let ((input (%effect-specialization-symbol
                       environment (first inputs) path))
               (bit-output (%fresh-specialized-state context))
               (state-output (%fresh-specialized-state context)))
           (setf (gethash (first outputs) environment) bit-output
                 (gethash (second outputs) environment) state-output)
           (values (list (list :measure bit-output state-output input
                               (first targets)))
                   nil nil))))
      ((:channel :reset)
       (let ((inputs (getf operands :inputs))
             (outputs (getf operands :outputs))
             (targets (getf operands :targets)))
         (unless (and (= (length inputs) 1) (= (length outputs) 1))
           (%obstruct :unsupported-shape :path path :details operands))
         (let ((input (%effect-specialization-symbol
                       environment (first inputs) path))
               (output (%fresh-specialized-state context)))
           (setf (gethash (first outputs) environment) output)
           (values
            (list
             (if (eq opcode :channel)
                 (append (list :channel output input
                               (first (getf operands :parameters)))
                         (copy-list targets))
                 (append (list :reset output input) (copy-list targets))))
            nil nil))))
      (:discard
       (let ((inputs (getf operands :inputs))
             (outputs (getf operands :outputs))
             (targets (getf operands :targets)))
         (unless (= (length inputs) 1)
           (%obstruct :unsupported-shape :path path :details operands))
         (let ((input (%effect-specialization-symbol
                       environment (first inputs) path)))
           (if (null outputs)
               (values (list (list :discard input)) nil nil)
               (let ((output (%fresh-specialized-state context)))
                 (unless (= (length outputs) 1)
                   (%obstruct :unsupported-shape :path path :details operands))
                 (setf (gethash (first outputs) environment) output)
                 (values (list (append (list :discard output input)
                                       (copy-list targets)))
                         nil nil))))))
      (:call
       (values (%specialize-effect-call context environment operation) nil nil))
      (:branch
       (unless emit-return-p
         (%obstruct :effectful-call-branch :path path
                    :details '(:reason :continuation-duplication-required)))
       (let ((inputs (getf operands :inputs)))
         (unless (= (length inputs) 1)
           (%obstruct :unsupported-shape :path path :details operands))
         (let ((bit (%effect-specialization-symbol
                     environment (first inputs) path))
               (arms nil))
           (dolist (block (rosette-quantum-ir:quantum-tape-op-blocks operation))
             (multiple-value-bind (statements returned-p values)
                 (%specialize-effect-operations
                  context (%copy-specialization-environment environment)
                  (second block) t)
               (declare (ignore values))
               (unless returned-p
                 (%obstruct :unsupported-shape :path path
                            :details (list :branch (first block)
                                           :missing-return t)))
               (push (cons (first block) statements) arms)))
           (values (list (cons :case (cons bit (nreverse arms)))) t nil))))
      (:return
       (let ((values
               (loop for input in (getf operands :inputs)
                     collect (%effect-specialization-value
                              environment input path))))
         (when (some #'%bound-real-p values)
           (%obstruct :unsupported-shape :path path
                      :details (list :real-return values)))
         (values (if emit-return-p (list (cons :return values)) nil)
                 t values)))
      (otherwise
       (%obstruct :unsupported-opcode :path path :details (list :opcode opcode))))))

(defun %specialize-effect-operations
    (context environment operations emit-return-p)
  (let ((statements nil))
    (dolist (operation operations (values (nreverse statements) nil nil))
      (multiple-value-bind (emitted terminal-p values)
          (%specialize-effect-operation
           context environment operation emit-return-p)
        (dolist (statement emitted) (push statement statements))
        (when terminal-p
          (return (values (nreverse statements) t values)))))))

(defun %specialize-effectful-tape (tape bindings occurrence-shifts)
  (let* ((normalized
           (loop for (name . value) in (%normalize-bindings tape bindings)
                 collect (cons name (%s-value value))))
         (arguments
           (loop for (name . value) in normalized
                 collect (%make-bound-real value name)))
         (environment
           (%effect-call-environment
            (rosette-quantum-ir:quantum-tape-parameters tape) arguments nil))
         (context
           (%make-specialization-context
            tape (%normalize-occurrence-shifts occurrence-shifts))))
    (multiple-value-bind (statements returned-p values)
        (%specialize-effect-operations
         context environment (rosette-quantum-ir:quantum-tape-operations tape) t)
      (declare (ignore values))
      (unless returned-p
        (%obstruct :unsupported-shape :details '(:missing-return t)))
      (let ((unseen (%unseen-occurrence-shifts context)))
        (when unseen
          (%obstruct :malformed-occurrence-shift
                     :details (list :non-rotation-indices unseen))))
      (let* ((entry
               (make-symbol
                (format nil "~A-SPECIALIZED"
                        (rosette-quantum-ir:quantum-tape-entry tape))))
             (declaration
               (append (list :program entry nil
                             (rosette-quantum-ir:quantum-tape-result-types tape))
                       statements))
             (program
               (rosette-quantum-ir:make-quantum-program
                (list :quantum-unit declaration)))
             (report (rosette-quantum-ir:check-program program)))
        (unless (rosette-quantum-ir:program-report-ok-p report)
          (%obstruct :specialization-invalid
                     :details (%report-content report)))
        (values program
                (copy-tree (nreverse
                            (%specialization-occurrences context))))))))

(defun specialize-quantum-tape
    (tape bindings &key occurrence-shifts (effects :reject))
  "Partially evaluate TAPE into a parameter-free qIR ProgramIR.

The pass inlines CALL depth-first, substitutes numeric rotations, and returns
(values PROGRAM OCCURRENCES).  It never allocates an amplitude vector.
OCCURRENCE-SHIFTS is an alist from stable gate index to numeric delta.

EFFECTS is :REJECT (the original pure-unitary contract) or :PRESERVE.  The
latter retains measurement, branch, channel, reset, and discard syntax and is
still state-free; it never samples an outcome or evaluates a Kraus operator."
  (unless (rosette-quantum-ir:quantum-tape-p tape)
    (%obstruct :malformed-tape :details (list :tape tape)))
  (unless (member effects '(:reject :preserve))
    (%obstruct :unsupported-effect-policy :details (list :effects effects)))
  (when (eq effects :preserve)
    (return-from specialize-quantum-tape
      (%specialize-effectful-tape tape bindings occurrence-shifts)))
  (let ((result-types (rosette-quantum-ir:quantum-tape-result-types tape)))
    (unless (and (= (length result-types) 1)
                 (consp (first result-types))
                 (eq (first (first result-types)) :qstate))
      (%obstruct :unsupported-shape :details (list :results result-types))))
  (let* ((normalized
           (loop for (name . value) in (%normalize-bindings tape bindings)
                 collect (cons name (%s-value value))))
         (arguments
           (loop for (name . value) in normalized
                 collect (%make-bound-real value name)))
         (environment
           (%specialization-environment-for-parameters
            (rosette-quantum-ir:quantum-tape-parameters tape) arguments nil))
         (context
           (%make-specialization-context
            tape (%normalize-occurrence-shifts occurrence-shifts))))
    (multiple-value-bind (returned-p values)
        (%specialize-operations context environment
                                (rosette-quantum-ir:quantum-tape-operations tape))
      (unless (and returned-p (= (length values) 1)
                   (symbolp (first values))
                   (not (%bound-real-p (first values))))
        (%obstruct :unsupported-shape :details (list :return values)))
      (let ((unseen (%unseen-occurrence-shifts context)))
        (when unseen
          (%obstruct :malformed-occurrence-shift
                     :details (list :non-rotation-indices unseen))))
      (push (list :return (first values)) (%specialization-statements context))
      (let* ((entry
               (make-symbol
                (format nil "~A-SPECIALIZED"
                        (rosette-quantum-ir:quantum-tape-entry tape))))
             (declaration
               (append (list :program entry nil
                             (rosette-quantum-ir:quantum-tape-result-types tape))
                       (nreverse (%specialization-statements context))))
             (program
               (rosette-quantum-ir:make-quantum-program
                (list :quantum-unit declaration)))
             (report (rosette-quantum-ir:check-program program)))
        (unless (rosette-quantum-ir:program-report-ok-p report)
          (%obstruct :specialization-invalid
                     :details (%report-content report)))
        (values program
                (copy-tree (nreverse (%specialization-occurrences context))))))))
