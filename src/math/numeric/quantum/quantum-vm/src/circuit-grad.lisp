;;;; circuit-grad.lisp --- validated data-only circuit response filter.
;;;;
;;;; This file is an adapter over the admitted qIR tape and the VM's existing
;;;; differentiation routes.  It deliberately accepts only strings, numbers,
;;;; and proper lists; callers cannot supply Lisp or qIR forms to execute.

(in-package #:rosette-quantum-vm)

(defparameter +circuit-grad-schema+ "rosette-circuit-grad/v1")

;; Dense execution retains the state history needed by the adjoint.  Bound the
;; logical number of amplitudes retained before allocating any state.  This is
;; an access wall, not a claim about a precise byte count (hyper-dual scalars
;; are larger than double-float scalars).
(defparameter +circuit-grad-max-qubits+ 20)
(defparameter +circuit-grad-max-gates+ 4096)
(defparameter +circuit-grad-max-state-history-elements+ (ash 1 22))
(defparameter +circuit-grad-max-parameters+ 1024)
(defparameter +circuit-grad-max-observable-terms+ 4096)

(defun %cg-fail (kind &rest details)
  (%obstruct kind :details details))

(defun %cg-finite-double (value label)
  (unless (realp value)
    (%cg-fail :malformed-circuit-request label value :expected :finite-real))
  (let ((number
          (handler-case (coerce value 'double-float)
            (error ()
              (%cg-fail :malformed-circuit-request
                        label value :expected :representable-double)))))
    (unless (and (= number number)
                 (<= (abs number) most-positive-double-float))
      (%cg-fail :malformed-circuit-request label value :expected :finite-real))
    number))

(defun %cg-list-of-length-p (value expected)
  (and (%proper-list-p value) (= (length value) expected)))

(defun %cg-identifier-character-p (character)
  (or (alpha-char-p character)
      (digit-char-p character)
      (member character '(#\- #\_) :test #'char=)))

(defun %cg-normalize-name (name)
  (unless (and (stringp name)
               (<= 1 (length name) 64)
               (alpha-char-p (char name 0))
               (every #'%cg-identifier-character-p name))
    (%cg-fail :malformed-circuit-request
              :parameter-name name
              :expected "[A-Za-z][A-Za-z0-9_-]{0,63}"))
  (string-downcase name))

(defun %cg-decode-parameters (parameters)
  (unless (%proper-list-p parameters)
    (%cg-fail :malformed-circuit-request :parameters parameters
              :expected :proper-list))
  (when (> (length parameters) +circuit-grad-max-parameters+)
    (%cg-fail :resource-budget :parameters (length parameters)
              :maximum +circuit-grad-max-parameters+))
  (let ((seen (make-hash-table :test #'equal))
        (records nil))
    (dolist (entry parameters (nreverse records))
      (unless (%cg-list-of-length-p entry 2)
        (%cg-fail :malformed-circuit-request :parameter entry
                  :expected '(name value)))
      (let* ((name (%cg-normalize-name (first entry)))
             (value (%cg-finite-double (second entry)
                                       (list :parameter name))))
        (when (gethash name seen)
          (%cg-fail :malformed-circuit-request :duplicate-parameter name))
        (setf (gethash name seen) t)
        (push (list :name name
                    :symbol (make-symbol (string-upcase name))
                    :value value)
              records)))))

(defun %cg-parameter-record (name records)
  (let ((normalized (%cg-normalize-name name)))
    (or (find normalized records :key (lambda (record) (getf record :name))
                                 :test #'string=)
        (%cg-fail :malformed-circuit-request
                  :unknown-parameter normalized))))

(defun %cg-target (value n-qubits gate)
  (unless (and (integerp value) (<= 0 value) (< value n-qubits))
    (%cg-fail :malformed-circuit-request
              :gate gate :target value :n-qubits n-qubits))
  value)

(defun %cg-angle (value parameter-records gate)
  (cond
    ((realp value) (%cg-finite-double value (list :gate gate :angle)))
    ((stringp value)
     (getf (%cg-parameter-record value parameter-records) :symbol))
    (t
     (%cg-fail :malformed-circuit-request
               :gate gate :angle value :expected :finite-real-or-parameter-name))))

(defun %cg-state-symbol (index)
  (make-symbol (format nil "Q~D" index)))

(defun %cg-decode-gates (gates n-qubits parameter-records)
  (unless (%proper-list-p gates)
    (%cg-fail :malformed-circuit-request :gates gates :expected :proper-list))
  (let ((gate-count (length gates)))
    (when (> gate-count +circuit-grad-max-gates+)
      (%cg-fail :resource-budget :gates gate-count
                :maximum +circuit-grad-max-gates+))
    (let ((history-elements (* (1+ gate-count) (ash 1 n-qubits))))
      (when (> history-elements +circuit-grad-max-state-history-elements+)
        (%cg-fail :resource-budget
                  :state-history-elements history-elements
                  :maximum +circuit-grad-max-state-history-elements+
                  :qubits n-qubits :gates gate-count)))
    (let ((statements nil)
          (parameter-occurrences 0)
          (states (loop for index from 0 to gate-count
                        collect (%cg-state-symbol index))))
      (loop for gate in gates
            for index from 0
            for input = (nth index states)
            for output = (nth (1+ index) states)
            do
               (unless (and (%proper-list-p gate) gate (stringp (first gate)))
                 (%cg-fail :malformed-circuit-request :gate gate
                           :expected :positional-gate-list))
               (let ((name (string-downcase (first gate))))
                 (cond
                   ((member name '("h" "x" "y" "z" "s" "t")
                            :test #'string=)
                    (unless (= (length gate) 2)
                      (%cg-fail :malformed-circuit-request :gate gate
                                :expected (list name "target")))
                    (push (list :apply output input
                                (intern (string-upcase name) :keyword)
                                (%cg-target (second gate) n-qubits gate))
                          statements))
                   ((member name '("rx" "ry" "rz") :test #'string=)
                    (unless (= (length gate) 3)
                      (%cg-fail :malformed-circuit-request :gate gate
                                :expected (list name "angle" "target")))
                    (let ((angle (%cg-angle (second gate) parameter-records gate)))
                      (when (symbolp angle) (incf parameter-occurrences))
                      (push (list :apply output input
                                  (list (intern (string-upcase name) :keyword)
                                        angle)
                                  (%cg-target (third gate) n-qubits gate))
                            statements)))
                   ((member name '("cnot" "cz" "swap") :test #'string=)
                    (unless (= (length gate) 3)
                      (%cg-fail :malformed-circuit-request :gate gate
                                :expected (list name "first" "second")))
                    (let ((first (%cg-target (second gate) n-qubits gate))
                          (second (%cg-target (third gate) n-qubits gate)))
                      (when (= first second)
                        (%cg-fail :malformed-circuit-request :gate gate
                                  :expected :distinct-targets))
                      (push (list :apply output input
                                  (intern (string-upcase name) :keyword)
                                  first second)
                            statements)))
                   (t
                    (%cg-fail :unsupported-opcode :gate gate
                              :supported '(h x y z s t rx ry rz cnot cz swap))))))
      (values (nreverse statements) gate-count parameter-occurrences
              (first states) (car (last states))))))

(defun %cg-decode-observable (terms n-qubits)
  (unless (and (%proper-list-p terms) terms)
    (%cg-fail :malformed-circuit-request :observable terms
              :expected :nonempty-proper-list))
  (when (> (length terms) +circuit-grad-max-observable-terms+)
    (%cg-fail :resource-budget :observable-terms (length terms)
              :maximum +circuit-grad-max-observable-terms+))
  (make-pauli-sum
   (loop for term in terms collect
     (progn
       (unless (%cg-list-of-length-p term 2)
         (%cg-fail :malformed-circuit-request :observable-term term
                   :expected '(coefficient "IXYZ")))
       (let ((coefficient (%cg-finite-double
                           (first term) (list :observable-term term)))
             (word (second term)))
         (unless (and (stringp word)
                      (= (length word) n-qubits)
                      (every (lambda (character)
                               (find character "IXYZ" :test #'char-equal))
                             word))
           (%cg-fail :malformed-circuit-request
                     :observable-word word :expected-width n-qubits
                     :alphabet "IXYZ"))
         (list coefficient (string-upcase word)))))))

(defun %cg-method (method)
  (unless (stringp method)
    (%cg-fail :malformed-circuit-request :method method
              :expected '("adjoint" "parameter-shift" "hyper-dual")))
  (cond ((string-equal method "adjoint")
         (values :adjoint "adjoint"))
        ((string-equal method "parameter-shift")
         (values :parameter-shift "parameter-shift"))
        ((string-equal method "hyper-dual")
         (values :hyper-dual "hyper-dual"))
        (t
         (%cg-fail :unsupported-method :method method
                   :supported '("adjoint" "parameter-shift" "hyper-dual")))))

(defun %cg-program-and-bindings
    (n-qubits basis-index parameter-records statements initial-state final-state)
  (let* ((program-name (make-symbol "CIRCUIT-GRAD"))
         (declarations
           (loop for record in parameter-records
                 collect (list (getf record :symbol) :real)))
         (body
           (append (list (list :prepare initial-state n-qubits basis-index))
                   statements
                   (list (list :return final-state))))
         (source
           (list :quantum-unit
                 (append (list :program program-name declarations
                               (list (list :qstate n-qubits)))
                         body)))
         (bindings
           (loop for record in parameter-records
                 collect (cons (getf record :symbol) (getf record :value)))))
    (values (rosette-quantum-ir:lower-quantum-tape
             (rosette-quantum-ir:make-quantum-program source))
            bindings)))

(defun circuit-value-gradient
    (n-qubits basis-index parameters gates observable
     &optional (method "adjoint"))
  "Return a validated dense pure-state observable value and ordered gradient.

All inputs are positional data.  PARAMETERS is ((NAME VALUE) ...).  GATES uses
(NAME ...) rows over H/X/Y/Z/S/T, RX/RY/RZ, CNOT/CZ/SWAP; a rotation angle is a
finite real literal or parameter-name string.  OBSERVABLE is ((COEFF \"IXYZ\")
...). METHOD is \"adjoint\", \"parameter-shift\", or \"hyper-dual\".

The result contains only ordinary data plus deterministic replay and semantic
fingerprints.  The resource budget is explicit because this VM retains dense
state history; exceeding it is a :RESOURCE-BUDGET obstruction, never an
implicit approximation."
  (unless (and (integerp n-qubits)
               (<= 1 n-qubits +circuit-grad-max-qubits+))
    (%cg-fail :resource-budget :n-qubits n-qubits
              :supported (list 1 +circuit-grad-max-qubits+)))
  (unless (and (integerp basis-index)
               (<= 0 basis-index) (< basis-index (ash 1 n-qubits)))
    (%cg-fail :malformed-circuit-request :basis-index basis-index
              :n-qubits n-qubits))
  (let* ((parameter-records (%cg-decode-parameters parameters))
         (decoded-observable (%cg-decode-observable observable n-qubits)))
    (multiple-value-bind
        (statements gate-count parameter-occurrences initial-state final-state)
        (%cg-decode-gates gates n-qubits parameter-records)
      (multiple-value-bind (method-key method-name) (%cg-method method)
        (multiple-value-bind (tape bindings)
            (%cg-program-and-bindings n-qubits basis-index parameter-records
                                      statements initial-state final-state)
          (let ((result (value-and-gradient tape decoded-observable bindings
                                            :method method-key)))
            (unless (verify-gradient-result result)
              (%cg-fail :verification-failed
                        :result (gradient-result-fingerprint result)))
            (list
             :schema +circuit-grad-schema+
             :method method-name
             :scope (list :state "dense-pure"
                          :effects "unitary-only"
                          :qubits n-qubits
                          :basis-index basis-index
                          :gates gate-count
                          :parameter-occurrences parameter-occurrences)
             :value (gradient-result-value result)
             :parameters (loop for record in parameter-records
                               collect (getf record :name))
             :gradient
             (loop with gradient = (gradient-result-gradient result)
                   for record in parameter-records
                   for symbol = (getf record :symbol)
                   collect (list (getf record :name)
                                 (cdr (assoc symbol gradient :test #'eq))))
             :certificate
             (list :kind "deterministic-replay"
                   :replay-valid t
                   :tape-fingerprint
                   (rosette-quantum-ir:quantum-tape-fingerprint tape)
                   :observable-fingerprint
                   (pauli-sum-fingerprint decoded-observable)
                   :result-fingerprint
                   (gradient-result-fingerprint result)))))))))

(defun %cg-required-field (request key)
  (let ((marker (list :missing)))
    (let ((value (getf request key marker)))
      (when (eq value marker)
        (%cg-fail :malformed-circuit-request :missing-field key))
      value)))

(defun %cg-run-request (request)
  (unless (and (%proper-list-p request) (evenp (length request)))
    (%cg-fail :malformed-circuit-request :request request
              :expected :property-list))
  (let ((allowed '(:schema :n-qubits :basis-index :parameters :gates
                   :observable :method))
        (seen (make-hash-table :test #'eq)))
    (loop for tail on request by #'cddr
          for key = (first tail)
          do (unless (member key allowed :test #'eq)
               (%cg-fail :malformed-circuit-request :unknown-field key))
             (when (gethash key seen)
               (%cg-fail :malformed-circuit-request :duplicate-field key))
             (setf (gethash key seen) t)))
  (let ((schema (%cg-required-field request :schema)))
    (unless (equal schema +circuit-grad-schema+)
      (%cg-fail :malformed-circuit-request :schema schema
                :expected +circuit-grad-schema+)))
  (let* ((marker (list :missing))
         (method (getf request :method marker)))
    (circuit-value-gradient
     (%cg-required-field request :n-qubits)
     (%cg-required-field request :basis-index)
     (%cg-required-field request :parameters)
     (%cg-required-field request :gates)
     (%cg-required-field request :observable)
     (if (eq method marker) "adjoint" method))))

(defun circuit-grad-main ()
  "Read one rosette-circuit-grad/v1 request from stdin and print one response.
Reader evaluation is disabled, unknown/duplicate fields are refused, and a
second form is an error."
  (let ((*read-eval* nil))
    (let ((request (read *standard-input* nil :eof)))
      (when (eq request :eof)
        (%cg-fail :malformed-circuit-request :input :empty))
      (let ((trailing (read *standard-input* nil :eof)))
        (unless (eq trailing :eof)
          (%cg-fail :malformed-circuit-request :input :trailing-form)))
      (let ((answer (%cg-run-request request)))
        (prin1 answer *standard-output*)
        (terpri *standard-output*)
        (finish-output *standard-output*)
        answer))))
