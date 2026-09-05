;;;; tape.lisp --- deterministic semantic tape view over admitted ProgramIR.

(in-package #:rosette-quantum-ir)

(define-condition quantum-tape-compilation-obstruction (error)
  ((kind :initarg :kind :reader %quantum-tape-obstruction-kind)
   (diagnostics :initarg :diagnostics
                :initform nil
                :reader %quantum-tape-obstruction-diagnostics)
   (details :initarg :details
            :initform nil
            :reader %quantum-tape-obstruction-details))
  (:report
   (lambda (condition stream)
     (format stream "Quantum semantic-tape lowering refused (~S)~@[ — ~S~]"
             (%quantum-tape-obstruction-kind condition)
             (%quantum-tape-obstruction-details condition)))))

(defun quantum-tape-obstruction-kind (condition)
  (%quantum-tape-obstruction-kind condition))

(defun quantum-tape-obstruction-diagnostics (condition)
  (copy-list (%quantum-tape-obstruction-diagnostics condition)))

(defun quantum-tape-obstruction-details (condition)
  (copy-tree (%quantum-tape-obstruction-details condition)))

(defun %tape-obstruction (kind &key diagnostics details)
  (error 'quantum-tape-compilation-obstruction
         :kind kind :diagnostics diagnostics :details details))

;; The generated accessors are deliberately private.  Every public collection
;; reader below returns a fresh spine/tree, and every slot is structurally
;; read-only, so callers cannot mutate the compiled artifact through the API.
(defstruct (quantum-tape-op
            (:constructor %make-quantum-tape-op
                (&key opcode path operands blocks))
            (:conc-name %quantum-tape-op-)
            (:copier nil))
  (opcode nil :read-only t)
  (path nil :read-only t)
  (operands nil :read-only t)
  (blocks nil :read-only t))

(defstruct (quantum-tape
            (:constructor %make-quantum-tape
                (&key entry parameters result-types operations subroutines
                      signatures fingerprint))
            (:conc-name %quantum-tape-)
            (:copier nil))
  (entry nil :read-only t)
  (parameters nil :read-only t)
  (result-types nil :read-only t)
  (operations nil :read-only t)
  (subroutines nil :read-only t)
  (signatures nil :read-only t)
  (fingerprint "" :type string :read-only t))

(defun quantum-tape-op-opcode (operation)
  (%quantum-tape-op-opcode operation))

(defun quantum-tape-op-path (operation)
  (copy-tree (%quantum-tape-op-path operation)))

(defun quantum-tape-op-operands (operation)
  (copy-tree (%quantum-tape-op-operands operation)))

(defun quantum-tape-op-blocks (operation)
  (mapcar (lambda (block)
            (list (first block) (copy-list (second block))))
          (%quantum-tape-op-blocks operation)))

(defun quantum-tape-entry (tape)
  (%quantum-tape-entry tape))

(defun quantum-tape-parameters (tape)
  (copy-tree (%quantum-tape-parameters tape)))

(defun quantum-tape-result-types (tape)
  (copy-tree (%quantum-tape-result-types tape)))

(defun quantum-tape-operations (tape)
  (copy-list (%quantum-tape-operations tape)))

(defun quantum-tape-subroutines (tape)
  (mapcar (lambda (subroutine)
            (list :name (getf subroutine :name)
                  :parameters (copy-tree (getf subroutine :parameters))
                  :result-types (copy-tree (getf subroutine :result-types))
                  :operations (copy-list (getf subroutine :operations))))
          (%quantum-tape-subroutines tape)))

(defun quantum-tape-signatures (tape)
  (copy-tree (%quantum-tape-signatures tape)))

(defun quantum-tape-fingerprint (tape)
  (copy-seq (%quantum-tape-fingerprint tape)))

(defun %make-normalized-op (opcode path operands &optional blocks)
  (%make-quantum-tape-op :opcode opcode
                         :path (copy-tree path)
                         :operands (copy-tree operands)
                         :blocks blocks))

(declaim (ftype function %lower-tape-block))

(defun %lower-tape-statement (statement path)
  (case (first statement)
    (:prepare
     (%make-normalized-op
      :prepare path
      (list :outputs (list (second statement))
            :parameters (list (third statement) (fourth statement)))))
    (:apply
     (let* ((gate (fourth statement))
            (opcode (if (consp gate) (first gate) gate))
            (parameters (and (consp gate) (list (second gate)))))
       (%make-normalized-op
        opcode path
        (list :outputs (list (second statement))
              :inputs (list (third statement))
              :parameters parameters
              :targets (cddddr statement)))))
    (:measure
     (%make-normalized-op
      :measure path
      (list :outputs (list (second statement) (third statement))
            :inputs (list (fourth statement))
            :targets (list (fifth statement)))))
    (:copy
     (%make-normalized-op
      :copy path
      (list :outputs (list (second statement) (third statement))
            :inputs (list (fourth statement)))))
    (:case
     (let ((blocks
             (loop for label in '(:zero :one)
                   for branch = (find label (cddr statement) :key #'first)
                   collect
                   (list label
                         (%lower-tape-block
                          (rest branch) (append path (list label)))))))
       (%make-normalized-op
        :branch path (list :inputs (list (second statement))) blocks)))
    (:discard
     (if (= (length statement) 2)
         (%make-normalized-op
          :discard path (list :inputs (list (second statement))))
         (%make-normalized-op
          :discard path
          (list :outputs (list (second statement))
                :inputs (list (third statement))
                :targets (cdddr statement)))))
    (:channel
     (%make-normalized-op
      :channel path
      (list :outputs (list (second statement))
            :inputs (list (third statement))
            :parameters (list (fourth statement))
            :targets (cddddr statement))))
    (:reset
     (%make-normalized-op
      :reset path
      (list :outputs (list (second statement))
            :inputs (list (third statement))
            :targets (cdddr statement))))
    (:call
     (%make-normalized-op
      :call path
      (list :outputs (copy-list (second statement))
            :inputs (copy-list (fourth statement))
            :parameters (list (third statement)))))
    (:return
     (%make-normalized-op
      :return path (list :inputs (copy-list (rest statement)))))
    (otherwise
     ;; Admission ran first, so reaching this arm would mean the lowering and
     ;; checker grammars drifted.  Keep that an explicit typed obstruction.
     (%tape-obstruction :lowering-drift
                        :details (list :path path :statement statement)))))

(defun %lower-tape-block (statements path)
  (loop for statement in statements
        for index from 0
        collect (%lower-tape-statement
                 statement (append path (list :statement index)))))

(defun %operation-content (operation)
  (list :opcode (%quantum-tape-op-opcode operation)
        :path (%quantum-tape-op-path operation)
        :operands (%quantum-tape-op-operands operation)
        :blocks
        (loop for block in (%quantum-tape-op-blocks operation)
              collect (list (first block)
                            (mapcar #'%operation-content (second block))))))

(defun %stable-fingerprint (content)
  "Return a process-independent FNV-1a digest of readable semantic CONTENT."
  (let* ((text
           (with-standard-io-syntax
             (let ((*package* (find-package '#:rosette-quantum-ir))
                   (*print-circle* nil)
                   (*print-pretty* nil)
                   (*print-readably* t))
               (write-to-string content))))
         (hash #xcbf29ce484222325))
    (loop for character across text do
      (setf hash
            (logand #xffffffffffffffff
                    (* (logxor hash (char-code character))
                       #x100000001b3))))
    (format nil "fnv1a64-~16,'0X" hash)))

(defun %declaration-record (declaration index)
  (multiple-value-bind (kind name parameters result-types body)
      (%declaration-components declaration)
    (let ((path (list :declaration index)))
      (list :kind kind
            :name name
            :parameters (copy-tree parameters)
            :result-types (copy-tree result-types)
            :operations (%lower-tape-block body (append path '(:body)))))))

(defun %record-signature (record)
  (list :name (getf record :name)
        :kind (getf record :kind)
        :parameters (copy-tree (getf record :parameters))
        :result-types (copy-tree (getf record :result-types))))

(defun %subroutine-content (subroutine)
  (list :name (getf subroutine :name)
        :parameters (getf subroutine :parameters)
        :result-types (getf subroutine :result-types)
        :operations (mapcar #'%operation-content
                            (getf subroutine :operations))))

(defun lower-quantum-tape (program &key entry)
  "Lower admitted PROGRAM to a deterministic immutable semantic tape.

PROGRAM remains the sole source AST: this function checks the ProgramIR carrier,
then projects and normalizes it.  ENTRY must name a :PROGRAM declaration when a
unit has zero or multiple closed entries.  Calls stay as CALL operations and
CASE alternatives stay as nested BRANCH blocks; no inlining or sampling occurs.
Invalid source and entry ambiguity signal QUANTUM-TAPE-COMPILATION-OBSTRUCTION."
  (let ((report (check-program program)))
    (unless (program-report-ok-p report)
      (%tape-obstruction :invalid-source
                         :diagnostics (program-report-errors report)))
    (let* ((source-form (program-form program))
           (carrier
             (if (eq (first source-form) :qprogram)
                 (elaborate-legacy-qprogram program)
                 program))
           (form (program-form carrier))
           (records
             (loop for declaration in (rest form)
                   for index from 0
                   collect (%declaration-record declaration index)))
           (programs
             (remove-if-not (lambda (record)
                              (eq (getf record :kind) :program))
                            records))
           (selected
             (cond
               (entry
                (or (find entry programs :key (lambda (record)
                                                (getf record :name))
                                      :test #'eq)
                    (%tape-obstruction
                     :unknown-entry
                     :details (list :requested entry
                                    :available (mapcar (lambda (record)
                                                        (getf record :name))
                                                      programs)))))
               ((= (length programs) 1) (first programs))
               ((null programs)
                (%tape-obstruction :missing-entry
                                   :details '(:available nil)))
               (t
                (%tape-obstruction
                 :ambiguous-entry
                 :details (list :available (mapcar (lambda (record)
                                                     (getf record :name))
                                                   programs))))))
           (subroutines
             (loop for record in records
                   when (eq (getf record :kind) :circuit)
                     collect
                     (list :name (getf record :name)
                           :parameters (copy-tree (getf record :parameters))
                           :result-types (copy-tree (getf record :result-types))
                           :operations (copy-list (getf record :operations)))))
           (signatures (mapcar #'%record-signature records))
           (operations (copy-list (getf selected :operations)))
           (content
             (list :entry (getf selected :name)
                   :parameters (getf selected :parameters)
                   :result-types (getf selected :result-types)
                   :operations (mapcar #'%operation-content operations)
                   :subroutines (mapcar #'%subroutine-content subroutines)
                   :signatures signatures)))
      (%make-quantum-tape
       :entry (getf selected :name)
       :parameters (copy-tree (getf selected :parameters))
       :result-types (copy-tree (getf selected :result-types))
       :operations operations
       :subroutines subroutines
       :signatures signatures
       :fingerprint (%stable-fingerprint content)))))
