;;;; effects.lisp --- effect-word normalization and commute helpers.

(in-package #:rosette-work-scheduler)

(defun normalize-effect-word (word laws)
  "Normalize a symbolic effect WORD under small algebraic laws.

LAWS is a plist keyed by effect symbol.  Supported per-symbol properties are:
`:IDEMPOTENT T`, `:NILPOTENT N`, `:INVOLUTIVE T`, `:UNIPOTENT N`,
`:COMMUTATIVE T`, `:MONOTONE T`, `:IDENTITY T`, `:ABSORBING T`, and
`:ANTICOMMUTES-WITH (...)`, `:PROJECTOR T`, and `:ANNIHILATES (...)`.

The normalizer is intentionally local and deterministic.  Absorbing effects
collapse the whole word to the first absorber.  Otherwise it sorts maximal
regions whose symbols are declared commutative or anticommutative, tracking a
phase sign for anticommuting swaps, then compresses runs of equal symbols:
annihilating adjacent pairs vanish, identities vanish, projectors/idempotents
collapse to one copy, nilpotents vanish when the run reaches their index,
involutions cancel pairs, and unipotents are recorded as bounded residual
powers.  Monotone effects are not reordered; they are recorded in the trace as
order-preserving barriers.  The function returns three values: the normalized
word, a rewrite trace, and the phase sign."
  (multiple-value-bind (absorbed absorber absorber-index)
      (%absorbing-effect word laws)
    (if absorbed
        (values (list absorber)
                (list (list :law :absorbing
                            :symbol absorber
                            :index absorber-index
                            :replacement (list absorber)))
                1)
        (multiple-value-bind (ordered order-trace phase)
            (%normalize-commutative-regions word laws)
          (multiple-value-bind (reduced annihilation-trace)
              (%remove-annihilating-pairs ordered laws)
          (let ((result nil)
                (trace (append order-trace annihilation-trace)))
            (labels ((emit (symbol count start)
                       (multiple-value-bind (replacement event)
                           (%normalize-effect-run symbol count laws)
                         (setf result (append replacement result))
                         (when event
                           (push (append (list :symbol symbol
                                               :count count
                                               :start start
                                               :end (+ start count -1))
                                         event)
                                 trace)))))
              (loop with current = nil
                    with count = 0
                    with start = 0
                    for symbol in reduced
                    for index from 0
                    do (cond
                         ((and current (equal symbol current))
                          (incf count))
                         (current
                          (emit current count start)
                          (setf current symbol
                                count 1
                                start index))
                         (t
                          (setf current symbol
                                count 1
                                start index)))
                    finally (when current
                              (emit current count start))))
            (values (nreverse result) (nreverse trace) phase)))))))

(defun effect-word-certificate (word laws &key (name :effect-word-normalization)
                                           metadata)
  "Return a proof-witness certificate for effect-word normalization."
  (multiple-value-bind (normal trace phase)
      (normalize-effect-word word laws)
    (rosette-proof-witness:make-certificate
     :name name
     :kind :effect-word-normalization
     :claim :algebraic-effect-normal-form
     :payload (list :word word
                    :laws laws
                    :normal-form normal
                    :trace trace
                    :phase phase)
     :passed t
     :metadata metadata)))

(defun verify-effect-word-certificate (certificate)
  "Recompute an effect-word normalization certificate.

Returns two values: a boolean and a list of verifier reasons.  The verifier
does not trust the stored pass bit; it recomputes the normal form and trace
from the recorded word and laws."
  (check-type certificate rosette-proof-witness:certificate)
  (let* ((payload (rosette-proof-witness:certificate-payload certificate))
         (word (getf payload :word))
         (laws (getf payload :laws)))
    (multiple-value-bind (normal trace phase)
        (normalize-effect-word word laws)
      (let ((reasons nil))
        (unless (equal normal (getf payload :normal-form))
          (push (list :kind :effect-normal-form-mismatch
                      :expected (getf payload :normal-form)
                      :actual normal)
                reasons))
        (unless (equal trace (getf payload :trace))
          (push (list :kind :effect-trace-mismatch
                      :expected (getf payload :trace)
                      :actual trace)
                reasons))
        (unless (= phase (getf payload :phase 1))
          (push (list :kind :effect-phase-mismatch
                      :expected (getf payload :phase)
                      :actual phase)
                reasons))
        (%finish-reasons reasons)))))

(defun work-item-effect-word (item)
  "Return ITEM's algebraic effect word from metadata, if present.

The scheduler does not interpret domain objects directly.  Callers may attach
`:EFFECT-WORD` or `:EFFECTS` to WORK-ITEM-METADATA to expose the algebraic
action of a task as symbols that NORMALIZE-EFFECT-WORD understands."
  (or (getf (work-item-metadata item) :effect-word)
      (getf (work-item-metadata item) :effects)))

(defun normalize-work-item-effects (item laws)
  "Return a copy of ITEM with normalized algebraic effect metadata attached."
  (let ((word (work-item-effect-word item)))
    (unless word
      (return-from normalize-work-item-effects item))
    (multiple-value-bind (normal trace phase)
        (normalize-effect-word word laws)
      (%copy-work-item
       item
       :metadata (append (list :effect-normal-form normal
                               :effect-trace trace
                               :effect-phase phase)
                         (work-item-metadata item))))))

(defun algebraic-commute-p (a b laws)
  "Return T when A and B commute under their effect-word normal forms.

This is deliberately conservative: both items must provide effect words, the
normal form of A followed by B must equal B followed by A, and the phase must
match.  The result can then be used to suppress otherwise false write/write
or read/write conflicts."
  (let ((aw (work-item-effect-word a))
        (bw (work-item-effect-word b)))
    (and aw bw
         (multiple-value-bind (ab-form ab-trace ab-phase)
             (normalize-effect-word (append aw bw) laws)
           (declare (ignore ab-trace))
           (multiple-value-bind (ba-form ba-trace ba-phase)
               (normalize-effect-word (append bw aw) laws)
             (declare (ignore ba-trace))
             (and (= ab-phase ba-phase)
                  (equal ab-form ba-form)))))))

(defun apply-algebraic-commutes (items laws)
  "Return copies of ITEMS with commutes-with inferred from effect laws.

This is the bridge from algebra to cheap parallelism: domain libraries attach
effect words to work metadata, this pass proves pairwise commuting actions by
normalizing both orders, and the existing scheduler consumes only ordinary
COMMUTES-WITH certificates."
  (let ((commutes (%make-equal-table)))
    (dolist (item items)
      (setf (gethash (work-item-name item) commutes)
            (copy-list (work-item-commutes-with item))))
    (loop for rest on items
          for a = (first rest)
          do (dolist (b (rest rest))
               (when (and (work-conflicts a b)
                          (algebraic-commute-p a b laws))
                 (pushnew (work-item-name b)
                          (gethash (work-item-name a) commutes)
                          :test #'equal)
                 (pushnew (work-item-name a)
                          (gethash (work-item-name b) commutes)
                          :test #'equal))))
    (mapcar (lambda (item)
              (%copy-work-item
               (normalize-work-item-effects item laws)
               :commutes-with (gethash (work-item-name item) commutes)))
            items)))

(defun explain-parallel-plan (plan)
  "Return a compact plist suitable for logs or proof witnesses."
  (list :profitable-p (parallel-plan-profitable-p plan)
        :speedup (parallel-plan-speedup plan)
        :serial-cost (parallel-plan-serial-cost plan)
        :parallel-cost (parallel-plan-parallel-cost plan)
        :batches (mapcar (lambda (batch)
                           (mapcar #'work-item-name batch))
                         (parallel-plan-batches plan))
        :reasons (parallel-plan-reasons plan)))

