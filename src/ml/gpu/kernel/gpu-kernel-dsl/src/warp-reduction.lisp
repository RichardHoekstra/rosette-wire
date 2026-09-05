;;;; warp-reduction.lisp --- named, replayable full-warp sum-tree forms.

(in-package #:rosette-gpu-kernel-dsl)

(defun %warp-power-of-two-p (value)
  (and (plusp value) (zerop (logand value (1- value)))))

(defun %warp-sum-tree-offsets (width)
  (loop for offset = (ash width -1) then (ash offset -1)
        while (plusp offset)
        collect offset))

(defun make-warp-sum-tree (&key (width 32) (owner 0) (carrier :float))
  "Return the canonical identity of a full-width SHFL-DOWN sum tree.

The identity is ordinary immutable-by-contract data suitable for a
KERNEL-SPEC schedule receipt.  This constructor currently admits power-of-two
widths through one physical warp, owner lane zero, and f32/i32 word carriers.
It names floating-point association explicitly; it does not claim equality to
a serial sum."
  (unless (and (integerp width) (<= 2 width 32)
               (%warp-power-of-two-p width))
    (error 'dsl-syntax-error :form width
           :reason "warp sum width must be a power of two in [2,32]"))
  (unless (eql owner 0)
    (error 'dsl-syntax-error :form owner
           :reason "SHFL-DOWN sum trees currently expose owner lane zero only"))
  (unless (member carrier '(:float :int) :test #'eq)
    (error 'dsl-syntax-error :form carrier
           :reason "warp sum carrier must be :FLOAT or :INT"))
  (list :schema :rosette-warp-sum-tree/v1
        :width width
        :offsets (%warp-sum-tree-offsets width)
        :owner owner
        :carrier carrier))

(defun %validated-warp-sum-tree (tree)
  (unless (and (listp tree)
               (eq (getf tree :schema) :rosette-warp-sum-tree/v1))
    (error 'dsl-syntax-error :form tree
           :reason "not an :ROSETTE-WARP-SUM-TREE/V1 identity"))
  (let ((canonical
          (make-warp-sum-tree
           :width (getf tree :width)
           :owner (getf tree :owner)
           :carrier (getf tree :carrier))))
    (unless (equal tree canonical)
      (error 'dsl-syntax-error :form tree
             :reason "warp sum tree identity is non-canonical or has stale offsets"))
    canonical))

(defun warp-sum-tree-form (accumulator tree)
  "Compose the DSL statement that reduces ACCUMULATOR according to TREE.

Every participating lane must execute the returned full-mask shuffle sites in
the same dynamic order.  Only TREE's owner lane is the complete sum afterward;
other lanes are unspecified partials."
  (unless (and (symbolp accumulator) (not (keywordp accumulator)))
    (error 'dsl-syntax-error :form accumulator
           :reason "warp sum accumulator must be a non-keyword symbol"))
  (let ((canonical (%validated-warp-sum-tree tree)))
    `(progn
       ,@(loop for offset in (getf canonical :offsets)
               collect `(setq ,accumulator
                              (+ ,accumulator
                                 (shfl-down ,accumulator ,offset)))))))

(defun make-block-segmented-sum-tree
    (&key (warps 8) (lanes 32) (slots 1) (values 1)
          (owner-warp 0) (carrier :float))
  "Name a fixed-order column reduction across the warps of one block.

LANE and SLOT identify independent output segments; WARP is the fan-in axis.
VALUES permits several parallel sums (for example dK and dV) to share the same
two barriers.  The identity records its exact static shared footprint and does
not claim determinism beyond one block."
  (unless (and (integerp warps) (%warp-power-of-two-p warps)
               (<= 2 warps 32))
    (error 'dsl-syntax-error :form warps
           :reason "block segmented warps must be a power of two in [2,32]"))
  (unless (eql lanes 32)
    (error 'dsl-syntax-error :form lanes
           :reason "block segmented sums currently require 32 physical lanes"))
  (unless (and (integerp slots) (plusp slots))
    (error 'dsl-syntax-error :form slots
           :reason "block segmented slots must be positive"))
  (unless (and (integerp values) (plusp values))
    (error 'dsl-syntax-error :form values
           :reason "block segmented values must be positive"))
  (unless (and (integerp owner-warp) (<= 0 owner-warp) (< owner-warp warps))
    (error 'dsl-syntax-error :form owner-warp
           :reason "block segmented owner warp is outside the block"))
  (unless (member carrier '(:float :int) :test #'eq)
    (error 'dsl-syntax-error :form carrier
           :reason "block segmented carrier must be :FLOAT or :INT"))
  (let ((elements (* warps lanes slots values)))
    (list :schema :rosette-block-segmented-sum-tree/v1
          :warps warps :lanes lanes :slots slots :values values
          :owner-warp owner-warp :carrier carrier
          :association :ascending-warp-order
          :required-block-width (* warps lanes)
          :shared-elements elements
          :shared-bytes (* elements 4)
          :barriers 2)))

(defun %validated-block-segmented-sum-tree (tree)
  (unless (and (listp tree)
               (eq (getf tree :schema) :rosette-block-segmented-sum-tree/v1))
    (error 'dsl-syntax-error :form tree
           :reason "not an :ROSETTE-BLOCK-SEGMENTED-SUM-TREE/V1 identity"))
  (let ((canonical
          (make-block-segmented-sum-tree
           :warps (getf tree :warps) :lanes (getf tree :lanes)
           :slots (getf tree :slots) :values (getf tree :values)
           :owner-warp (getf tree :owner-warp)
           :carrier (getf tree :carrier))))
    (unless (equal tree canonical)
      (error 'dsl-syntax-error :form tree
             :reason "block segmented sum identity is non-canonical"))
    canonical))

(defun block-segmented-sum-tree-declarations (tree scratch-symbols)
  "Return TREE's top-level static shared-array declarations."
  (let ((canonical (%validated-block-segmented-sum-tree tree)))
    (unless (and (listp scratch-symbols)
                 (= (length scratch-symbols) (getf canonical :values))
                 (every (lambda (name)
                          (and (symbolp name) (not (keywordp name))))
                        scratch-symbols))
      (error 'dsl-syntax-error :form scratch-symbols
             :reason "one non-keyword scratch symbol is required per value"))
    (let ((per-value (* (getf canonical :warps)
                        (getf canonical :lanes)
                        (getf canonical :slots))))
      (loop for scratch in scratch-symbols
            collect `(shared-array ,scratch ,(getf canonical :carrier)
                                   ,per-value)))))

(defun block-segmented-sum-tree-form
    (tree warp lane bindings owner-forms)
  "Compose one shared column reduction and its owner-only consumer.

BINDINGS contains one `(SCRATCH CONTRIBUTIONS RESULTS)` entry per parallel
value.  CONTRIBUTIONS and RESULTS each contain TREE's SLOT count expressions
and lexical result symbols respectively.  Shared declarations are emitted
separately because they must live at kernel scope.  Every block thread must
execute this form in identical dynamic order."
  (let* ((canonical (%validated-block-segmented-sum-tree tree))
         (warps (getf canonical :warps))
         (lanes (getf canonical :lanes))
         (slots (getf canonical :slots))
         (values (getf canonical :values))
         (zero (if (eq (getf canonical :carrier) :float) 0.0 0)))
    (unless (and (symbolp warp) (not (keywordp warp))
                 (symbolp lane) (not (keywordp lane)))
      (error 'dsl-syntax-error :form (list warp lane)
             :reason "warp and lane must be non-keyword symbols"))
    (unless (and (listp bindings) (= (length bindings) values)
                 (every
                  (lambda (entry)
                    (and (listp entry) (= (length entry) 3)
                         (symbolp (first entry))
                         (listp (second entry)) (= (length (second entry)) slots)
                         (listp (third entry)) (= (length (third entry)) slots)
                         (every (lambda (result)
                                  (and (symbolp result) (not (keywordp result))))
                                (third entry))))
                  bindings))
      (error 'dsl-syntax-error :form bindings
             :reason "segmented bindings do not match TREE values/slots"))
    (labels ((index (warp-expr slot)
               `(+ (* ,warp-expr ,(* lanes slots))
                   (+ ,(* slot lanes) ,lane)))
             (scratch-ref (scratch warp-expr slot)
               (intern (format nil "~A[~S]" (symbol-name scratch)
                               (index warp-expr slot))
                       (symbol-package scratch))))
      `(progn
         ,@(loop for (scratch contributions results) in bindings
                 append
                 (loop for contribution in contributions
                       for slot from 0
                       collect `(setq ,(scratch-ref scratch warp slot)
                                      ,contribution)))
         (sync)
         (if (= ,warp ,(getf canonical :owner-warp))
             (let (,@(loop for entry in bindings
                           append (loop for result in (third entry)
                                        collect `(,result ,zero))))
               (for-range (source-warp ,warps)
                 ,@(loop for (scratch contributions results) in bindings
                         append
                         (loop for result in results
                               for slot from 0
                               collect
                               `(setq ,result
                                      (+ ,result
                                         ,(scratch-ref scratch
                                                       'source-warp slot))))))
               ,@owner-forms))
         (sync)))))
