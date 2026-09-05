;;;; optimal-axiom.lisp --- DAG-CSE metric for axiom selection.
;;;;
;;;; Lean source: OptimalAxiom.lean.
;;;;
;;;; Question (OptimalAxiom.lean): given a tower of n candidate axioms
;;;; over the same lambda calculus, which single axiom minimises the
;;;; merged-DAG node count K(combined) when chosen as the head of a
;;;; CSE-shared derivation of the rest?
;;;;
;;;; Lean's heuristic: K(combined) is the same constant across
;;;; candidates (the merged DAG covers the same set of subterms).
;;;; Therefore the ranking reduces to: highest K(axiom) wins, because
;;;; that axiom 'covers' the most of the tower.
;;;;
;;;; Computational content (this file): we reuse the TLC-TERM struct
;;;; from church-rosser.lisp and compute DAG node counts via hash-cons
;;;; under EQUAL.

(in-package #:rosette-foundation-rewrite)

(defstruct (axiom-rank-entry
            (:constructor %make-axiom-rank-entry)
            (:copier nil))
  "One row of the OPTIMAL-AXIOM-RANK report.

  NAME        — symbol naming the candidate axiom
  K-AXIOM     — DAG node count of the axiom alone (= K(A))
  K-COMBINED  — DAG node count of the axiom merged with all others
  K-REST      — K-COMBINED − K-AXIOM"
  (name nil :read-only t)
  (k-axiom 0 :type (integer 0) :read-only t)
  (k-combined 0 :type (integer 0) :read-only t)
  (k-rest 0 :type (integer 0) :read-only t))

(defun term-key (term)
  "Hash-cons key for a TLC term: a tree of canonical lists.  EQUAL
suffices because the structure has no functions or arrays."
  (case (tlc-term-kind term)
    (:var (list :var (tlc-var-name term)))
    (:lam (list :lam
                (tlc-lam-param term)
                (term-key (tlc-lam-body term))))
    (:app (list :app
                (term-key (tlc-app-fn term))
                (term-key (tlc-app-arg term))))))

(defun %dag-collect (term table)
  "Internal helper: insert every subterm of TERM as a key in TABLE."
  (let ((k (term-key term)))
    (unless (gethash k table)
      (setf (gethash k table) t)
      (case (tlc-term-kind term)
        (:lam (%dag-collect (tlc-lam-body term) table))
        (:app (%dag-collect (tlc-app-fn term) table)
              (%dag-collect (tlc-app-arg term) table))
        (t nil))))
  table)

(defun dag-node-count (term)
  "Return the number of distinct sub-terms of TERM after CSE-merge.
This is K(term) in OptimalAxiom.lean's notation."
  (check-type term tlc-term)
  (hash-table-count (%dag-collect term (make-hash-table :test 'equal))))

(defun shared-dag-node-count (terms-list)
  "Return the number of distinct sub-terms across the merge of all
terms in TERMS-LIST — the K(combined) of OptimalAxiom.lean.

By construction this is monotone in the input: adding a term cannot
decrease the count, and adding a term whose subterms are already
present does not increase it."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (term terms-list)
      (%dag-collect term tbl))
    (hash-table-count tbl)))

(defun optimal-axiom-rank (candidates)
  "Rank a list of (NAME . TERM) candidate axioms by the OptimalAxiom
metric.

Returns a list of AXIOM-RANK-ENTRY records, sorted by descending
K-AXIOM (the highest K(A) wins, per the Lean heuristic).

K-COMBINED is computed once over the full term list and is therefore
the same value in every entry — exposed so callers can verify the
'constant K(combined)' invariant of the heuristic explicitly."
  (let* ((all-terms (mapcar #'cdr candidates))
         (k-combined (shared-dag-node-count all-terms))
         (entries
           (mapcar (lambda (pair)
                     (let* ((name (car pair))
                            (term (cdr pair))
                            (k-a (dag-node-count term)))
                       (%make-axiom-rank-entry
                        :name name
                        :k-axiom k-a
                        :k-combined k-combined
                        :k-rest (- k-combined k-a))))
                   candidates)))
    (sort entries #'> :key #'axiom-rank-entry-k-axiom)))
