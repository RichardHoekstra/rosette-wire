;;;; shrink.lisp --- ddmin as a substrate bug-shrinker (#7).
;;;;
;;;; THE SAME ENGINE, TWO ORACLES. Shipping (ddmin.lisp) points the Zeller
;;;; delta-debugger at a PARITY oracle -- "does this subset still compute the
;;;; right answer?" -- and the 1-minimal witness it returns is the minimal
;;;; SHIPPABLE set: the fewest forms that still reproduce the capability.
;;;;
;;;; Repoint the very same engine at a FAILING oracle -- "does this subset
;;;; still trigger the bug?" -- and the 1-minimal witness becomes the minimal
;;;; FAILING set: the smallest reproducer that still breaks. Reduction and
;;;; debugging are one operation under different oracles. The gauge choice is
;;;; which predicate you hand ddmin; the invariant is the 1-minimality it
;;;; guarantees. Nothing new is computed here -- this file only names and
;;;; frames the shipping ddmin for the bug-shrinking oracle.

(in-package #:rosette-ship)

;;; ---- shrink-to-property: ddmin, framed for reduction --------------------
(defun shrink-to-property (items predicate)
  "Return a 1-minimal sublist of ITEMS that still exhibits a PROPERTY, where
PREDICATE maps a sublist to a boolean reporting whether the property is
present (e.g. \"this subset still triggers the bug\").

A thin, intent-revealing alias of DDMIN: the shipping oracle asks \"still
correct?\"; here the oracle asks \"still failing?\". Same delta-debug engine,
same 1-minimality guarantee -- only the meaning of the oracle differs.

CONTRACT: PREDICATE MUST hold for the whole ITEMS list -- the full input is
assumed to reproduce the property. The returned witness is 1-minimal: removing
any single remaining element makes PREDICATE fail. If PREDICATE is not perfectly
monotone, ddmin still converges to a LOCALLY-minimal witness."
  (ddmin items predicate))

;;; ---- make-property-oracle: TEST-FN -> a ddmin predicate -----------------
(defun make-property-oracle (test-fn)
  "Wrap TEST-FN (a subset -> boolean saying whether the property/bug is
present) as a predicate suitable for SHRINK-TO-PROPERTY / DDMIN.

The wrapping is a trivial passthrough -- the contract is what matters:

  * TEST-FN receives a candidate sublist of the original ITEMS and returns
    generalised-boolean T iff the property is still present in that subset.
  * ddmin converges cleanly when the property is MONOTONE-ISH: if a subset
    exhibits it, supersets do too (removing elements can only lose the bug,
    never introduce it). This is the usual shape of a minimal reproducer.
  * If the property is NOT monotone, ddmin does not loop or error -- it simply
    returns a LOCALLY 1-minimal witness rather than a globally minimal one."
  (lambda (subset) (funcall test-fn subset)))

;;; ---- minimal-reproducer: the guarded convenience entrypoint -------------
(defun minimal-reproducer (items test-fn)
  "Shrink ITEMS to a 1-minimal subset that still satisfies TEST-FN, i.e. the
minimal reproducer of whatever property/bug TEST-FN detects.

Convenience over (SHRINK-TO-PROPERTY ITEMS (MAKE-PROPERTY-ORACLE TEST-FN)),
guarded so a caller learns immediately if there is nothing to shrink: TEST-FN
MUST hold for the full ITEMS list. If it does not, the full input does not
reproduce the property and no minimisation is meaningful -- this signals an
error rather than returning a misleading (possibly empty) witness."
  (let ((oracle (make-property-oracle test-fn)))
    (unless (funcall oracle items)
      (error "minimal-reproducer: TEST-FN does not hold for the full ITEMS ~
              list; nothing to shrink (the input does not reproduce the ~
              property)."))
    (shrink-to-property items oracle)))
