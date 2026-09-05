;;;; verify.lisp --- seL4/Microkit system referee: OS correctness as obstruction.
;;;;
;;;; A protected procedure call (ppcall) is SYNCHRONOUS, so a cycle in the
;;;; ppcall graph is a deadlock.  The system is deadlock-free iff a global
;;;; PROGRESS POTENTIAL exists (a topological order of the ppcall graph) -- which
;;;; is exactly Microkit's rule that a ppcall target must have strictly higher
;;;; priority than its caller (the priority vector IS the potential).
;;;;
;;;; Information-flow security is the same shape: a flow X->Y must not DROP the
;;;; security level (level(X) <= level(Y)); a leak is a level-dropping edge, the
;;;; exact analogue of a priority-rule violation.
;;;;
;;;; The EXACT invariant is the directed potential.  The undirected b_1 (from
;;;; rosette-chain-complex) is reported as a homological SHADOW only -- it
;;;; over-counts reconvergent DAGs (an undirected cycle that is not a directed
;;;; cycle, hence not a deadlock).

(in-package #:rosette-sel4-verify)

;;; ------------------------------------------------------------------ model ---
(defstruct (system (:constructor make-system))
  (pds '())        ; alist: name(keyword) -> plist (:priority N :level L)
  (edges '())      ; directed ppcall edges (caller . callee) as (a b) lists
  (levels '()))    ; alist: name -> security level (integer); optional

(defun add-pd (sys name &key (priority 0) (level nil))
  (push (cons name (list :priority priority :level level)) (system-pds sys))
  (when level (push (cons name level) (system-levels sys)))
  sys)
(defun add-ppcall (sys caller callee) (push (list caller callee) (system-edges sys)) sys)
(defun pd-priority (sys name) (getf (cdr (assoc name (system-pds sys))) :priority))
(defun %verts (sys) (mapcar #'car (system-pds sys)))

;;; ------------------------------------------------------- the invariants ----
(defun progress-potential (verts edges)
  "A topological order of the directed graph (the global progress potential), or
NIL if a cycle (deadlock) obstructs it."
  (let ((indeg (make-hash-table)) (order '()))
    (dolist (v verts) (setf (gethash v indeg) 0))
    (dolist (e edges) (incf (gethash (second e) indeg)))
    (let ((ready (loop for v in verts when (zerop (gethash v indeg)) collect v)))
      (loop while ready do
        (let ((v (pop ready))) (push v order)
          (dolist (e edges) (when (eq (first e) v)
            (when (zerop (decf (gethash (second e) indeg))) (push (second e) ready))))))
      (when (= (length order) (length verts)) (nreverse order)))))

(defun betti1 (verts edges)
  "Homological shadow b_1 of the underlying 1-complex (via rosette-chain-complex).
NOTE: exact for call-chains/trees; over-counts reconvergent DAGs -- use the
directed potential as the exact deadlock invariant."
  (if edges (second (rosette-chain-complex:betti-numbers (rosette-chain-complex:graph-complex verts edges))) 0))

(defun priority-monotone-p (sys)
  "Microkit's rule: every ppcall edge goes caller -> strictly-higher-priority callee."
  (every (lambda (e) (> (pd-priority sys (second e)) (pd-priority sys (first e))))
         (system-edges sys)))

(defun deadlock-free-p (sys)
  "Returns (values DEADLOCK-FREE-P ORDER B1)."
  (let* ((verts (%verts sys)) (edges (system-edges sys))
         (order (progress-potential verts edges)))
    (values (and order t) order (betti1 verts edges))))

(defun info-flow-secure-p (sys)
  "T if the security-level labelling never DROPS along a flow (no high->low leak).
If no levels are declared, returns T (not applicable)."
  (let ((lv (system-levels sys)))
    (or (null lv)
        (every (lambda (e)
                 (let ((s (cdr (assoc (first e) lv))) (d (cdr (assoc (second e) lv))))
                   (or (null s) (null d) (<= s d))))
               (system-edges sys)))))

;;; ---------------------------------------------------------- the verdict ----
(defstruct verdict deadlock-free info-flow-secure b1 order violations)

(defun referee (sys)
  "Certify a SYSTEM.  Returns a VERDICT with the deadlock + info-flow results and
a list of located violations (the obstructions)."
  (multiple-value-bind (df order b1) (deadlock-free-p sys)
    (let ((viol '()))
      (unless df (push (list :deadlock :no-progress-potential :b1 b1) viol))
      ;; locate priority-rule violations (back-edges)
      (dolist (e (system-edges sys))
        (when (<= (pd-priority sys (second e)) (pd-priority sys (first e)))
          (push (list :priority-violation (first e) '-> (second e)) viol)))
      ;; locate info-flow leaks
      (let ((lv (system-levels sys)))
        (when lv (dolist (e (system-edges sys))
          (let ((s (cdr (assoc (first e) lv))) (d (cdr (assoc (second e) lv))))
            (when (and s d (> s d)) (push (list :info-leak (first e) '-> (second e)) viol))))))
      (make-verdict :deadlock-free df :info-flow-secure (info-flow-secure-p sys)
                    :b1 b1 :order order :violations (nreverse viol)))))

(defun report (sys &optional (stream *standard-output*))
  (let ((v (referee sys)))
    (format stream "~&seL4 system referee:~%")
    (format stream "  PDs:           ~a~%" (mapcar (lambda (p) (cons (car p) (getf (cdr p) :priority))) (system-pds sys)))
    (format stream "  ppcall edges:  ~a~%" (or (system-edges sys) "(none -- async only)"))
    (format stream "  deadlock-free: ~a   (progress potential ~a)~%"
            (verdict-deadlock-free v) (if (verdict-order v) "exists" "OBSTRUCTED"))
    (format stream "  info-flow:     ~a~%" (if (verdict-info-flow-secure v) "secure" "LEAK"))
    (format stream "  b_1 (shadow):  ~d~%" (verdict-b1 v))
    (when (verdict-violations v)
      (format stream "  violations:~%")
      (dolist (x (verdict-violations v)) (format stream "    ~a~%" x)))
    (format stream "  VERDICT: ~a~%"
            (if (and (verdict-deadlock-free v) (verdict-info-flow-secure v)) "SAFE" "UNSAFE"))
    v))

;;; ------------------------------------------------ minimal .system parser ----
(defun %attr (s name &optional (from 0))
  (let ((p (search (concatenate 'string name "=\"") s :start2 from)))
    (when p (let ((start (+ p (length name) 2)))
              (subseq s start (position #\" s :start start))))))
(defun %kw (s) (intern (string-upcase s) :keyword))
(defun %starts (s tag) (loop with f = 0 for p = (search tag s :start2 f)
                             while p collect p do (setf f (1+ p))))

(defun parse-system-string (xml &key levels)
  "Parse a Microkit .system XML string into a SYSTEM.  ppcall edges are inferred
from channel ends carrying pp=\"true\" (caller = that end's PD).  LEVELS is an
optional alist PD-name -> security level for info-flow checking."
  (let ((sys (make-system)))
    (dolist (p (%starts xml "<protection_domain"))
      (let* ((e (position #\> xml :start p)) (tag (subseq xml p e))
             (name (%attr tag "name")) (pr (%attr tag "priority")))
        (when name (add-pd sys (%kw name)
                           :priority (if pr (parse-integer pr) 0)
                           :level (cdr (assoc (%kw name) levels))))))
    (loop with f = 0 for cs = (search "<channel" xml :start2 f) while cs do
      (let* ((ce (or (search "</channel>" xml :start2 cs) (length xml)))
             (block (subseq xml cs ce))
             (ends (loop for ep in (%starts block "<end") for ee = (position #\> block :start ep)
                         collect (let ((tg (subseq block ep ee)))
                                   (list (%kw (%attr tg "pd")) (%attr tg "pp"))))))
        (when (= 2 (length ends))
          (destructuring-bind ((pd1 pp1) (pd2 pp2)) ends
            (when (equal pp1 "true") (add-ppcall sys pd1 pd2))
            (when (equal pp2 "true") (add-ppcall sys pd2 pd1))))
        (setf f (+ cs 8))))
    sys))

(defun parse-system-file (path &key levels)
  (parse-system-string (uiop:read-file-string path) :levels levels))
