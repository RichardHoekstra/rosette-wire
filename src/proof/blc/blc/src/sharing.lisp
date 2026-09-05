;;;; rosette-blc/src/sharing.lisp --- Structural sharing for BLC.
;;;;
;;;; Pure tree-BLC over-counts: a subterm used k times costs k x its bits,
;;;; which is NOT its true description length.  Real code (and the substrate
;;;; dependency graph) is a DAG.  Two sharing forms, both cheap:
;;;;
;;;;   1. CSE / let-factoring (no new opcode).  Factor a repeated CLOSED
;;;;      subterm M into a let, (:app (:lam N) M), so each use of M becomes
;;;;      a de Bruijn variable rather than a copy.  Pure BLC already encodes
;;;;      this -- blc-length-shared just CSE-factors first.  The factored
;;;;      term is structurally reversible to the original (no beta needed),
;;;;      because a standalone-closed subterm lifts to the top unchanged.
;;;;
;;;;   2. A genuine minimal DAG / term-graph form (blc-dag-*).  Every
;;;;      distinct non-trivial subterm is emitted ONCE; repeats are a short
;;;;      back-reference into a dictionary.  The encoding is CANONICAL --
;;;;      structurally identical terms produce identical bits -- which is
;;;;      exactly what makes it a dedup / overlap detector: sharing a
;;;;      subterm IS the assertion "these are the same object."
;;;;
;;;; Both keep the pure-tree blc-length untouched: tree = naive upper bound,
;;;; shared = the DAG-honest count.

(in-package #:rosette-blc)

;;; ------------------------------------------------------------------
;;; de Bruijn helpers.
;;; ------------------------------------------------------------------

(defun closed-p (term &optional (depth 0))
  "T iff TERM has no free de Bruijn indices (binds every variable it uses).
A standalone-closed subterm has the same meaning in every context, so it is
exactly the subterm class that can be safely factored / shared."
  (cond
    ((term-var-p term) (< (term-index term) depth))
    ((term-lam-p term) (closed-p (term-body term) (1+ depth)))
    ((term-app-p term) (and (closed-p (term-fun term) depth)
                            (closed-p (term-arg term) depth)))
    (t nil)))

(defun node-size (term)
  "Number of nodes in TERM (its tree size)."
  (cond
    ((term-var-p term) 1)
    ((term-lam-p term) (1+ (node-size (term-body term))))
    ((term-app-p term) (+ 1 (node-size (term-fun term))
                          (node-size (term-arg term))))
    (t 0)))

(defun count-subterm (sub term)
  "How many node positions of TERM are structurally equal to SUB."
  (let ((here (if (equal sub term) 1 0)))
    (cond
      ((term-lam-p term) (+ here (count-subterm sub (term-body term))))
      ((term-app-p term) (+ here
                            (count-subterm sub (term-fun term))
                            (count-subterm sub (term-arg term))))
      (t here))))

(defun %subterm-counts (term)
  "An EQUAL hash-table mapping every distinct subterm of TERM to its
occurrence count."
  (let ((tbl (make-hash-table :test 'equal)))
    (labels ((walk (node)
               (incf (gethash node tbl 0))
               (cond
                 ((term-lam-p node) (walk (term-body node)))
                 ((term-app-p node)
                  (walk (term-fun node)) (walk (term-arg node))))))
      (walk term))
    tbl))

;;; ------------------------------------------------------------------
;;; 1. CSE / let-factoring.
;;; ------------------------------------------------------------------

(defun %replace-with-letvar (term m depth)
  "Replace every occurrence of the closed subterm M in TERM with a reference
to a new outermost binder; DEPTH tracks the binders crossed (the index of
that binder seen from here).  Genuine variables (all < DEPTH in a closed
term) are untouched -- only the fresh let-var (index DEPTH) is introduced."
  (cond
    ((equal term m) (tvar depth))
    ((term-lam-p term) (tlam (%replace-with-letvar (term-body term) m (1+ depth))))
    ((term-app-p term) (tapp (%replace-with-letvar (term-fun term) m depth)
                             (%replace-with-letvar (term-arg term) m depth)))
    (t term)))

(defun %restore-top (term)
  "Structural inverse of one factoring step: given (:app (:lam N) M), put M
back at every let-var occurrence, recovering the pre-factor term."
  (let* ((lam (term-fun term))
         (body (term-body lam))
         (m (term-arg term)))
    (labels ((restore (node depth)
               (cond
                 ((term-var-p node)
                  (let ((n (term-index node)))
                    (cond ((= n depth) m)
                          ((> n depth) (tvar (1- n)))
                          (t node))))
                 ((term-lam-p node) (tlam (restore (term-body node) (1+ depth))))
                 ((term-app-p node) (tapp (restore (term-fun node) depth)
                                          (restore (term-arg node) depth)))
                 (t node))))
      (restore body 0))))

(defun %factor-once (term)
  "Pick the closed repeated subterm whose let-factoring shrinks TERM the
most; return (values factored changed-p).  CHANGED-P is NIL when no
factoring helps."
  (let ((counts (%subterm-counts term))
        (best nil)
        (best-len (blc-length term)))
    (maphash
     (lambda (sub n)
       (when (and (>= n 2)
                  (not (term-var-p sub))
                  (not (equal sub term))
                  (closed-p sub))
         (let* ((cand (tapp (tlam (%replace-with-letvar term sub 0)) sub))
                (len (blc-length cand)))
           (when (< len best-len)
             (setf best cand best-len len)))))
     counts)
    (if best (values best t) (values term nil))))

(defun blc-cse-factor (term)
  "CSE-factor TERM: greedily let-factor closed repeated subterms until no
factoring shrinks it.  Returns (values factored n-factors).  The result is a
plain BLC term (pure BLC already encodes lets) that is structurally
reversible to TERM via blc-cse-expand."
  (let ((cur term) (n 0))
    (loop
      (multiple-value-bind (next changed) (%factor-once cur)
        (unless changed (return (values cur n)))
        (setf cur next)
        (incf n)))))

(defun blc-cse-expand (factored n-factors)
  "Reverse N-FACTORS factoring steps applied by blc-cse-factor, recovering
the original term."
  (let ((cur factored))
    (dotimes (i n-factors cur)
      (setf cur (%restore-top cur)))))

(defun blc-length-shared (term)
  "The CSE-factored BLC bit-length of TERM: the description length once
repeated closed subterms are shared through lets.  <= (blc-length term),
with equality exactly when no factoring helps."
  (blc-length (blc-cse-factor term)))

;;; ------------------------------------------------------------------
;;; 2. Minimal DAG / back-reference encoding.
;;; ------------------------------------------------------------------
;;;
;;; A small header carries the dictionary size D (Elias-gamma of D+1).
;;;
;;;   D = 0 : there is nothing worth sharing; the rest is EXACTLY pure BLC
;;;           (so blc-dag-length = blc-length + 1, the one-bit header is the
;;;           whole O(1) overhead, and the gamma code's leading 1 marks it).
;;;
;;;   D > 0 : D dictionary definitions (each distinct non-trivial subterm
;;;           that occurs >= 2 times, smallest first) then the root, in the
;;;           shared grammar with four 2-bit opcodes:
;;;
;;;             00  lambda        ; body follows
;;;             01  application   ; f then g follow
;;;             10  variable      ; de Bruijn index as 1^(n+1) 0
;;;             11  back-ref      ; dictionary id as Elias-gamma(id+1)
;;;
;;;           The gamma header for D>0 begins with a 0, so the first bit
;;;           alone discriminates the two modes for the decoder.
;;;
;;; blc-dag-encode picks whichever of {plain, DAG} is shorter, so the DAG
;;; form is never worse than plain BLC + 1 bit, and is strictly shorter than
;;; the tree exactly when sharing pays for itself.

(defun %gamma-encode (n)
  "Elias-gamma code of the positive integer N (>= 1) as a bit list."
  (let* ((bits (loop for k = n then (ash k -1) while (plusp k)
                     collect (logand k 1)))     ; LSB-first
         (rev (nreverse bits))                  ; MSB-first, leading 1
         (len (length rev)))
    (append (make-list (1- len) :initial-element 0) rev)))

(defun %gamma-decode (bits)
  "Decode one Elias-gamma code from the front of BITS; (values n rest)."
  (let ((zeros 0) (cur bits))
    (loop while (and cur (zerop (first cur))) do (incf zeros) (setf cur (rest cur)))
    ;; cur now starts with the leading 1; read zeros+1 bits as the number.
    (let ((n 0))
      (dotimes (i (1+ zeros))
        (when (null cur) (error "blc-dag-decode: truncated gamma code"))
        (setf n (+ (ash n 1) (first cur)) cur (rest cur)))
      (values n cur))))

(defun %unary-encode (n)
  "Variable index N as 1^(n+1) 0 (the BLC variable code)."
  (append (make-list (1+ n) :initial-element 1) (list 0)))

(defun %unary-decode (bits)
  "Decode a variable index 1^(n+1) 0 from BITS; (values n rest)."
  (let ((ones 0) (cur bits))
    (loop while (and cur (= (first cur) 1)) do (incf ones) (setf cur (rest cur)))
    (when (null cur) (error "blc-dag-decode: variable not terminated"))
    (values (1- ones) (rest cur))))

(defun %bits-lex< (a b)
  "Lexicographic order on bit lists (0 < 1; a proper prefix sorts first)."
  (loop
    (cond ((null a) (return (not (null b))))
          ((null b) (return nil))
          ((< (first a) (first b)) (return t))
          ((> (first a) (first b)) (return nil))
          (t (setf a (rest a) b (rest b))))))

(defun %dag-dictionary (term)
  "The shared subterms of TERM (occurrence count >= 2, not a bare variable),
ordered canonically (size ascending, then BLC bits) and assigned ids 0..D-1.
Returns (values ordered-list id-table) where id-table is EQUAL term -> id."
  (let ((counts (%subterm-counts term))
        (shared '()))
    (maphash (lambda (sub n)
               (when (and (>= n 2) (not (term-var-p sub)))
                 (push sub shared)))
             counts)
    (setf shared
          (sort shared
                (lambda (x y)
                  (let ((sx (node-size x)) (sy (node-size y)))
                    (cond ((< sx sy) t)
                          ((> sx sy) nil)
                          (t (%bits-lex< (blc-encode x) (blc-encode y))))))))
    (let ((ids (make-hash-table :test 'equal)))
      (loop for sub in shared for i from 0 do (setf (gethash sub ids) i))
      (values shared ids))))

(defun %dag-emit (node ids force-structural)
  "Emit NODE in the shared grammar.  Unless FORCE-STRUCTURAL, a node present
in the IDS dictionary is emitted as a back-reference."
  (let ((id (and (not force-structural) (gethash node ids))))
    (cond
      (id (list* 1 1 (%gamma-encode (1+ id))))
      ((term-var-p node) (list* 1 0 (%unary-encode (term-index node))))
      ((term-lam-p node) (list* 0 0 (%dag-emit (term-body node) ids nil)))
      ((term-app-p node) (list* 0 1 (append (%dag-emit (term-fun node) ids nil)
                                            (%dag-emit (term-arg node) ids nil))))
      (t (error "blc-dag-encode: not a lambda term: ~S" node)))))

(defun blc-dag-encode (term)
  "Encode TERM as a minimal DAG with back-references (a bit list).
Canonical: structurally identical terms encode to identical bits.  Returns
whichever is shorter of the DAG form and plain BLC + a 1-bit header, so it is
never worse than tree + O(1) and strictly shorter when sharing pays off."
  (multiple-value-bind (ordered ids) (%dag-dictionary term)
    (let* ((plain (append (%gamma-encode 1) (blc-encode term)))  ; D = 0
           (dag (when ordered
                  (append (%gamma-encode (1+ (length ordered)))
                          (loop for sub in ordered
                                append (%dag-emit sub ids t)) ; defs: structural top
                          (%dag-emit term ids nil)))))         ; root
      (if (and dag (< (length dag) (length plain))) dag plain))))

(defun blc-dag-length (term)
  "Bit count of (blc-dag-encode term) -- the DAG-honest description length."
  (length (blc-dag-encode term)))

(defun %dag-decode-node (bits dict limit)
  "Decode one shared-grammar node from BITS; (values term rest).  Back-refs
resolve against DICT (a vector) at ids < LIMIT."
  (when (or (null bits) (null (rest bits)))
    (error "blc-dag-decode: truncated node"))
  (let ((a (first bits)) (b (second bits)) (rest (cddr bits)))
    (cond
      ((and (zerop a) (zerop b))                 ; 00 lambda
       (multiple-value-bind (body more) (%dag-decode-node rest dict limit)
         (values (tlam body) more)))
      ((and (zerop a) (= b 1))                   ; 01 application
       (multiple-value-bind (f more) (%dag-decode-node rest dict limit)
         (multiple-value-bind (g more2) (%dag-decode-node more dict limit)
           (values (tapp f g) more2))))
      ((and (= a 1) (zerop b))                   ; 10 variable
       (multiple-value-bind (n more) (%unary-decode rest)
         (values (tvar n) more)))
      (t                                         ; 11 back-reference
       (multiple-value-bind (g more) (%gamma-decode rest)
         (let ((id (1- g)))
           (unless (< id limit)
             (error "blc-dag-decode: back-reference ~D out of range" id))
           (values (aref dict id) more)))))))

(defun blc-dag-decode (bits)
  "Decode a DAG encoding BITS back to a tree (back-references expanded).
Equal to the original term."
  (multiple-value-bind (gv rest) (%gamma-decode bits)
    (let ((d (1- gv)))
      (if (zerop d)
          ;; plain BLC mode.
          (blc-decode rest)
          (let ((dict (make-array d)))
            (dotimes (i d)
              (multiple-value-bind (node more) (%dag-decode-node rest dict i)
                (setf (aref dict i) node rest more)))
            (multiple-value-bind (root more) (%dag-decode-node rest dict d)
              (when more
                (error "blc-dag-decode: ~D trailing bit(s)" (length more)))
              root))))))
