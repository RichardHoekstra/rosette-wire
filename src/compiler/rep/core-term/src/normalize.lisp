;;;; normalize.lisp --- coincidence 2 of 3: PROVE.
;;;;
;;;; The core novelty over rosette-proof-kernel: there, a term is normalized by
;;;; NbE and a certificate is ATTACHED afterwards.  Here, NORMALIZATION ITSELF
;;;; EMITS THE PROOF.  Reducing a ground core term to its normal form (a
;;;; literal) produces one rosette-proof-carrier-core PROOF-NODE per reduction
;;;; step -- a delta-unfold for a function call, a beta/let-substitution for
;;;; LET, an iota step for IF, a primitive delta-reduction for each operator --
;;;; and the whole reduction tree is sealed into an rosette-proof-witness
;;;; CERTIFICATE whose witness RE-RUNS the normalization independently.
;;;;
;;;; DEFINITIONAL EQUALITY of two core terms is decided by NORMAL-FORM IDENTITY
;;;; (both reduce to the same literal), exactly the proof-kernel discipline --
;;;; but the deciding reduction is the same one that carries the proof.

(in-package #:rosette-core-term)

;;; --- normalize-with-proof --------------------------------------------------
;;; Returns (values NORMAL-FORM-INT PROOF-NODE).  ENV maps symbol-name -> int
;;; (already-normalized values, since the fragment is ground).  Each clause
;;; both computes the value AND records the reduction step it just performed.

(defun %reduce (term env sig)
  (cond
    ((integerp term)
     (values term (proof-leaf :value 'lit :payload term)))
    ((symbolp term)
     (let ((v (cdr (assoc (symbol-name term) env :test #'string=))))
       (values v (proof-leaf :value (symbol-name term) :payload v))))
    ((consp term)
     (let ((head (first term)))
       (cond
         ;; IF: iota reduction -- record the tested value and the branch taken.
         ((sym= head "IF")
          (multiple-value-bind (vc nc) (%reduce (second term) env sig)
            (let ((taken (if (zerop vc) (fourth term) (third term))))
              (multiple-value-bind (vb nb) (%reduce taken env sig)
                (values vb
                        (proof-branch :iota (if (zerop vc) "if-false" "if-true")
                                      (list nc nb) :payload vb))))))
         ;; LET: beta/substitution reduction -- x := (normal form of e) in b.
         ((sym= head "LET")
          (multiple-value-bind (ve ne) (%reduce (third term) env sig)
            (let ((env* (cons (cons (symbol-name (second term)) ve) env)))
              (multiple-value-bind (vb nb) (%reduce (fourth term) env* sig)
                (values vb
                        (proof-branch :beta (symbol-name (second term))
                                      (list ne nb) :payload vb))))))
         ;; LAM: a value in normal form -- capture the env as a closure.
         ((sym= head "LAM")
          (values (make-closure (second term) (third term) (fourth term) env)
                  (proof-leaf :value 'lam :payload :closure)))
         ;; APP: beta-reduction of a closure applied to a normalized argument.
         ((sym= head "APP")
          (multiple-value-bind (cl nf) (%reduce (second term) env sig)
            (multiple-value-bind (av na) (%reduce (third term) env sig)
              (let ((env* (cons (cons (symbol-name (core-closure-param cl)) av)
                                (core-closure-env cl))))
                (multiple-value-bind (vb nb) (%reduce (core-closure-body cl) env* sig)
                  (values vb
                          (proof-branch :beta-app (symbol-name (core-closure-param cl))
                                        (list nf na nb) :payload vb)))))))
         ;; OP: primitive delta-reduction on the two normalized operands.
         ((binop-name head)
          (multiple-value-bind (va na) (%reduce (second term) env sig)
            (multiple-value-bind (vb nb) (%reduce (third term) env sig)
              (let ((v (apply-binop (binop-name head) va vb)))
                (values v
                        (proof-branch :delta-prim (binop-name head)
                                      (list na nb) :payload v))))))
         ;; CALL: delta-unfold of a defined function on normalized arguments.
         (t
          (let* ((entry (sig-lookup sig head))
                 (params (car entry)) (body (cdr entry))
                 (arg-nodes '()) (env* '()))
            (loop for p in params for a in (rest term) do
              (multiple-value-bind (va na) (%reduce a env sig)
                (push na arg-nodes)
                (push (cons (symbol-name p) va) env*)))
            (multiple-value-bind (vbody nbody) (%reduce body (nreverse env*) sig)
              (values vbody
                      (proof-branch :delta-unfold (symbol-name head)
                                    (append (nreverse arg-nodes) (list nbody))
                                    :payload vbody))))))))
    (t (error "rosette-core-term: bad term ~S" term))))

(defun term-normalize (term sig)
  "Reduce closed ground TERM to its normal form.  Returns (values INT
PROOF-NODE), the proof being emitted BY the reduction."
  (%reduce term '() sig))

(defun program-normalize (program)
  "Normalize PROGRAM's MAIN.  Type-checks first.  Returns (values INT NODE)."
  (term-check program)
  (term-normalize (core-program-main program) (program-signature program)))

;;; --- seal the reduction into a re-runnable certificate ---------------------

(defun %funs->plist (funs)
  "Serialize FUNS to a re-readable plist form (symbols kept as data)."
  (loop for (name params body) in funs
        collect (list :name name :params params :body body)))

(defun %plist->funs (plist)
  (loop for f in plist
        collect (list (getf f :name) (getf f :params) (getf f :body))))

(defun normalization-certificate (program)
  "Normalize PROGRAM and SEAL the reduction into an rosette-proof-witness
CERTIFICATE.  The certificate PAYLOAD carries the program, the term, the
sealed normal-form VALUE, and the emitted proof-node (as a plist).  Its
WITNESS re-runs the normalization from the payload and checks that BOTH the
recomputed value AND the freshly re-derived proof TRACE equal the sealed ones --
the reduction trace IS the certificate.  So a tampered VALUE, a garbage proof
node, or a proof grafted from an unrelated (even same-value) reduction all fail
RUNTIME-VERIFY.  NORMALIZING PRODUCED THE CERTIFICATE."
  (term-check program)
  (multiple-value-bind (value node)
      (term-normalize (core-program-main program) (program-signature program))
    (let* ((payload (list :funs (%funs->plist (core-program-funs program))
                          :term (core-program-main program)
                          :value value
                          :proof (proof-node->plist node)
                          :proof-size (proof-node-size node)
                          :proof-depth (proof-node-depth node)))
           (cert (make-certificate
                  :name :core-term-normalization
                  :kind :proof
                  :claim :normal-form
                  :payload payload
                  :passed t)))
      ;; Install a witness that RE-DERIVES the value from the payload, so the
      ;; certificate carries its own falsifier: reading the sealed value from
      ;; the (possibly tampered) certificate and comparing to a fresh reduction.
      (setf (certificate-witness cert)
            (make-lean-witness
             :core-term-normal-form-re-runs
             (lambda () (%recheck-certificate cert))))
      cert)))

(defun %recheck-certificate (cert)
  "Independently re-normalize the term stored in CERT and require BOTH the
recomputed normal-form VALUE *and* the freshly re-derived PROOF TRACE to equal
the ones CERT seals.  The reduction trace IS the certificate: re-running the
normalization must reproduce the very proof node the certificate carries, step
for step (kind / label / payload / children).  So a tampered value, a garbage
proof node, or a proof grafted from an unrelated (even same-VALUE) reduction all
fail here.  Returns T iff the certificate is honest."
  (let* ((pl (certificate-payload cert))
         (prog (make-core-program :funs (%plist->funs (getf pl :funs))
                                  :main (getf pl :term)))
         (claimed (getf pl :value))
         (sealed-proof (getf pl :proof)))
    (handler-case
        (multiple-value-bind (v node)
            (term-normalize (core-program-main prog) (program-signature prog))
          (and (eql v claimed)
               ;; the emitted-by-reduction proof must match, node for node.
               (equal (proof-node->plist node) sealed-proof)))
      (error () nil))))

(defun verify-normalization-certificate (cert)
  "Re-run CERT through rosette-proof-witness RUNTIME-VERIFY (which fires the
installed witness).  A forged/tampered certificate returns NIL."
  (runtime-verify cert))

;;; --- definitional equality by normal-form identity -------------------------

(defun definitionally-equal-p (program-a program-b)
  "T iff PROGRAM-A's MAIN and PROGRAM-B's MAIN reduce to the SAME normal form.
Two beta-equal terms share a normal form; two in-equal terms do not."
  (term-check program-a) (term-check program-b)
  (eql (nth-value 0 (program-normalize program-a))
       (nth-value 0 (program-normalize program-b))))

(defun definitional-equality-certificate (program-a program-b)
  "A certificate that PROGRAM-A =def PROGRAM-B (or is refuted).  PASSED is the
verdict; the witness re-decides equality independently."
  (let* ((va (nth-value 0 (program-normalize program-a)))
         (vb (nth-value 0 (program-normalize program-b)))
         (equal? (eql va vb))
         (cert (make-certificate
                :name :core-term-definitional-equality
                :kind :proof
                :claim :def-equal
                :payload (list :value-a va :value-b vb :equal equal?)
                :passed equal?)))
    (setf (certificate-witness cert)
          (make-lean-witness
           :core-term-def-equality-re-runs
           (lambda ()
             (eql equal? (definitionally-equal-p program-a program-b)))))
    cert))
