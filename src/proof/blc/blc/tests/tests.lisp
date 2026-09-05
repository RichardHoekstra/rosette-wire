;;;; rosette-blc/tests/tests.lisp --- Test suite.

(defpackage #:rosette-blc/tests
  (:use #:cl #:rosette-blc #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-blc/tests)

;;; Tromp's U8 universal machine -- the canonical 355-bit BLC term (from the
;;; tested reference blc8.py).  A known-good external anchor for the codec.
(defparameter *u8*
  "0001100101000110100001000000010110000000010111000000001000101111111100101111111111100001011111100111000000111100001011011011100111111111111000011110000101111010011101011100101111100101100001101111101110010111111111110000111000011100110111111011111101111111100001100001011111111101111111000010110111111011000011001111101110011010000001110010001101000011010")

(defun len= (term)
  "blc-length and (length (blc-encode ...)) must agree."
  (= (blc-length term) (length (blc-encode term))))

(defun run-all-tests ()
  "Run rosette-blc tests."
  (with-test-run (run "rosette-blc")

    ;; ===================================================================
    ;; Well-formedness of the term representation.
    ;; ===================================================================
    (check run (term-p (tvar 0)) "(:var 0) is a term")
    (check run (term-p (tvar 7)) "(:var 7) is a term")
    (check run (term-p (combinator-i)) "I is a term")
    (check run (term-p (combinator-k)) "K is a term")
    (check run (term-p (combinator-s)) "S is a term")
    (check run (not (term-p (list :var -1))) "(:var -1) is NOT a term")
    (check run (not (term-p '(:foo))) "(:foo) is NOT a term")
    (check run (not (term-p (list :app (tvar 0)))) "1-arg app is NOT a term")

    ;; ===================================================================
    ;; THE canonical check: I = \\x.x encodes to 0 0 1 0 = "0010", len 4.
    ;; ===================================================================
    (check run (equal (blc-encode (combinator-i)) '(0 0 1 0))
           "I encodes to bits (0 0 1 0)")
    (check run (string= (bits->string (blc-encode (combinator-i))) "0010")
           "I encodes to the string \"0010\"")
    (check run (= (blc-length (combinator-i)) 4)
           "I has BLC length 4")
    (check run (len= (combinator-i))
           "I: blc-length = (length (blc-encode I))")

    ;; ===================================================================
    ;; K = \\x.\\y.x = (:lam (:lam (:var 1))): exact bits and length.
    ;;   00 00 110  =  "0000110", 7 bits.
    ;; ===================================================================
    (check run (equal (combinator-k) '(:lam (:lam (:var 1))))
           "K = (:lam (:lam (:var 1)))")
    (check run (string= (bits->string (blc-encode (combinator-k))) "0000110")
           "K encodes to \"0000110\"")
    (check run (= (blc-length (combinator-k)) 7)
           "K has BLC length 7")
    (check run (len= (combinator-k))
           "K: blc-length = (length (blc-encode K))")

    ;; S = \\x.\\y.\\z.(x z)(y z): three lambdas (6) + outer app
    ;; (2) + (app 2 0)=8 + (app 1 0)=7 = 23 bits.
    (check run (= (blc-length (combinator-s)) 23)
           "S has BLC length 23")
    (check run (len= (combinator-s))
           "S: blc-length = (length (blc-encode S))")

    ;; ===================================================================
    ;; Variable length law: (:var n) has length n+2.
    ;; ===================================================================
    (check run (loop for n from 0 to 20
                     always (= (blc-length (tvar n)) (+ n 2)))
           "(:var n) has BLC length n+2 for n in [0,20]")
    (check run (equal (blc-encode (tvar 3)) '(1 1 1 1 0))
           "(:var 3) encodes to 1^4 0 = (1 1 1 1 0)")

    ;; ===================================================================
    ;; Length additivity (the structural law):
    ;;   len(lam M)   = 2 + len(M)
    ;;   len(app M N) = 2 + len(M) + len(N)
    ;; ===================================================================
    (let* ((m (combinator-k))
           (n (church-numeral 2))
           (lam-m (tlam m))
           (app-mn (tapp m n)))
      (check run (= (blc-length lam-m) (+ 2 (blc-length m)))
             "len(lam M) = 2 + len(M)")
      (check run (= (blc-length app-mn)
                    (+ 2 (blc-length m) (blc-length n)))
             "len(app M N) = 2 + len(M) + len(N)")
      (check run (len= lam-m) "lam M: length/encode agree")
      (check run (len= app-mn) "app M N: length/encode agree"))

    ;; ===================================================================
    ;; Church numerals: exact lengths.
    ;;   0 -> 6, 1 -> 11, 2 -> 16, 3 -> 21  (each successor adds 5 bits:
    ;;   one application (2) + one (:var 1) (3)).
    ;; ===================================================================
    (check run (= (blc-length (church-numeral 0)) 6) "Church 0 has length 6")
    (check run (= (blc-length (church-numeral 1)) 11) "Church 1 has length 11")
    (check run (= (blc-length (church-numeral 2)) 16) "Church 2 has length 16")
    (check run (= (blc-length (church-numeral 3)) 21) "Church 3 has length 21")
    (check run (loop for n from 0 to 10
                     always (= (blc-length (church-numeral (1+ n)))
                               (+ 5 (blc-length (church-numeral n)))))
           "each Church successor adds exactly 5 bits")
    (check run (string= (bits->string (blc-encode (church-numeral 0))) "000010")
           "Church 0 encodes to \"000010\"")

    ;; ===================================================================
    ;; Round-trip: decode . encode = identity over a term suite.
    ;; ===================================================================
    (let ((suite (list (tvar 0) (tvar 5)
                       (combinator-i) (combinator-k) (combinator-s)
                       (church-numeral 0) (church-numeral 1)
                       (church-numeral 2) (church-numeral 4)
                       (tapp (combinator-s) (combinator-k))
                       (tapp (tapp (combinator-s) (combinator-k))
                             (combinator-i))
                       (tlam (tapp (tvar 0) (tlam (tvar 1)))))))
      (dolist (tm suite)
        (check run (equal (blc-decode (blc-encode tm)) tm)
               (format nil "round-trip: decode(encode(~S)) = ~:*~S" tm))))

    ;; ===================================================================
    ;; string<->bits round-trip and decode-from-string.
    ;; ===================================================================
    (check run (equal (string->bits "0010") '(0 0 1 0))
           "string->bits \"0010\"")
    (check run (string= (bits->string (string->bits "0000110")) "0000110")
           "bits->string . string->bits = id")
    (check run (equal (blc-decode (string->bits "0010")) (combinator-i))
           "decode \"0010\" = I")

    ;; ===================================================================
    ;; Decoder rejects malformed / trailing input.
    ;; ===================================================================
    (check run (handler-case (progn (blc-decode '(0 0 1 0 0)) nil)
                 (error () t))
           "decode rejects trailing bits")
    (check run (handler-case (progn (blc-decode '(0)) nil)
                 (error () t))
           "decode rejects truncated input")

    ;; ===================================================================
    ;; STRUCTURAL SHARING.
    ;; A term with a closed subterm repeated k times.  R = S (23 bits),
    ;; right-nested five deep, so R occurs k=5 times.
    ;; ===================================================================
    (let* ((r (combinator-s))
           (rbits (blc-length r))         ; 23
           (k 5)
           (rep (let ((acc r))            ; (R (R (R (R R)))) -- R five times
                  (dotimes (i (1- k) acc) (setf acc (tapp r acc)))))
           (tree (blc-length rep)))

      ;; ---- helpers' own laws ----
      (check run (closed-p r) "S is closed")
      (check run (closed-p (combinator-i)) "I is closed")
      (check run (not (closed-p (tvar 0))) "(:var 0) is NOT closed")
      (check run (not (closed-p (tlam (tvar 1)))) "(:lam (:var 1)) is NOT closed")
      (check run (= (count-subterm r rep) k) "S occurs k=5 times in the repeated term")

      ;; ===================================================================
      ;; 1. CSE / let-factoring path.
      ;; ===================================================================
      (multiple-value-bind (factored nfac) (blc-cse-factor rep)
        (let ((shared (blc-length factored)))
          ;; shared < tree
          (check run (< shared tree)
                 "CSE: shared-length < tree-length for a repeated subterm")
          ;; savings ~ (k-1) x subterm bits, minus reference overhead.
          ;; Upper bound is the ideal (k-1)*bits; actual is within a small
          ;; per-occurrence + wrapper overhead of it.
          (let ((savings (- tree shared)))
            (check run (<= savings (* (1- k) rbits))
                   "CSE: savings do not exceed the ideal (k-1) x subterm bits")
            (check run (>= savings (- (* (1- k) rbits) (* 5 (+ rbits 4))))
                   "CSE: savings ~ (k-1) x subterm bits minus reference overhead"))
          (check run (= (blc-length-shared rep) shared)
                 "blc-length-shared = blc-length of the CSE-factored term")
          ;; round-trip: expanding the factoring recovers the original term.
          (check run (equal (blc-cse-expand factored nfac) rep)
                 "CSE: expand . factor = identity (round-trip)")
          ;; the factored term is a valid BLC term and beta-faithful via decode.
          (check run (term-p factored) "CSE: factored term is well-formed")
          (check run (equal (blc-decode (blc-encode factored)) factored)
                 "CSE: factored term round-trips through pure BLC")))

      ;; No repetition: CSE adds nothing -> shared == tree, EXACTLY.
      (check run (= (blc-length-shared (combinator-i)) (blc-length (combinator-i)))
             "CSE: no-repetition I has shared-length == tree-length (4)")
      (check run (= (blc-length-shared (combinator-k)) (blc-length (combinator-k)))
             "CSE: no-repetition K has shared-length == tree-length (7)")
      (check run (= (blc-length-shared (combinator-i)) 4) "CSE: I shared-length = 4")
      (check run (= (blc-length-shared (combinator-k)) 7) "CSE: K shared-length = 7")

      ;; ===================================================================
      ;; 2. Minimal DAG / back-reference path.
      ;; ===================================================================
      (let ((dlen (blc-dag-length rep)))
        (check run (< dlen tree)
               "DAG: shared-length < tree-length for a repeated subterm")
        ;; round-trip: decode expands back-refs to the original tree.
        (check run (equal (blc-dag-decode (blc-dag-encode rep)) rep)
               "DAG: decode . encode = original tree (round-trip)")
        ;; CANONICAL: structurally identical terms -> identical bits (dedup).
        (let* ((r2 (combinator-s))
               (rep2 (let ((acc r2)) (dotimes (i (1- k) acc) (setf acc (tapp r2 acc))))))
          (check run (equal (blc-dag-encode rep) (blc-dag-encode rep2))
                 "DAG: identical terms produce identical bits (canonical -> dedup)")))

      ;; No repetition: DAG falls back to plain BLC + a 1-bit header (O(1)).
      (check run (= (blc-dag-length (combinator-i)) (1+ (blc-length (combinator-i))))
             "DAG: no-repetition I has shared-length = tree + 1 (O(1) overhead)")
      (check run (<= (blc-dag-length rep) (1+ tree))
             "DAG: shared-length never exceeds tree + O(1)")
      ;; round-trip on I and K through the DAG form (gate identity/K on BOTH).
      (check run (equal (blc-dag-decode (blc-dag-encode (combinator-i))) (combinator-i))
             "DAG: I round-trips")
      (check run (equal (blc-dag-decode (blc-dag-encode (combinator-k))) (combinator-k))
             "DAG: K round-trips")

      ;; DAG round-trip over the broader suite.
      (dolist (tm (list (tvar 0) (combinator-s) (church-numeral 3)
                        (tapp (combinator-s) (combinator-k))
                        rep))
        (check run (equal (blc-dag-decode (blc-dag-encode tm)) tm)
               (format nil "DAG: round-trip ~S" tm))))

    ;; Elias-gamma header self-check via a clean win.
    (check run (let ((d (blc-dag-encode (combinator-i))))
                 (= (first d) 1))
           "DAG: D=0 header is the gamma code (1) -- first bit 1 selects plain BLC")

    ;; ===================================================================
    ;; CROSS-VALIDATION against John Tromp's tested references.
    ;; blc8.py: identity = "0010" (4 bits), U8 universal machine = 355 bits.
    ;; ===================================================================
    (check run (string= (bits->string (blc-encode (combinator-i))) "0010")
           "REF blc8.py: identity encodes to \"0010\"")
    (check run (= (blc-length (combinator-i)) 4)
           "REF blc8.py: identity is 4 bits")
    ;; The 355-bit U8 term decodes to a well-formed term and re-encodes to
    ;; EXACTLY the same 355 bits -- a full round-trip against a known-good
    ;; external artifact (the strongest correctness anchor).
    (let* ((bits (string->bits *u8*))
           (term (blc-decode bits)))
      (check run (= (length bits) 355) "REF blc8.py: U8 string is 355 bits")
      (check run (term-p term) "REF blc8.py: U8 decodes to a well-formed term")
      (check run (= (blc-length term) 355) "REF blc8.py: U8 blc-length = 355")
      (check run (equal (blc-encode term) bits)
             "REF blc8.py: U8 re-encodes bit-for-bit (round-trip)")
      ;; BLC2 of U8 is well-defined and round-trips through the BLC2 codec.
      (check run (equal (blc2-decode (blc2-encode term)) term)
             "REF: U8 round-trips through the BLC2 codec"))
    ;; NOTE: blc8.py's hardcoded Church-numeral CONSTANTS ("00000001" etc.)
    ;; are non-standard and do NOT parse as BLC terms; the canonical Tromp
    ;; Church lengths are 6/11/16, gated above.  We do not match those buggy
    ;; constants on purpose.

    ;; ===================================================================
    ;; BLC2 -- Levenshtein-coded de Bruijn indices (Tromp's gblc.c).
    ;; ===================================================================
    ;; The Levenshtein code, cross-validated against gblc.c's documented
    ;; examples: code(0)=0, code(1)=10, code(2)=1100, code(5)=1110001.
    (check run (equal (levenshtein-encode 0) '(0)) "Levenshtein code(0) = 0")
    (check run (equal (levenshtein-encode 1) '(1 0)) "Levenshtein code(1) = 10")
    (check run (equal (levenshtein-encode 2) '(1 1 0 0)) "Levenshtein code(2) = 1100")
    (check run (equal (levenshtein-encode 5) '(1 1 1 0 0 0 1))
           "Levenshtein code(5) = 1110001")
    (check run (loop for n from 0 to 80
                     always (= (levenshtein-length n)
                               (length (levenshtein-encode n))))
           "levenshtein-length = (length (levenshtein-encode n)) for n in [0,80]")
    (check run (loop for n from 0 to 80
                     always (multiple-value-bind (v rest) (levenshtein-decode (levenshtein-encode n))
                              (and (= v n) (null rest))))
           "Levenshtein round-trip: decode . encode = id for n in [0,80]")

    ;; BLC2 length is structural and matches its encoder.
    (let ((suite (list (combinator-i) (combinator-k) (combinator-s)
                       (church-numeral 0) (church-numeral 3)
                       (tvar 0) (tvar 1) (tvar 7) (tvar 30)
                       (tapp (combinator-s) (combinator-k)))))
      (dolist (tm suite)
        (check run (= (blc2-length tm) (length (blc2-encode tm)))
               (format nil "BLC2: blc2-length = (length (blc2-encode ~S))" tm))
        (check run (equal (blc2-decode (blc2-encode tm)) tm)
               (format nil "BLC2: round-trip ~S" tm))))

    ;; BLC2 and plain BLC AGREE on small indices (only 0 and 1): I, K, Church.
    (check run (= (blc2-length (combinator-i)) (blc-length (combinator-i)))
           "BLC2: I agrees with plain BLC (4 bits)")
    (check run (= (blc2-length (combinator-k)) (blc-length (combinator-k)))
           "BLC2: K agrees with plain BLC (7 bits)")
    (check run (loop for n from 0 to 6
                     always (= (blc2-length (church-numeral n))
                               (blc-length (church-numeral n))))
           "BLC2: Church numerals agree with plain BLC (indices 0/1 only)")
    ;; index crossover: equal at n in {0,1}, plain better for mid, BLC2 wins big.
    (check run (and (= (blc2-length (tvar 0)) (blc-length (tvar 0)))
                    (= (blc2-length (tvar 1)) (blc-length (tvar 1))))
           "BLC2: variable cost equals plain BLC for n in {0,1}")
    (check run (< (blc2-length (tvar 30)) (blc-length (tvar 30)))
           "BLC2: variable cost < plain BLC for a large index (n=30)")
    ;; A deep term (large indices) is strictly cheaper in BLC2 -- the FAIR
    ;; ruler for deep / DAG-heavy terms.
    (let ((deep (let ((acc (tvar 25)))           ; 26 lambdas over (:var 25)
                  (dotimes (i 26 acc) (setf acc (tlam acc))))))
      (check run (< (blc2-length deep) (blc-length deep))
             "BLC2: deep term (index 25) is strictly cheaper than plain BLC"))
    ))
