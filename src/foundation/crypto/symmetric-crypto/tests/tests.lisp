;;;; tests/tests.lisp --- rosette-symmetric-crypto test suite.
;;;;
;;;; Every numeric test vector below is transcribed from a published
;;;; standard (FIPS-197 / NIST SP 800-38A / NIST SP 800-38D CAVP /
;;;; RFC 8439 / RFC 4231 / RFC 5869) -- these are exact oracles, not
;;;; self-asserting round-trips.  Round-trip and tamper-detection tests
;;;; are also included where the standards don't hand us every value.
;;;;
;;;; Coverage:
;;;;   1.  SHA-256 known-answer vectors ("" and "abc")
;;;;   2.  HMAC-SHA256 -- RFC 4231 Test Cases 1-3
;;;;   3.  HKDF-SHA256 -- RFC 5869 Test Cases 1-3
;;;;   4.  AES-128/192/256 ECB single-block -- NIST SP 800-38A Appendix F.1
;;;;   5.  AES ECB decrypt inverts encrypt (round trip)
;;;;   6.  AES-128 CTR, all 4 blocks -- NIST SP 800-38A Appendix F.5.1/F.5.2
;;;;   7.  AES-128/256-GCM -- NIST SP 800-38D CAVP gcmEncryptExtIV vectors
;;;;   8.  AES-GCM rejects a tampered ciphertext byte and a tampered tag
;;;;   9.  AES-GCM round trip on non-block-aligned plaintext/AAD
;;;;  10.  ChaCha20 block function -- RFC 8439 section 2.3.2
;;;;  11.  ChaCha20 full "Sunscreen" encryption -- RFC 8439 section 2.4.2
;;;;  12.  Poly1305 MAC -- RFC 8439 section 2.5.2
;;;;  13.  Poly1305 key generation -- RFC 8439 section 2.6.2
;;;;  14.  AEAD_CHACHA20_POLY1305 -- RFC 8439 section 2.8.2
;;;;  15.  ChaCha20-Poly1305 rejects a tampered ciphertext byte

(defpackage #:rosette-symmetric-crypto/tests
  (:use #:cl #:rosette-symmetric-crypto)
  (:export #:run-all-tests))

(in-package #:rosette-symmetric-crypto/tests)

(defvar *test-count* 0)
(defvar *failure-count* 0)

(defun reset-counters ()
  (setf *test-count* 0
        *failure-count* 0))

(defmacro is (form &optional (message "assertion failed"))
  `(progn
     (incf *test-count*)
     (unless ,form
       (incf *failure-count*)
       (format t "  FAIL: ~A~%       form: ~S~%" ,message ',form))))

(defmacro test-case (name &body body)
  `(progn
     (format t "~&~A~%" ,name)
     ,@body))

(defun hex= (bytes hexstring)
  (string-equal (bytes->hex bytes) hexstring))

;;;; --- 1. SHA-256 known-answer vectors ------------------------------

(defun test-sha256 ()
  (test-case "SHA-256 known-answer vectors"
    (is (hex= (sha256 #())
              "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        "SHA-256('') matches the empty-string known answer")
    (is (hex= (sha256 (string->bytes "abc"))
              "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
        "SHA-256('abc') matches the classic known answer")))

;;;; --- 2. HMAC-SHA256 -- RFC 4231 -----------------------------------

(defun test-hmac ()
  (test-case "HMAC-SHA256 -- RFC 4231 Test Cases 1-3"
    (is (hex= (hmac-sha256 (hex->bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
                            (string->bytes "Hi There"))
              "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
        "RFC 4231 Test Case 1")
    (is (hex= (hmac-sha256 (string->bytes "Jefe")
                            (string->bytes "what do ya want for nothing?"))
              "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
        "RFC 4231 Test Case 2 (key shorter than output)")
    (is (hex= (hmac-sha256 (hex->bytes "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                            (hex->bytes (make-string 100 :initial-element #\d)))
              "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe")
        "RFC 4231 Test Case 3 (combined key+data > block size)")))

;;;; --- 3. HKDF-SHA256 -- RFC 5869 ------------------------------------

(defun test-hkdf ()
  (test-case "HKDF-SHA256 -- RFC 5869 Test Cases 1-3"
    (is (hex= (hkdf (hex->bytes "000102030405060708090a0b0c")
                     (hex->bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
                     (hex->bytes "f0f1f2f3f4f5f6f7f8f9")
                     42)
              "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865")
        "RFC 5869 Test Case 1 (basic)")
    (is (hex= (hkdf (hex->bytes "606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf")
                     (hex->bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f")
                     (hex->bytes "b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff")
                     82)
              "b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87")
        "RFC 5869 Test Case 2 (longer inputs/outputs)")
    (is (hex= (hkdf nil
                     (hex->bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b")
                     #()
                     42)
              "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8")
        "RFC 5869 Test Case 3 (zero-length salt/info)")))

;;;; --- 4/5. AES ECB single-block -- NIST SP 800-38A F.1 --------------

(defun test-aes-ecb ()
  (test-case "AES-128/192/256 ECB single block -- NIST SP 800-38A F.1"
    (multiple-value-bind (w128 nr128) (aes-key-schedule (hex->bytes "2b7e151628aed2a6abf7158809cf4f3c"))
      (let* ((pt (hex->bytes "6bc1bee22e409f96e93d7e117393172a"))
             (ct (aes-encrypt-block w128 nr128 pt)))
        (is (hex= ct "3ad77bb40d7a3660a89ecaf32466ef97") "AES-128 ECB.Encrypt block #1")
        (is (equalp (aes-decrypt-block w128 nr128 ct) pt) "AES-128 ECB.Decrypt inverts Encrypt")))
    (multiple-value-bind (w192 nr192)
        (aes-key-schedule (hex->bytes "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"))
      (let* ((pt (hex->bytes "6bc1bee22e409f96e93d7e117393172a"))
             (ct (aes-encrypt-block w192 nr192 pt)))
        (is (hex= ct "bd334f1d6e45f25ff712a214571fa5cc") "AES-192 ECB.Encrypt block #1")
        (is (equalp (aes-decrypt-block w192 nr192 ct) pt) "AES-192 ECB.Decrypt inverts Encrypt")))
    (multiple-value-bind (w256 nr256)
        (aes-key-schedule (hex->bytes "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"))
      (let* ((pt (hex->bytes "6bc1bee22e409f96e93d7e117393172a"))
             (ct (aes-encrypt-block w256 nr256 pt)))
        (is (hex= ct "f3eed1bdb5d2a03c064b5a7e3db181f8") "AES-256 ECB.Encrypt block #1")
        (is (equalp (aes-decrypt-block w256 nr256 ct) pt) "AES-256 ECB.Decrypt inverts Encrypt")))))

;;;; --- 6. AES-128 CTR -- NIST SP 800-38A F.5.1/F.5.2 ------------------

(defun test-aes-ctr ()
  (test-case "AES-128 CTR (4 blocks) -- NIST SP 800-38A F.5.1/F.5.2"
    (let* ((key (hex->bytes "2b7e151628aed2a6abf7158809cf4f3c"))
           (icb (hex->bytes "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
           (pt (hex->bytes (concatenate 'string
                                          "6bc1bee22e409f96e93d7e117393172a"
                                          "ae2d8a571e03ac9c9eb76fac45af8e51"
                                          "30c81c46a35ce411e5fbc1191a0a52ef"
                                          "f69f2445df4f9b17ad2b417be66c3710")))
           (expected-ct "874d6191b620e3261bef6864990db6ce9806f66b7970fdff8617187bb9fffdff5ae4df3edbd5d35e5b4f09020db03eab1e031dda2fbe03d1792170a0f3009cee")
           (ct (aes-ctr-crypt key icb pt)))
      (is (hex= ct expected-ct) "AES-128 CTR encrypt matches NIST vector across all 4 blocks")
      (is (equalp (aes-ctr-crypt key icb ct) pt) "AES-128 CTR decrypt (re-apply keystream) recovers plaintext"))))

;;;; --- 7/8/9. AES-GCM -- NIST SP 800-38D CAVP -------------------------

(defun test-aes-gcm ()
  (test-case "AES-128/256-GCM -- NIST SP 800-38D CAVP gcmEncryptExtIV vectors"
    (multiple-value-bind (ct tag)
        (aes-gcm-encrypt (hex->bytes "c939cc13397c1d37de6ae0e1cb7c423c")
                          (hex->bytes "b3d8cc017cbb89b39e0f67e2")
                          (hex->bytes "c3b3c41f113a31b73d9a5cd432103069")
                          (hex->bytes "24825602bd12a984e0092d3e448eda5f"))
      (is (hex= ct "93fe7d9e9bfd10348a5606e5cafa7354") "AES-128-GCM ciphertext matches CAVP vector")
      (is (hex= tag "0032a1dc85f1c9786925a2e71d8272dd") "AES-128-GCM tag matches CAVP vector")
      (is (equalp (aes-gcm-decrypt (hex->bytes "c939cc13397c1d37de6ae0e1cb7c423c")
                                    (hex->bytes "b3d8cc017cbb89b39e0f67e2")
                                    ct (hex->bytes "24825602bd12a984e0092d3e448eda5f") tag)
                  (hex->bytes "c3b3c41f113a31b73d9a5cd432103069"))
          "AES-128-GCM decrypt recovers the CAVP plaintext"))
    (multiple-value-bind (ct tag)
        (aes-gcm-encrypt (hex->bytes "92e11dcdaa866f5ce790fd24501f92509aacf4cb8b1339d50c9c1240935dd08b")
                          (hex->bytes "ac93a1a6145299bde902f21a")
                          (hex->bytes "2d71bcfa914e4ac045b2aa60955fad24")
                          (hex->bytes "1e0889016f67601c8ebea4943bc23ad6"))
      (is (hex= ct "8995ae2e6df3dbf96fac7b7137bae67f") "AES-256-GCM ciphertext matches CAVP vector")
      (is (hex= tag "eca5aa77d51d4a0a14d9c51e1da474ab") "AES-256-GCM tag matches CAVP vector"))
    ;; tamper detection: flip a ciphertext byte, and separately a tag byte
    (let* ((key (hex->bytes "c939cc13397c1d37de6ae0e1cb7c423c"))
           (iv (hex->bytes "b3d8cc017cbb89b39e0f67e2"))
           (aad (hex->bytes "24825602bd12a984e0092d3e448eda5f")))
      (multiple-value-bind (ct tag) (aes-gcm-encrypt key iv (hex->bytes "c3b3c41f113a31b73d9a5cd432103069") aad)
        (let ((bad-ct (copy-seq ct)))
          (setf (aref bad-ct 0) (logxor (aref bad-ct 0) 1))
          (is (handler-case (progn (aes-gcm-decrypt key iv bad-ct aad tag) nil)
                (gcm-auth-error () t))
              "AES-GCM decrypt signals GCM-AUTH-ERROR on a tampered ciphertext byte"))
        (let ((bad-tag (copy-seq tag)))
          (setf (aref bad-tag 0) (logxor (aref bad-tag 0) 1))
          (is (handler-case (progn (aes-gcm-decrypt key iv ct aad bad-tag) nil)
                (gcm-auth-error () t))
              "AES-GCM decrypt signals GCM-AUTH-ERROR on a tampered tag byte"))))
    ;; round trip on non-block-aligned plaintext/AAD (exercises pad16)
    (let* ((key (hex->bytes "000102030405060708090a0b0c0d0e0f"))
           (iv (hex->bytes "000000000000000000000000"))
           (pt (string->bytes "The quick brown fox jumps"))  ; 26 octets, not a multiple of 16
           (aad (string->bytes "header")))                    ; 6 octets
      (multiple-value-bind (ct tag) (aes-gcm-encrypt key iv pt aad)
        (is (= (length ct) (length pt)) "AES-GCM ciphertext length equals plaintext length")
        (is (equalp (aes-gcm-decrypt key iv ct aad tag) pt)
            "AES-GCM round trip on non-block-aligned plaintext/AAD")))))

;;;; --- 10/11. ChaCha20 -- RFC 8439 -------------------------------------

(defun test-chacha20 ()
  (test-case "ChaCha20 block function + Sunscreen encryption -- RFC 8439 section 2.3-2.4"
    (is (hex= (chacha20-block (hex->bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
                               1
                               (hex->bytes "000000090000004a00000000"))
              "10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e")
        "ChaCha20 block function matches RFC 8439 section 2.3.2")
    (let* ((key (hex->bytes "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))
           (nonce (hex->bytes "000000000000004a00000000"))
           (pt (hex->bytes "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e"))
           (expected-ct "6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0bf91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d807ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab77937365af90bbf74a35be6b40b8eedf2785e42874d")
           (ct (chacha20-crypt key 1 nonce pt)))
      (is (hex= ct expected-ct) "ChaCha20 'Sunscreen' encryption matches RFC 8439 section 2.4.2")
      (is (equalp (chacha20-crypt key 1 nonce ct) pt) "ChaCha20 decrypt (re-apply keystream) recovers plaintext"))))

;;;; --- 12/13. Poly1305 -- RFC 8439 --------------------------------------

(defun test-poly1305 ()
  (test-case "Poly1305 MAC + key generation -- RFC 8439 section 2.5-2.6"
    (is (hex= (poly1305-mac (hex->bytes "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b")
                             (hex->bytes "43727970746f6772617068696320466f72756d2052657365617263682047726f7570"))
              "a8061dc1305136c6c22b8baf0c0127a9")
        "Poly1305 MAC matches RFC 8439 section 2.5.2")
    (is (hex= (subseq (chacha20-block (hex->bytes "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
                                       0
                                       (hex->bytes "000000000001020304050607"))
                       0 32)
              "8ad5a08b905f81cc815040274ab29471a833b637e3fd0da508dbb8e2fdd1a646")
        "poly1305-key-gen's 32-byte key (ChaCha20 block, counter 0) matches RFC 8439 section 2.6.2")))

;;;; --- 14/15. AEAD_CHACHA20_POLY1305 -- RFC 8439 section 2.8 -------------

(defun test-chacha20-poly1305 ()
  (test-case "AEAD_CHACHA20_POLY1305 -- RFC 8439 section 2.8.2"
    (let* ((key (hex->bytes "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
           (nonce (hex->bytes "070000004041424344454647"))
           (aad (hex->bytes "50515253c0c1c2c3c4c5c6c7"))
           (pt (hex->bytes "4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e")))
      (multiple-value-bind (ct tag) (chacha20-poly1305-encrypt key nonce pt aad)
        (is (hex= ct "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116")
            "AEAD_CHACHA20_POLY1305 ciphertext matches RFC 8439 section 2.8.2")
        (is (hex= tag "1ae10b594f09e26a7e902ecbd0600691") "AEAD_CHACHA20_POLY1305 tag matches RFC 8439 section 2.8.2")
        (is (equalp (chacha20-poly1305-decrypt key nonce ct aad tag) pt)
            "AEAD_CHACHA20_POLY1305 decrypt recovers the RFC plaintext")
        (let ((bad-ct (copy-seq ct)))
          (setf (aref bad-ct 0) (logxor (aref bad-ct 0) 1))
          (is (handler-case (progn (chacha20-poly1305-decrypt key nonce bad-ct aad tag) nil)
                (gcm-auth-error () t))
              "AEAD_CHACHA20_POLY1305 decrypt signals GCM-AUTH-ERROR on a tampered ciphertext byte"))))))

;;;; --- entry point ------------------------------------------------------

(defun run-all-tests ()
  (reset-counters)
  (format t "~%running rosette-symmetric-crypto tests...~%")
  (test-sha256)
  (test-hmac)
  (test-hkdf)
  (test-aes-ecb)
  (test-aes-ctr)
  (test-aes-gcm)
  (test-chacha20)
  (test-poly1305)
  (test-chacha20-poly1305)
  (format t "~%~A test assertions, ~A failures.~%"
          *test-count* *failure-count*)
  (when (plusp *failure-count*)
    (error "rosette-symmetric-crypto tests failed: ~A failures" *failure-count*))
  (values *test-count* *failure-count*))
