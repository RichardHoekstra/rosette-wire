#.(progn (require :asdf) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((self-dir (make-pathname :defaults *load-pathname* :name nil :type nil))
         (root (loop for dir = self-dir
                     then (make-pathname :directory (butlast (pathname-directory dir)) :defaults dir)
                     while (cdr (pathname-directory dir))
                     when (probe-file (merge-pathnames ".rosette-wire-root" dir)) return dir)))
    (if root
        (asdf:initialize-source-registry `(:source-registry (:tree ,root) :ignore-inherited-configuration))
        (pushnew self-dir asdf:*central-registry* :test #'equal))))

(in-package :asdf-user)


(defsystem #:symmetric-crypto
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Symmetric cryptography: AES (ECB/CTR/GCM), ChaCha20-Poly1305, HMAC-SHA256, HKDF -- vector-verified against FIPS-197/RFC-8439/RFC-4231/RFC-5869."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "bytes") (:file "sha256") (:file "hmac")
   (:file "hkdf") (:file "aes") (:file "aes-ctr") (:file "gcm")
   (:file "chacha20") (:file "poly1305") (:file "chacha20-poly1305"))
  :in-order-to
  ((test-op (test-op #:symmetric-crypto/tests))))


(defsystem #:symmetric-crypto/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for symmetric-crypto (published-test-vector gate)."
  :depends-on
  (#:symmetric-crypto)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-symmetric-crypto/tests :run-all-tests)))
