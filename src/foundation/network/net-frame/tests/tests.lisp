;;;; tests.lisp --- tests for rosette-net-frame.
;;;;
;;;; Gate strategy: every checksum and CRC is validated against PUBLISHED
;;;; standard test vectors (an independent oracle), build/parse are shown to
;;;; be exact inverses (round-trip), and deliberate single-byte corruption is
;;;; shown to be caught by the checksum.

(defpackage #:rosette-net-frame/tests
  (:use #:cl #:rosette-net-frame)
  (:import-from #:rosette-byte-core #:make-byte-vector #:coerce-byte-vector)
  (:export #:run-all-tests))

(in-package #:rosette-net-frame/tests)

(defun bv (&rest octets)
  (coerce-byte-vector (coerce octets 'vector)))

(defun run-all-tests ()
  (let ((assertions 0))
    (flet ((ok (condition message)
             (incf assertions)
             (unless condition
               (error "Assertion failed: ~A" message))))

      ;; ---------------------------------------------------------------
      ;; Big-endian octet helpers round-trip.
      ;; ---------------------------------------------------------------
      (let ((v (make-byte-vector 8)))
        (put-u16be v 0 #xdead)
        (ok (= (get-u16be v 0) #xdead) "u16be round-trips")
        (ok (and (= (aref v 0) #xde) (= (aref v 1) #xad))
            "u16be is big-endian on the wire")
        (put-u32be v 4 #xdeadbeef)
        (ok (= (get-u32be v 4) #xdeadbeef) "u32be round-trips")
        (ok (and (= (aref v 4) #xde) (= (aref v 7) #xef))
            "u32be is big-endian on the wire"))

      ;; ---------------------------------------------------------------
      ;; CRC-32 (IEEE 802.3) against published vectors.
      ;; ---------------------------------------------------------------
      (ok (= (crc32 "") 0)
          "CRC32 of empty input is 0")
      (ok (= (crc32 "123456789") #xcbf43926)
          "CRC32 check vector 0xCBF43926 (the canonical CRC-32 check value)")
      (ok (= (crc32 "a") #xe8b7be43)
          "CRC32 of \"a\" = 0xE8B7BE43")
      (ok (= (crc32 "The quick brown fox jumps over the lazy dog") #x414fa339)
          "CRC32 of the pangram = 0x414FA339")
      ;; chunked continuation equals whole-buffer CRC
      (ok (= (crc32 "56789" :init (crc32 "1234")) #xcbf43926)
          "CRC32 chunked via :init matches the whole-buffer CRC")

      ;; ---------------------------------------------------------------
      ;; Internet checksum (RFC 1071) against the canonical IPv4 header.
      ;; 45 00 00 73 00 00 40 00 40 11 00 00 c0 a8 00 01 c0 a8 00 c7
      ;; with the checksum field zeroed -> 0xB861.
      ;; ---------------------------------------------------------------
      (let ((hdr (bv #x45 #x00 #x00 #x73 #x00 #x00 #x40 #x00
                     #x40 #x11 #x00 #x00 #xc0 #xa8 #x00 #x01
                     #xc0 #xa8 #x00 #xc7)))
        (ok (= (internet-checksum hdr) #xb861)
            "IPv4 internet checksum of the RFC/Wikipedia header = 0xB861")
        ;; insert the checksum and the folded result must be 0
        (put-u16be hdr 10 #xb861)
        (ok (= (internet-checksum hdr) 0)
            "internet checksum over a header carrying its own checksum folds to 0")
        (ok (ipv4-header-valid-p hdr) "ipv4-header-valid-p on a correct header"))

      ;; RFC 1071 worked example: bytes 00 01 f2 03 f4 f5 f6 f7 -> 0x220d
      (ok (= (internet-checksum (bv #x00 #x01 #xf2 #x03 #xf4 #xf5 #xf6 #xf7))
             #x220d)
          "internet checksum of the RFC 1071 worked example = 0x220D")

      ;; ---------------------------------------------------------------
      ;; IPv4 addressing helpers.
      ;; ---------------------------------------------------------------
      (ok (= (ipv4-address 192 168 0 1) #xc0a80001) "ipv4-address packs octets")
      (ok (equal (ipv4-octets #xc0a80001) '(192 168 0 1)) "ipv4-octets unpacks")
      (ok (string= (ipv4-string #xc0a800c7) "192.168.0.199") "ipv4-string dots")
      (ok (= (ipv4-address 10 20 30 40)
             (apply #'ipv4-address (ipv4-octets (ipv4-address 10 20 30 40))))
          "ipv4-address/ipv4-octets are inverses")

      ;; ---------------------------------------------------------------
      ;; IPv4 header build == canonical bytes, and build->parse round-trip.
      ;; ---------------------------------------------------------------
      (let* ((h (make-ipv4-header
                 :version 4 :ihl 5 :dscp 0 :ecn 0
                 :total-length #x0073 :identification 0
                 :flags 2 :fragment-offset 0
                 :ttl #x40 :protocol #x11
                 :source (ipv4-address 192 168 0 1)
                 :destination (ipv4-address 192 168 0 199)))
             (bytes (build-ipv4-header h)))
        (ok (= (get-u16be bytes 10) #xb861)
            "build-ipv4-header inserts the correct checksum 0xB861")
        (ok (equalp bytes
                    (bv #x45 #x00 #x00 #x73 #x00 #x00 #x40 #x00
                        #x40 #x11 #xb8 #x61 #xc0 #xa8 #x00 #x01
                        #xc0 #xa8 #x00 #xc7))
            "build-ipv4-header reproduces the canonical header byte-for-byte")
        (ok (ipv4-header-valid-p bytes) "built header validates")
        (let ((p (parse-ipv4-header bytes)))
          (ok (= (ipv4-header-version p) 4) "parse: version")
          (ok (= (ipv4-header-ihl p) 5) "parse: ihl")
          (ok (= (ipv4-header-total-length p) #x0073) "parse: total-length")
          (ok (= (ipv4-header-flags p) 2) "parse: flags (DF)")
          (ok (= (ipv4-header-fragment-offset p) 0) "parse: fragment-offset")
          (ok (= (ipv4-header-ttl p) #x40) "parse: ttl")
          (ok (= (ipv4-header-protocol p) #x11) "parse: protocol")
          (ok (= (ipv4-header-checksum p) #xb861) "parse: checksum")
          (ok (= (ipv4-header-source p) (ipv4-address 192 168 0 1)) "parse: source")
          (ok (= (ipv4-header-destination p) (ipv4-address 192 168 0 199))
              "parse: destination")
          (ok (= (get-u16be (build-ipv4-header p) 10) #xb861)
              "parse then rebuild reproduces the checksum")))

      ;; corrupt one byte -> checksum no longer validates
      (let ((bytes (build-ipv4-header
                    (make-ipv4-header :total-length 40 :ttl 64 :protocol 6
                                      :source (ipv4-address 10 0 0 1)
                                      :destination (ipv4-address 10 0 0 2)))))
        (ok (ipv4-header-valid-p bytes) "fresh header validates before corruption")
        (setf (aref bytes 8) (logxor #x01 (aref bytes 8)))  ; flip a TTL bit
        (ok (not (ipv4-header-valid-p bytes))
            "single-byte corruption is caught by the IPv4 checksum"))

      ;; ---------------------------------------------------------------
      ;; Ethernet II frame build/parse round-trip + FCS validation.
      ;; ---------------------------------------------------------------
      (let* ((dst (mac-address #x00 #x11 #x22 #x33 #x44 #x55))
             (src (mac-address #xaa #xbb #xcc #xdd #xee #xff))
             (payload (coerce-byte-vector "hello, ethernet"))
             (frame (build-eth-frame
                     (make-eth-frame :destination dst :source src
                                     :ethertype #x0800 :payload payload))))
        (ok (= (length frame) (+ 6 6 2 (length payload) 4))
            "eth frame length = dst+src+type+payload+fcs")
        (ok (eth-frame-valid-p frame) "eth-frame FCS validates")
        (let ((p (parse-eth-frame frame)))
          (ok (equalp (eth-frame-destination p) dst) "parse eth: destination MAC")
          (ok (equalp (eth-frame-source p) src) "parse eth: source MAC")
          (ok (= (eth-frame-ethertype p) #x0800) "parse eth: ethertype")
          (ok (equalp (eth-frame-payload p) payload) "parse eth: payload")
          (ok (= (eth-frame-fcs p) (crc32 frame :end (- (length frame) 4)))
              "parse eth: FCS equals CRC over the frame body"))
        ;; corrupt a payload byte -> FCS fails
        (let ((bad (copy-seq frame)))
          (setf (aref bad 16) (logxor #xff (aref bad 16)))
          (ok (not (eth-frame-valid-p bad))
              "single-byte payload corruption is caught by the Ethernet FCS")))

      ;; ---------------------------------------------------------------
      ;; Whole vertical: frame-ipv4 / deframe-ipv4 exact round trip.
      ;; ---------------------------------------------------------------
      (let* ((dst (mac-address #xde #xad #xbe #xef #x00 #x01))
             (src (mac-address #xde #xad #xbe #xef #x00 #x02))
             (l4 (coerce-byte-vector "PING payload 12345"))
             (ip-in (make-ipv4-header :ttl 64 :protocol 1 :identification #x1234
                                      :flags 2
                                      :source (ipv4-address 172 16 5 4)
                                      :destination (ipv4-address 172 16 5 9)))
             (wire (frame-ipv4 ip-in l4 dst src)))
        (ok (eth-frame-valid-p wire) "framed IPv4 packet has a valid FCS")
        (ok (ipv4-header-valid-p (eth-frame-payload (parse-eth-frame wire)))
            "framed IPv4 header inside the frame has a valid checksum")
        (multiple-value-bind (frame ip payload) (deframe-ipv4 wire)
          (ok (equalp (eth-frame-destination frame) dst) "deframe: dst MAC")
          (ok (equalp (eth-frame-source frame) src) "deframe: src MAC")
          (ok (= (eth-frame-ethertype frame) #x0800) "deframe: ethertype IPv4")
          (ok (= (ipv4-header-ttl ip) 64) "deframe: ttl preserved")
          (ok (= (ipv4-header-protocol ip) 1) "deframe: protocol preserved")
          (ok (= (ipv4-header-identification ip) #x1234) "deframe: id preserved")
          (ok (= (ipv4-header-source ip) (ipv4-address 172 16 5 4))
              "deframe: source IP preserved")
          (ok (= (ipv4-header-destination ip) (ipv4-address 172 16 5 9))
              "deframe: destination IP preserved")
          (ok (= (ipv4-header-total-length ip) (+ 20 (length l4)))
              "deframe: total-length = 20 + payload")
          (ok (equalp payload l4) "deframe: L4 payload byte-identical"))
        ;; corrupt one byte anywhere in the wire frame -> a checksum catches it
        (let ((bad (copy-seq wire)))
          (setf (aref bad 20) (logxor #x80 (aref bad 20)))
          (ok (not (eth-frame-valid-p bad))
              "corruption anywhere in the wire frame breaks the FCS"))))

    (format t "rosette-net-frame: ~D assertions, 0 failures~%" assertions)
    assertions))
