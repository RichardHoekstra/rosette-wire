;;;; core.lisp --- Ethernet II / IPv4 framing + checksums (CRC32, internet checksum).
;;;;
;;;; The network OS-vertical's data-path kernel.  One immutable byte-vector IR
;;;; (the wire frame) is joined to structured header IRs by pure, oracle-gated
;;;; transforms: build (fields -> bytes) and parse (bytes -> fields) are exact
;;;; inverses, and the checksums are independent oracles that certify a frame
;;;; was not corrupted in transit.

(in-package #:rosette-net-frame)

;;; -------------------------------------------------------------------------
;;; Big-endian (network byte order) octet accessors.
;;; -------------------------------------------------------------------------

(declaim (inline get-u16be get-u32be))

(defun get-u16be (vec off)
  "Read a big-endian unsigned 16-bit integer from VEC at OFF."
  (logior (ash (aref vec off) 8)
          (aref vec (+ off 1))))

(defun put-u16be (vec off val)
  "Write VAL as a big-endian unsigned 16-bit integer into VEC at OFF."
  (setf (aref vec off) (logand #xff (ash val -8))
        (aref vec (+ off 1)) (logand #xff val))
  val)

(defun get-u32be (vec off)
  "Read a big-endian unsigned 32-bit integer from VEC at OFF."
  (logior (ash (aref vec off) 24)
          (ash (aref vec (+ off 1)) 16)
          (ash (aref vec (+ off 2)) 8)
          (aref vec (+ off 3))))

(defun put-u32be (vec off val)
  "Write VAL as a big-endian unsigned 32-bit integer into VEC at OFF."
  (setf (aref vec off) (logand #xff (ash val -24))
        (aref vec (+ off 1)) (logand #xff (ash val -16))
        (aref vec (+ off 2)) (logand #xff (ash val -8))
        (aref vec (+ off 3)) (logand #xff val))
  val)

;;; -------------------------------------------------------------------------
;;; Internet checksum (RFC 1071): the IPv4 header one's-complement checksum.
;;; -------------------------------------------------------------------------

(defun ones-complement-sum (bytes &key (start 0) (end nil))
  "Return the folded 16-bit one's-complement sum of the octet range
[START, END) of BYTES.  Octets are grouped big-endian into 16-bit words;
an odd trailing octet is padded with a zero low byte.  Carries are folded
back in.  This is the sum PRIOR to the final one's complement."
  (let* ((b (coerce-byte-vector bytes))
         (end (or end (length b)))
         (sum 0)
         (i start))
    (declare (type (unsigned-byte 32) sum))
    (loop while (< (+ i 1) end)
          do (incf sum (get-u16be b i))
             (incf i 2))
    (when (< i end)                     ; odd trailing octet
      (incf sum (ash (aref b i) 8)))
    ;; fold 32-bit accumulator down to 16 bits
    (loop while (> sum #xffff)
          do (setf sum (+ (logand sum #xffff) (ash sum -16))))
    sum))

(defun internet-checksum (bytes &key (start 0) (end nil))
  "Return the 16-bit internet checksum (RFC 1071) of the octet range
[START, END) of BYTES: the one's complement of the folded one's-complement
sum.  Computed over a header whose checksum field is zero it yields the
value to insert; computed over a header carrying its correct checksum it
yields 0."
  (logand #xffff (lognot (ones-complement-sum bytes :start start :end end))))

;;; -------------------------------------------------------------------------
;;; CRC32 (IEEE 802.3 / Ethernet FCS): reflected, poly 0xEDB88320,
;;; init 0xFFFFFFFF, final XOR 0xFFFFFFFF.
;;; -------------------------------------------------------------------------

(defparameter crc32-table
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c)
                      (logxor #xedb88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c))))
  "Precomputed reflected CRC-32 (IEEE 802.3) lookup table, poly 0xEDB88320.")

(defun crc32 (bytes &key (start 0) (end nil) (init 0))
  "Return the IEEE 802.3 CRC-32 (Ethernet FCS) of the octet range
[START, END) of BYTES.  INIT lets a CRC be continued across chunks (pass a
previously returned value); it defaults to a fresh CRC.  Result is a 32-bit
unsigned integer."
  (let* ((b (coerce-byte-vector bytes))
         (end (or end (length b)))
         (crc (logxor #xffffffff (logand #xffffffff init))))
    (declare (type (unsigned-byte 32) crc))
    (loop for i from start below end
          do (setf crc (logxor (aref crc32-table
                                     (logand #xff (logxor crc (aref b i))))
                               (ash crc -8))))
    (logxor #xffffffff crc)))

;;; -------------------------------------------------------------------------
;;; IPv4 addressing.
;;; -------------------------------------------------------------------------

(defun ipv4-address (a b c d)
  "Return the 32-bit integer for the dotted quad A.B.C.D."
  (logior (ash (logand #xff a) 24)
          (ash (logand #xff b) 16)
          (ash (logand #xff c) 8)
          (logand #xff d)))

(defun ipv4-octets (addr)
  "Return the four octets of the 32-bit IPv4 integer ADDR as a list."
  (list (logand #xff (ash addr -24))
        (logand #xff (ash addr -16))
        (logand #xff (ash addr -8))
        (logand #xff addr)))

(defun ipv4-string (addr)
  "Return the dotted-quad string of the 32-bit IPv4 integer ADDR."
  (format nil "~{~D~^.~}" (ipv4-octets addr)))

;;; -------------------------------------------------------------------------
;;; IPv4 header (20-byte, no options).
;;; -------------------------------------------------------------------------

(defstruct (ipv4-header (:constructor make-ipv4-header))
  (version 4)
  (ihl 5)
  (dscp 0)
  (ecn 0)
  (total-length 0)
  (identification 0)
  (flags 0)                             ; 3 bits: bit1 DF, bit0 MF
  (fragment-offset 0)                   ; 13 bits
  (ttl 64)
  (protocol 0)                          ; 1=ICMP, 6=TCP, 17=UDP
  (checksum 0)                          ; 0 => filled by build-ipv4-header
  (source 0)                            ; 32-bit integer
  (destination 0))                      ; 32-bit integer

(defun build-ipv4-header (header)
  "Serialize the IPV4-HEADER struct into a fresh 20-octet BYTE-VECTOR,
computing and inserting the header checksum (the struct's CHECKSUM slot is
ignored on build; use PARSE-IPV4-HEADER to read it back)."
  (let ((v (make-byte-vector 20)))
    (setf (aref v 0) (logior (ash (logand #xf (ipv4-header-version header)) 4)
                             (logand #xf (ipv4-header-ihl header)))
          (aref v 1) (logior (ash (logand #x3f (ipv4-header-dscp header)) 2)
                             (logand #x3 (ipv4-header-ecn header))))
    (put-u16be v 2 (ipv4-header-total-length header))
    (put-u16be v 4 (ipv4-header-identification header))
    (put-u16be v 6 (logior (ash (logand #x7 (ipv4-header-flags header)) 13)
                           (logand #x1fff (ipv4-header-fragment-offset header))))
    (setf (aref v 8) (logand #xff (ipv4-header-ttl header))
          (aref v 9) (logand #xff (ipv4-header-protocol header)))
    (put-u16be v 10 0)                  ; checksum field zero for computation
    (put-u32be v 12 (ipv4-header-source header))
    (put-u32be v 16 (ipv4-header-destination header))
    (put-u16be v 10 (internet-checksum v :start 0 :end 20))
    v))

(defun parse-ipv4-header (bytes &key (start 0))
  "Parse a 20-octet IPv4 header from BYTES at START into an IPV4-HEADER
struct (the CHECKSUM slot carries the value read off the wire)."
  (let ((v (coerce-byte-vector bytes)))
    (make-ipv4-header
     :version (ash (aref v start) -4)
     :ihl (logand #xf (aref v start))
     :dscp (ash (aref v (+ start 1)) -2)
     :ecn (logand #x3 (aref v (+ start 1)))
     :total-length (get-u16be v (+ start 2))
     :identification (get-u16be v (+ start 4))
     :flags (ash (get-u16be v (+ start 6)) -13)
     :fragment-offset (logand #x1fff (get-u16be v (+ start 6)))
     :ttl (aref v (+ start 8))
     :protocol (aref v (+ start 9))
     :checksum (get-u16be v (+ start 10))
     :source (get-u32be v (+ start 12))
     :destination (get-u32be v (+ start 16)))))

(defun ipv4-header-valid-p (bytes &key (start 0))
  "True iff the 20-octet IPv4 header at START in BYTES carries a correct
internet checksum (the checksum over the header, its own field included,
folds to zero)."
  (zerop (internet-checksum bytes :start start :end (+ start 20))))

;;; -------------------------------------------------------------------------
;;; Ethernet II frame.
;;; -------------------------------------------------------------------------

(defun mac-address (a b c d e f)
  "Return a fresh 6-octet BYTE-VECTOR MAC address from six octets."
  (let ((v (make-byte-vector 6)))
    (setf (aref v 0) (logand #xff a) (aref v 1) (logand #xff b)
          (aref v 2) (logand #xff c) (aref v 3) (logand #xff d)
          (aref v 4) (logand #xff e) (aref v 5) (logand #xff f))
    v))

(defstruct (eth-frame (:constructor make-eth-frame))
  (destination (make-byte-vector 6))    ; 6-octet MAC
  (source (make-byte-vector 6))         ; 6-octet MAC
  (ethertype #x0800)                    ; 0x0800 = IPv4
  (payload (make-byte-vector 0))        ; byte-vector
  (fcs nil))                            ; 32-bit CRC, filled on parse

(defun build-eth-frame (frame &key (fcs t))
  "Serialize the ETH-FRAME struct into a fresh BYTE-VECTOR:
dst(6) src(6) ethertype(2) payload [fcs(4)].  When FCS is true (default)
the IEEE 802.3 CRC-32 over dst..payload is appended big-endian as the Frame
Check Sequence.  Header MAC fields are coerced via COERCE-BYTE-VECTOR."
  (let* ((dst (coerce-byte-vector (eth-frame-destination frame)))
         (src (coerce-byte-vector (eth-frame-source frame)))
         (pl (coerce-byte-vector (eth-frame-payload frame)))
         (body-len (+ 6 6 2 (length pl)))
         (v (make-byte-vector (+ body-len (if fcs 4 0)))))
    (replace v dst :start1 0 :end2 6)
    (replace v src :start1 6 :end2 6)
    (put-u16be v 12 (eth-frame-ethertype frame))
    (replace v pl :start1 14)
    (when fcs
      (put-u32be v body-len (crc32 v :start 0 :end body-len)))
    v))

(defun parse-eth-frame (bytes &key (start 0) (end nil) (fcs t))
  "Parse an Ethernet II frame from the octet range [START, END) of BYTES.
When FCS is true (default) the trailing 4 octets are read as the CRC-32
Frame Check Sequence; the payload is everything between the ethertype and
the FCS.  Returns an ETH-FRAME struct."
  (let* ((v (coerce-byte-vector bytes))
         (end (or end (length v)))
         (body-end (if fcs (- end 4) end))
         (dst (subseq v start (+ start 6)))
         (src (subseq v (+ start 6) (+ start 12))))
    (make-eth-frame
     :destination dst
     :source src
     :ethertype (get-u16be v (+ start 12))
     :payload (subseq v (+ start 14) body-end)
     :fcs (when fcs (get-u32be v body-end)))))

(defun eth-frame-valid-p (bytes &key (start 0) (end nil))
  "True iff the Ethernet frame in [START, END) of BYTES carries a correct
FCS: the CRC-32 over dst..payload equals the trailing 4-octet FCS."
  (let* ((v (coerce-byte-vector bytes))
         (end (or end (length v)))
         (body-end (- end 4)))
    (= (crc32 v :start start :end body-end)
       (get-u32be v body-end))))

;;; -------------------------------------------------------------------------
;;; Whole network vertical: IPv4 packet <-> Ethernet frame, one round trip.
;;; -------------------------------------------------------------------------

(defun frame-ipv4 (ipv4-header payload dst-mac src-mac)
  "Build a complete Ethernet II frame (ethertype 0x0800) carrying an IPv4
packet: IPV4-HEADER (an ipv4-header struct) over PAYLOAD (a byte-vector),
with DST-MAC / SRC-MAC 6-octet MAC addresses.  TOTAL-LENGTH is set to
20 + |payload| before serialization.  Returns the wire BYTE-VECTOR."
  (let* ((pl (coerce-byte-vector payload))
         (hdr (copy-ipv4-header ipv4-header)))
    (setf (ipv4-header-total-length hdr) (+ 20 (length pl)))
    (let ((ip-bytes (build-ipv4-header hdr))
          (l3 (make-byte-vector (+ 20 (length pl)))))
      (replace l3 ip-bytes :start1 0)
      (replace l3 pl :start1 20)
      (build-eth-frame
       (make-eth-frame :destination dst-mac :source src-mac
                       :ethertype #x0800 :payload l3)
       :fcs t))))

(defun deframe-ipv4 (bytes &key (start 0) (end nil))
  "Inverse of FRAME-IPV4.  Parse an Ethernet II frame carrying IPv4 from
[START, END) of BYTES and return three values: the ETH-FRAME, the parsed
IPV4-HEADER, and the L4 PAYLOAD byte-vector (the IPv4 data after the 20-byte
header)."
  (let* ((frame (parse-eth-frame bytes :start start :end end :fcs t))
         (l3 (eth-frame-payload frame))
         (ip (parse-ipv4-header l3 :start 0))
         (payload (subseq l3 20)))
    (values frame ip payload)))
