;;;; wire-protocol.lisp --- bounded Cells and Frames over an octet stream.

(in-package #:rosette-wire-protocol)

(defconstant +wire-version+ 1)
(defconstant +cell-header-length+ 16)
(defconstant +frame-header-length+ 60)
(defparameter +cell-magic+ #(82 83 67 49)) ; RSC1
(defparameter +wire-magic+ #(82 83 87 49)) ; RSW1

(define-condition wire-protocol-error (error)
  ((reason :initarg :reason :reader wire-protocol-error-reason)
   (offset :initarg :offset :initform 0 :reader wire-protocol-error-offset)
   (detail :initarg :detail :initform "" :reader wire-protocol-error-detail))
  (:report (lambda (condition stream)
             (format stream "Wire protocol ~A at octet ~D: ~A"
                     (wire-protocol-error-reason condition)
                     (wire-protocol-error-offset condition)
                     (wire-protocol-error-detail condition)))))

(defun %fail (reason offset control &rest arguments)
  (error 'wire-protocol-error :reason reason :offset offset
                              :detail (apply #'format nil control arguments)))

(deftype octet-vector () '(simple-array (unsigned-byte 8) (*)))

(defun %octets (value &optional (offset 0))
  (unless (and (vectorp value)
               (every (lambda (byte) (typep byte '(unsigned-byte 8))) value))
    (%fail :invalid-octets offset "expected a vector of octets"))
  (let ((copy (make-byte-vector (length value))))
    (replace copy value)
    copy))

(defun %ascii-octets (string field)
  (unless (and (stringp string) (plusp (length string)))
    (%fail :invalid-cell 0 "~A must be a non-empty string" field))
  (let ((bytes (make-byte-vector (length string))))
    (dotimes (index (length string) bytes)
      (let ((code (char-code (char string index))))
        (unless (<= #x21 code #x7e)
          (%fail :invalid-cell index
                 "~A must contain only visible ASCII" field))
        (setf (aref bytes index) code)))))

(defun %ascii-string (bytes start end field)
  (let ((string (make-string (- end start))))
    (loop for source from start below end
          for target from 0
          for code = (aref bytes source)
          do (unless (<= #x21 code #x7e)
               (%fail :invalid-cell source
                      "~A contains a non-visible-ASCII octet" field))
             (setf (char string target) (code-char code)))
    string))

(defun %write-uint (value bytes offset width)
  (unless (and (integerp value) (<= 0 value (1- (ash 1 (* 8 width)))))
    (%fail :integer-range offset "~S does not fit in ~D octets" value width))
  (dotimes (index width bytes)
    (setf (aref bytes (+ offset index))
          (ldb (byte 8 (* 8 (- width index 1))) value))))

(defun %read-uint (bytes offset width)
  (loop with value = 0
        for index from offset below (+ offset width)
        do (setf value (+ (ash value 8) (aref bytes index)))
        finally (return value)))

(defun %magic-p (bytes offset magic)
  (and (<= (+ offset (length magic)) (length bytes))
       (loop for index below (length magic)
             always (= (aref bytes (+ offset index)) (aref magic index)))))

(defstruct (cell (:constructor %make-cell (tag media-type payload id))
                 (:conc-name %cell-))
  (tag "" :type string :read-only t)
  (media-type "" :type string :read-only t)
  (payload (make-byte-vector 0) :type octet-vector :read-only t)
  (id "" :type string :read-only t))

(defun cell-tag (cell) (copy-seq (%cell-tag cell)))
(defun cell-media-type (cell) (copy-seq (%cell-media-type cell)))
(defun cell-payload (cell) (%octets (%cell-payload cell)))
(defun cell-id (cell) (copy-seq (%cell-id cell)))

(defun %encode-cell-fields (tag media-type payload)
  (let* ((tag-bytes (%ascii-octets tag "cell tag"))
         (media-bytes (%ascii-octets media-type "cell media type"))
         (tag-length (length tag-bytes))
         (media-length (length media-bytes))
         (payload-length (length payload)))
    (when (> tag-length #xffff)
      (%fail :cell-tag-limit 4 "cell tag exceeds 65535 octets"))
    (when (> media-length #xffff)
      (%fail :cell-media-type-limit 6 "cell media type exceeds 65535 octets"))
    (let ((bytes (make-byte-vector
                  (+ +cell-header-length+ tag-length media-length
                     payload-length))))
      (replace bytes +cell-magic+)
      (%write-uint tag-length bytes 4 2)
      (%write-uint media-length bytes 6 2)
      (%write-uint payload-length bytes 8 8)
      (replace bytes tag-bytes :start1 +cell-header-length+)
      (replace bytes media-bytes
               :start1 (+ +cell-header-length+ tag-length))
      (replace bytes payload
               :start1 (+ +cell-header-length+ tag-length media-length))
      bytes)))

(defun make-cell (&key tag media-type payload id)
  "Construct an immutable tagged Cell. Supplied ID must match its exact bytes."
  (let* ((payload (%octets payload))
         (encoded (%encode-cell-fields tag media-type payload))
         (actual-id (format nil "sha256:~A" (sha256-hex encoded))))
    (when (and id (not (string= id actual-id)))
      (%fail :cell-id-mismatch 0 "supplied ~S, computed ~S" id actual-id))
    (%make-cell (copy-seq tag) (copy-seq media-type) payload actual-id)))

(defun encode-cell (cell)
  (check-type cell cell)
  (%encode-cell-fields (%cell-tag cell) (%cell-media-type cell)
                       (%cell-payload cell)))

(defun decode-cell (bytes &key (max-cell-bytes (* 16 1024 1024)))
  "Decode exactly one Cell, checking its declared size before body copying."
  (let ((bytes (%octets bytes)))
    (when (< (length bytes) +cell-header-length+)
      (%fail :truncated-cell (length bytes) "cell header is incomplete"))
    (unless (%magic-p bytes 0 +cell-magic+)
      (%fail :bad-cell-magic 0 "expected RSC1"))
    (let* ((tag-length (%read-uint bytes 4 2))
           (media-length (%read-uint bytes 6 2))
           (payload-length (%read-uint bytes 8 8))
           (total (+ +cell-header-length+ tag-length media-length
                     payload-length)))
      (when (> total max-cell-bytes)
        (%fail :cell-size-limit 8 "declared Cell size ~D exceeds ~D"
               total max-cell-bytes))
      (unless (= total (length bytes))
        (%fail (if (< (length bytes) total) :truncated-cell
                   :trailing-cell-bytes)
               (min total (length bytes)) "declared ~D octets, received ~D"
               total (length bytes)))
      (let* ((tag-end (+ +cell-header-length+ tag-length))
             (media-end (+ tag-end media-length))
             (tag (%ascii-string bytes +cell-header-length+ tag-end "cell tag"))
             (media (%ascii-string bytes tag-end media-end "cell media type"))
             (payload (subseq bytes media-end total)))
        (make-cell :tag tag :media-type media :payload payload)))))

(defparameter +frame-kind-codes+
  '((:cell . 1) (:blob . 2) (:end . 3) (:error . 4)))

(defun %kind-code (kind)
  (or (cdr (assoc kind +frame-kind-codes+))
      (%fail :unknown-frame-kind 5 "unknown frame kind ~S" kind)))

(defun %code-kind (code)
  (or (car (rassoc code +frame-kind-codes+))
      (%fail :unknown-frame-kind 5 "unknown frame kind code ~D" code)))

(defstruct (frame
             (:constructor %make-frame
                 (kind flags stream-id sequence payload payload-id))
             (:conc-name %frame-))
  (kind :cell :type keyword :read-only t)
  (flags 0 :type (unsigned-byte 16) :read-only t)
  (stream-id 0 :type (unsigned-byte 32) :read-only t)
  (sequence 0 :type (unsigned-byte 64) :read-only t)
  (payload (make-byte-vector 0) :type octet-vector :read-only t)
  (payload-id "" :type string :read-only t))

(defun frame-kind (frame) (%frame-kind frame))
(defun frame-flags (frame) (%frame-flags frame))
(defun frame-stream-id (frame) (%frame-stream-id frame))
(defun frame-sequence (frame) (%frame-sequence frame))
(defun frame-payload (frame) (%octets (%frame-payload frame)))
(defun frame-payload-id (frame) (copy-seq (%frame-payload-id frame)))

(defun make-frame (&key kind (flags 0) (stream-id 0) (sequence 0) payload)
  "Construct one immutable v1 Frame. V1 flags are reserved and therefore zero."
  (%kind-code kind)
  (unless (zerop flags) (%fail :reserved-flags 6 "v1 flags must be zero"))
  (unless (typep stream-id '(unsigned-byte 32))
    (%fail :stream-id-range 8 "stream id does not fit u32"))
  (unless (typep sequence '(unsigned-byte 64))
    (%fail :sequence-range 12 "sequence does not fit u64"))
  (let ((payload (%octets payload)))
    (when (and (eq kind :end) (plusp (length payload)))
      (%fail :end-payload 28 "an END frame must have an empty payload"))
    (%make-frame kind flags stream-id sequence payload
                 (format nil "sha256:~A" (sha256-hex payload)))))

(defun make-cell-frame (cell &key (stream-id 0) (sequence 0))
  (check-type cell cell)
  (make-frame :kind :cell :stream-id stream-id :sequence sequence
              :payload (encode-cell cell)))

(defun encode-frame (frame)
  (check-type frame frame)
  (let* ((payload (%frame-payload frame))
         (length (length payload))
         (bytes (make-byte-vector (+ +frame-header-length+ length))))
    (replace bytes +wire-magic+)
    (setf (aref bytes 4) +wire-version+
          (aref bytes 5) (%kind-code (%frame-kind frame)))
    (%write-uint (%frame-flags frame) bytes 6 2)
    (%write-uint (%frame-stream-id frame) bytes 8 4)
    (%write-uint (%frame-sequence frame) bytes 12 8)
    (%write-uint length bytes 20 8)
    (replace bytes (sha256 payload) :start1 28)
    (replace bytes payload :start1 +frame-header-length+)
    bytes))

(defun %declared-frame-length (header max-frame-bytes)
  (unless (%magic-p header 0 +wire-magic+)
    (%fail :bad-frame-magic 0 "expected RSW1"))
  (unless (= (aref header 4) +wire-version+)
    (%fail :unsupported-version 4 "expected 1, received ~D" (aref header 4)))
  (%code-kind (aref header 5))
  (unless (zerop (%read-uint header 6 2))
    (%fail :reserved-flags 6 "v1 flags must be zero"))
  (let ((payload-length (%read-uint header 20 8)))
    (when (> payload-length max-frame-bytes)
      (%fail :frame-size-limit 20 "declared payload ~D exceeds ~D"
             payload-length max-frame-bytes))
    (+ +frame-header-length+ payload-length)))

(defun decode-frame (bytes &key (max-frame-bytes (* 16 1024 1024)))
  "Decode exactly one Frame and verify its payload digest."
  (let ((bytes (%octets bytes)))
    (when (< (length bytes) +frame-header-length+)
      (%fail :truncated-frame (length bytes) "frame header is incomplete"))
    (let ((total (%declared-frame-length bytes max-frame-bytes)))
      (unless (= total (length bytes))
        (%fail (if (< (length bytes) total) :truncated-frame
                   :trailing-frame-bytes)
               (min total (length bytes)) "declared ~D octets, received ~D"
               total (length bytes)))
      (let* ((payload (subseq bytes +frame-header-length+ total))
             (expected (subseq bytes 28 +frame-header-length+)))
        (unless (equalp expected (sha256 payload))
          (%fail :payload-digest-mismatch 28 "payload SHA-256 does not match"))
        (make-frame :kind (%code-kind (aref bytes 5))
                    :flags (%read-uint bytes 6 2)
                    :stream-id (%read-uint bytes 8 4)
                    :sequence (%read-uint bytes 12 8)
                    :payload payload)))))

(defstruct (wire-decoder
             (:constructor %make-wire-decoder
                 (max-frame-bytes max-feed-bytes max-frames-per-feed
                  strict-sequence-p buffer))
             (:conc-name %decoder-))
  (max-frame-bytes (* 16 1024 1024) :type (integer 0 *) :read-only t)
  (max-feed-bytes (* 64 1024) :type (integer 1 *) :read-only t)
  (max-frames-per-feed 64 :type (integer 1 *) :read-only t)
  (strict-sequence-p t :type boolean :read-only t)
  (buffer #() :type vector)
  (fill 0 :type (integer 0 *))
  (expected-total nil :type (or null integer))
  (next-sequences (make-hash-table :test #'equal) :read-only t)
  (closed-streams (make-hash-table :test #'equal) :read-only t)
  (fault nil :type (or null wire-protocol-error)))

(defun %empty-buffer ()
  (make-array +frame-header-length+ :element-type '(unsigned-byte 8)
              :adjustable t))

(defun make-wire-decoder (&key (max-frame-bytes (* 16 1024 1024))
                                (max-feed-bytes (* 64 1024))
                                (max-frames-per-feed 64)
                                (strict-sequence-p t))
  (unless (and (integerp max-frame-bytes) (not (minusp max-frame-bytes))
               (integerp max-feed-bytes) (plusp max-feed-bytes)
               (integerp max-frames-per-feed) (plusp max-frames-per-feed))
    (%fail :invalid-limit 0 "wire limits are invalid"))
  (%make-wire-decoder max-frame-bytes max-feed-bytes max-frames-per-feed
                      strict-sequence-p (%empty-buffer)))

(defun wire-decoder-fault (decoder) (%decoder-fault decoder))
(defun wire-decoder-buffered-bytes (decoder) (%decoder-fill decoder))

(defun %terminalize (decoder condition)
  (setf (%decoder-fault decoder) condition
        (%decoder-buffer decoder) (%empty-buffer)
        (%decoder-fill decoder) 0
        (%decoder-expected-total decoder) nil)
  condition)

(defun %check-sequence (decoder frame)
  (when (%decoder-strict-sequence-p decoder)
    (let* ((stream (%frame-stream-id frame))
           (expected (gethash stream (%decoder-next-sequences decoder) 0)))
      (when (gethash stream (%decoder-closed-streams decoder))
        (%fail :closed-stream 8 "stream ~D is already closed" stream))
      (unless (= (%frame-sequence frame) expected)
        (%fail :sequence 12 "stream ~D expected sequence ~D, received ~D"
               stream expected (%frame-sequence frame)))
      (setf (gethash stream (%decoder-next-sequences decoder)) (1+ expected))
      (when (eq (%frame-kind frame) :end)
        (setf (gethash stream (%decoder-closed-streams decoder)) t)))))

(defun feed-wire-decoder (decoder bytes)
  "Consume one bounded chunk and return complete Frames in wire order.

Arbitrary fragmentation and coalescing are accepted. Any fault is terminal."
  (check-type decoder wire-decoder)
  (when (%decoder-fault decoder) (error (%decoder-fault decoder)))
  (let ((bytes (%octets bytes)))
    (when (> (length bytes) (%decoder-max-feed-bytes decoder))
      (let ((fault (make-condition
                    'wire-protocol-error :reason :feed-size-limit :offset 0
                    :detail (format nil "feed ~D exceeds ~D" (length bytes)
                                    (%decoder-max-feed-bytes decoder)))))
        (%terminalize decoder fault)
        (error fault)))
    (handler-case
        (let ((frames nil))
          (dotimes (source (length bytes))
            (when (and (>= (length frames) (%decoder-max-frames-per-feed decoder))
                       (zerop (%decoder-fill decoder)))
              (%fail :frames-per-feed-limit source "feed exceeds ~D frames"
                     (%decoder-max-frames-per-feed decoder)))
            (setf (aref (%decoder-buffer decoder) (%decoder-fill decoder))
                  (aref bytes source))
            (incf (%decoder-fill decoder))
            (when (= (%decoder-fill decoder) +frame-header-length+)
              (let ((total (%declared-frame-length
                            (%decoder-buffer decoder)
                            (%decoder-max-frame-bytes decoder))))
                (setf (%decoder-expected-total decoder) total)
                (adjust-array (%decoder-buffer decoder) total)))
            (when (and (%decoder-expected-total decoder)
                       (= (%decoder-fill decoder) (%decoder-expected-total decoder)))
              (let ((frame (decode-frame
                            (%decoder-buffer decoder)
                            :max-frame-bytes (%decoder-max-frame-bytes decoder))))
                (%check-sequence decoder frame)
                (push frame frames))
              (setf (%decoder-buffer decoder) (%empty-buffer)
                    (%decoder-fill decoder) 0
                    (%decoder-expected-total decoder) nil)))
          (nreverse frames))
      (wire-protocol-error (condition)
        (%terminalize decoder condition)
        (error condition)))))
