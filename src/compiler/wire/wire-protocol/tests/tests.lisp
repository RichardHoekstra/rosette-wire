;;;; wire-protocol/tests/tests.lisp --- byte-exact protocol laws.

(defpackage #:wire-protocol/tests
  (:use #:cl #:rosette-wire-protocol)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:wire-protocol/tests)

(defparameter *assertions* 0)

(defun check (value control &rest arguments)
  (incf *assertions*)
  (assert-true value (apply #'format nil control arguments)))

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun protocol-error-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (wire-protocol-error (condition) (wire-protocol-error-reason condition))))

(defun fixture-cell ()
  (make-cell :tag "result" :media-type "application/json"
             :payload (octets 123 34 111 107 34 58 116 114 117 101 125)))

(defun test-cell ()
  (let* ((source (octets 1 2 3 255))
         (cell (make-cell :tag "blob" :media-type "application/octet-stream"
                          :payload source))
         (encoded (encode-cell cell))
         (decoded (decode-cell encoded)))
    (setf (aref source 0) 99)
    (check (= 1 (aref (cell-payload cell) 0)) "Cell retained caller mutation")
    (let ((copy (cell-payload cell)))
      (setf (aref copy 1) 99)
      (check (= 2 (aref (cell-payload cell) 1)) "Cell leaked mutable payload"))
    (check (equalp encoded (encode-cell decoded)) "Cell did not round-trip")
    (check (string= (cell-id cell) (cell-id decoded)) "Cell identity drifted")
    (check (string= "blob" (cell-tag decoded)) "Cell tag drifted")
    (check (eq :cell-id-mismatch
               (protocol-error-reason
                (lambda ()
                  (make-cell :tag "blob" :media-type "x/y"
                             :payload (octets 1) :id "sha256:wrong"))))
           "forged Cell identity was accepted")
    (check (eq :cell-size-limit
               (protocol-error-reason
                (lambda () (decode-cell encoded :max-cell-bytes 4))))
           "Cell ceiling was not enforced before decoding")))

(defun test-frame ()
  (let* ((cell (fixture-cell))
         (frame (make-cell-frame cell :stream-id 7 :sequence 9))
         (encoded (encode-frame frame))
         (decoded (decode-frame encoded)))
    (check (equalp +wire-magic+ (subseq encoded 0 4)) "wrong Wire magic")
    (check (= +wire-version+ (aref encoded 4)) "wrong Wire version")
    (check (= (+ +frame-header-length+ (length (encode-cell cell)))
              (length encoded))
           "Frame length is not header plus payload")
    (check (eq :cell (frame-kind decoded)) "Frame kind drifted")
    (check (= 7 (frame-stream-id decoded)) "stream id drifted")
    (check (= 9 (frame-sequence decoded)) "sequence drifted")
    (check (equalp (encode-cell cell) (frame-payload decoded))
           "Frame payload drifted")
    (let ((tampered (copy-seq encoded)))
      (incf (aref tampered (1- (length tampered))))
      (check (eq :payload-digest-mismatch
                 (protocol-error-reason (lambda () (decode-frame tampered))))
             "tampered payload passed its digest"))
    (check (eq :reserved-flags
               (protocol-error-reason
                (lambda () (make-frame :kind :blob :flags 1
                                       :payload (octets 1)))))
           "reserved v1 flags were accepted")
    (check (eq :end-payload
               (protocol-error-reason
                (lambda () (make-frame :kind :end :payload (octets 1)))))
           "END frame accepted a payload")))

(defun test-fragmentation-and-multiplexing ()
  (let* ((first (make-cell-frame (fixture-cell) :stream-id 3 :sequence 0))
         (second (make-frame :kind :blob :stream-id 9 :sequence 0
                             :payload (octets 10 20 30)))
         (third (make-frame :kind :end :stream-id 3 :sequence 1
                            :payload (octets)))
         (wire (concatenate '(simple-array (unsigned-byte 8) (*))
                            (encode-frame first) (encode-frame second)
                            (encode-frame third)))
         (decoder (make-wire-decoder :max-feed-bytes 1))
         (frames nil))
    (loop for byte across wire
          do (setf frames
                   (nconc frames (feed-wire-decoder decoder (octets byte)))))
    (check (= 3 (length frames)) "bytewise fragmentation lost Frames")
    (check (equal '((3 0) (9 0) (3 1))
                  (mapcar (lambda (frame)
                            (list (frame-stream-id frame)
                                  (frame-sequence frame)))
                          frames))
           "multiplexed stream order drifted")
    (check (zerop (wire-decoder-buffered-bytes decoder))
           "decoder retained bytes after complete Wire")))

(defun test-coalescing-and-faults ()
  (let* ((one (encode-frame (make-frame :kind :blob :stream-id 1 :sequence 0
                                        :payload (octets 1 2 3 4 5))))
         (end (encode-frame (make-frame :kind :end :stream-id 1 :sequence 1
                                        :payload (octets))))
         (both (concatenate '(simple-array (unsigned-byte 8) (*)) one end))
         (decoder (make-wire-decoder :max-feed-bytes (length both))))
    (check (= 2 (length (feed-wire-decoder decoder both)))
           "coalesced Frames were not both emitted"))
  (let* ((encoded (encode-frame
                   (make-frame :kind :blob :stream-id 2 :sequence 0
                               :payload (octets 1 2 3 4 5))))
         (decoder (make-wire-decoder :max-frame-bytes 4
                                     :max-feed-bytes +frame-header-length+)))
    (check (eq :frame-size-limit
               (protocol-error-reason
                (lambda ()
                  (feed-wire-decoder decoder
                                     (subseq encoded 0 +frame-header-length+)))))
           "declared oversize payload was not rejected at the header")
    (check (wire-decoder-fault decoder) "decoder did not terminalize")
    (check (eq :frame-size-limit
               (protocol-error-reason
                (lambda () (feed-wire-decoder decoder (octets)))))
           "terminal decoder did not preserve its first fault"))
  (let* ((wrong (encode-frame
                 (make-frame :kind :blob :stream-id 4 :sequence 1
                             :payload (octets 8))))
         (decoder (make-wire-decoder :max-feed-bytes (length wrong))))
    (check (eq :sequence
               (protocol-error-reason
                (lambda () (feed-wire-decoder decoder wrong))))
           "out-of-order first Frame was accepted")))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-cell)
  (test-frame)
  (test-fragmentation-and-multiplexing)
  (test-coalescing-and-faults)
  (format t "wire-protocol: ~D assertions passed~%" *assertions*)
  t)
