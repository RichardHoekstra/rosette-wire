;;;; wire-protocol/src/package.lisp --- public byte-wire API.

(defpackage #:rosette-wire-protocol
  (:use #:cl)
  (:import-from #:rosette-symmetric-crypto
                #:make-byte-vector #:sha256 #:sha256-hex)
  (:export
   #:+cell-magic+ #:+wire-magic+ #:+wire-version+
   #:+cell-header-length+ #:+frame-header-length+
   #:wire-protocol-error #:wire-protocol-error-reason
   #:wire-protocol-error-offset #:wire-protocol-error-detail
   #:cell #:cell-p #:make-cell #:cell-tag #:cell-media-type
   #:cell-payload #:cell-id #:encode-cell #:decode-cell
   #:frame #:frame-p #:make-frame #:make-cell-frame
   #:frame-kind #:frame-flags #:frame-stream-id #:frame-sequence
   #:frame-payload #:frame-payload-id #:encode-frame #:decode-frame
   #:wire-decoder #:wire-decoder-p #:make-wire-decoder
   #:wire-decoder-fault #:wire-decoder-buffered-bytes
   #:feed-wire-decoder))
