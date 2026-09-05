;;;; package.lisp --- package definition for rosette-net-frame.

(in-package #:cl-user)

(defpackage #:rosette-net-frame
  (:use #:cl)
  (:import-from #:rosette-byte-core
                #:byte-vector #:make-byte-vector #:coerce-byte-vector)
  (:export
   ;; big-endian octet helpers
   #:get-u16be #:put-u16be #:get-u32be #:put-u32be
   ;; checksums
   #:internet-checksum #:ones-complement-sum
   #:crc32 #:crc32-table
   ;; IPv4 addressing
   #:ipv4-address #:ipv4-octets #:ipv4-string
   ;; IPv4 header
   #:ipv4-header #:make-ipv4-header #:ipv4-header-p
   #:ipv4-header-version #:ipv4-header-ihl
   #:ipv4-header-dscp #:ipv4-header-ecn
   #:ipv4-header-total-length #:ipv4-header-identification
   #:ipv4-header-flags #:ipv4-header-fragment-offset
   #:ipv4-header-ttl #:ipv4-header-protocol #:ipv4-header-checksum
   #:ipv4-header-source #:ipv4-header-destination
   #:build-ipv4-header #:parse-ipv4-header #:ipv4-header-valid-p
   ;; Ethernet II
   #:mac-address
   #:eth-frame #:make-eth-frame #:eth-frame-p
   #:eth-frame-destination #:eth-frame-source
   #:eth-frame-ethertype #:eth-frame-payload #:eth-frame-fcs
   #:build-eth-frame #:parse-eth-frame #:eth-frame-valid-p
   ;; whole-vertical round trip
   #:frame-ipv4 #:deframe-ipv4))
