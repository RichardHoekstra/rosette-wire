;;;; package.lisp --- package definition for rosette-metadata-core.

(defpackage #:rosette-metadata-core
  (:use #:cl)
  (:export
   #:plist-p
   #:require-plist
   #:copy-plist
   #:merge-plist-right
   #:keyword-plist-p
   #:require-keyword-plist
   #:copy-keyword-plist
   #:tagged-term-p
   #:required-key-p
   #:tagged-term-key-p
   #:copy-plist-tree
   #:rosette-metadata-core))

(in-package #:rosette-metadata-core)
