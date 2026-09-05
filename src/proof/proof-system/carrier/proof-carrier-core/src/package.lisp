;;;; package.lisp --- package definition for rosette-proof-carrier-core.

(defpackage #:rosette-proof-carrier-core
  (:use #:cl)
  (:import-from #:rosette-metadata-core
                #:copy-plist)
  (:export
   #:proof-node
   #:proof-node-p
   #:make-proof-node
   #:proof-node-kind
   #:proof-node-label
   #:proof-node-payload
   #:proof-node-children
   #:proof-node-metadata
   #:proof-leaf
   #:proof-branch
   #:proof-node->plist
   #:plist->proof-node
   #:proof-node-size
   #:proof-node-depth))

(in-package #:rosette-proof-carrier-core)
