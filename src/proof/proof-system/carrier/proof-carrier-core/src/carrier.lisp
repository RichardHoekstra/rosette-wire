;;;; carrier.lisp --- proof certificate AST.

(in-package #:rosette-proof-carrier-core)

(defstruct (proof-node
            (:constructor %make-proof-node
                (&key (kind :claim) label payload children metadata)))
  "Small explicit AST node for proof, runtime, and benchmark certificates."
  (kind :claim :type keyword)
  (label nil :type t)
  payload
  (children nil :type list)
  (metadata nil :type list))

(defun make-proof-node (&key (kind :claim) label payload children metadata)
  "Construct a proof node without retaining caller-owned list structure."
  (%make-proof-node
   :kind kind
   :label label
   :payload payload
   :children (copy-list children)
   :metadata (copy-plist metadata :label "Proof node METADATA")))

(defun proof-leaf (kind label &key payload metadata)
  "Construct a proof node with no children."
  (make-proof-node :kind kind :label label :payload payload :metadata metadata))

(defun proof-branch (kind label children &key payload metadata)
  "Construct a proof node with CHILDREN."
  (make-proof-node :kind kind
                   :label label
                   :payload payload
                   :children children
                   :metadata metadata))

(defun proof-node->plist (node)
  "Return NODE as a stable nested plist."
  (check-type node proof-node)
  (list :kind (proof-node-kind node)
        :label (proof-node-label node)
        :payload (proof-node-payload node)
        :children (mapcar #'proof-node->plist (proof-node-children node))
        :metadata (copy-list (proof-node-metadata node))))

(defun plist->proof-node (plist)
  "Decode a plist produced by PROOF-NODE->PLIST."
  (make-proof-node
   :kind (getf plist :kind)
   :label (getf plist :label)
   :payload (getf plist :payload)
   :children (mapcar #'plist->proof-node (getf plist :children))
   :metadata (copy-list (getf plist :metadata))))

(defun proof-node-size (node)
  "Return the number of nodes in NODE's tree."
  (check-type node proof-node)
  (1+ (loop for child in (proof-node-children node)
            sum (proof-node-size child))))

(defun proof-node-depth (node)
  "Return the maximum root-to-leaf depth of NODE."
  (check-type node proof-node)
  (if (null (proof-node-children node))
      1
      (1+ (loop for child in (proof-node-children node)
                maximize (proof-node-depth child)))))
