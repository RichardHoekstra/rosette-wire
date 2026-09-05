;;;; dag.lisp --- Hash-consed s-expression DAG core.

(in-package #:rosette-sexpr-dag)

(defstruct (dag-node
            (:constructor %make-dag-node
                (id kind value children key size occurrences provenance)))
  "A hash-consed s-expression node.

KIND is :ATOM or :CONS. VALUE is used for atoms. CHILDREN is NIL for atoms
and a two-element list of child ids for cons cells."
  id
  kind
  value
  children
  key
  (size 1 :type fixnum)
  (occurrences 0 :type fixnum)
  (provenance nil :type list))

(defun %make-equal-table ()
  "Return a hash table keyed by structurally equal S-expressions."
  (make-hash-table :test #'equal))

(defstruct (dag-index (:constructor %make-dag-index))
  "Mutable hash-cons table for s-expression DAGs."
  (nodes (make-array 0 :adjustable t :fill-pointer 0))
  (table (%make-equal-table))
  (root-ids nil)
  (tree-node-count 0 :type fixnum))

(defun make-dag-index ()
  "Return a fresh DAG index."
  (%make-dag-index))

(defun dag-index-node-count (index)
  "Return the number of unique DAG nodes in INDEX."
  (length (dag-index-nodes index)))

(defun %node (index id)
  (aref (dag-index-nodes index) id))

(defun %atom-key (value)
  (list :atom value))

(defun %cons-key (car-id cdr-id)
  (list :cons car-id cdr-id))

(defun %intern-node (index kind value children key size provenance)
  (let ((existing (gethash key (dag-index-table index))))
    (if existing
        (let ((node (%node index existing)))
          (incf (dag-node-occurrences node))
          (when provenance
            (pushnew provenance (dag-node-provenance node) :test #'equal))
          existing)
        (let* ((id (length (dag-index-nodes index)))
               (node (%make-dag-node id kind value children key size 1
                                      (and provenance (list provenance)))))
          (vector-push-extend node (dag-index-nodes index))
          (setf (gethash key (dag-index-table index)) id)
          id))))

(defun tree-node-count (sexp)
  "Count cons and atom nodes in SEXP as a tree, with repeated subtrees counted repeatedly."
  (if (consp sexp)
      (+ 1 (tree-node-count (car sexp)) (tree-node-count (cdr sexp)))
      1))

(defun sexp->dag (sexp &key (index (make-dag-index)) provenance root)
  "Hash-cons SEXP into INDEX and return the root node id.

When ROOT is true, the id is also appended to INDEX root ids and the tree
node count is added to the index total. PROVENANCE is an arbitrary object,
typically a plist such as (:FILE ... :FORM-INDEX ...)."
  (labels ((walk (x)
             (if (consp x)
                 (multiple-value-bind (car-id car-size)
                     (walk (car x))
                   (multiple-value-bind (cdr-id cdr-size)
                       (walk (cdr x))
                     (let ((size (+ 1 car-size cdr-size)))
                       (values
                        (%intern-node index :cons nil (list car-id cdr-id)
                                      (%cons-key car-id cdr-id) size provenance)
                        size))))
                 (values
                  (%intern-node index :atom x nil (%atom-key x) 1 provenance)
                  1))))
    (multiple-value-bind (id tree-size)
        (walk sexp)
      (when root
        (push id (dag-index-root-ids index))
        (incf (dag-index-tree-node-count index) tree-size))
      id)))

(defun dag->sexp (index node-or-id)
  "Reconstruct a tree s-expression from NODE-OR-ID in INDEX."
  (let ((node (if (typep node-or-id 'dag-node)
                  node-or-id
                  (%node index node-or-id))))
    (ecase (dag-node-kind node)
      (:atom (dag-node-value node))
      (:cons (destructuring-bind (car-id cdr-id) (dag-node-children node)
               (cons (dag->sexp index car-id)
                     (dag->sexp index cdr-id)))))))

(defun dag-node-count (sexp)
  "Return the number of unique subnodes in SEXP after hash-consing."
  (let ((index (make-dag-index)))
    (sexp->dag sexp :index index)
    (dag-index-node-count index)))

(defun beta1 (thing)
  "Return TREE-NODE-COUNT minus DAG-NODE-COUNT.

THING may be an s-expression or a DAG-INDEX."
  (etypecase thing
    (dag-index (- (dag-index-tree-node-count thing)
                  (dag-index-node-count thing)))
    (t (- (tree-node-count thing) (dag-node-count thing)))))

(defun shared-ratio (index)
  "Return DAG unique nodes divided by total tree nodes for INDEX.

Lower values mean more sharing. Returns 1.0 for an empty index."
  (if (zerop (dag-index-tree-node-count index))
      1d0
      (/ (as-f64 (dag-index-node-count index))
         (as-f64 (dag-index-tree-node-count index)))))

(defun dag-index->csr (index)
  "Project INDEX into a directed ROSETTE-GRAPH-CORE:CSR-GRAPH.

Vertices are DAG-NODE ids (0 .. node-count-1); a CONS node has out-edges
to its CAR-id and CDR-id; atom nodes have no out-edges.  This makes the
substrate's CSR traversal kernels (BFS, DFS, SCC, transpose) available
to consumers of hash-consed sexpr DAGs without each of them
reimplementing children-walking."
  (let* ((n (dag-index-node-count index))
         (edges nil))
    (loop for u from 0 below n
          for node = (%node index u)
          when (eq (dag-node-kind node) :cons)
            do (destructuring-bind (car-id cdr-id) (dag-node-children node)
                 (push (cons u car-id) edges)
                 (push (cons u cdr-id) edges)))
    (rosette-graph-core:csr-from-edges n (nreverse edges) :directed-p t)))

(defun dag-index-bfs-order (index root-id)
  "Return BFS visit order over INDEX's children-graph starting at ROOT-ID."
  (multiple-value-bind (parents levels order)
      (rosette-graph-core:csr-bfs (dag-index->csr index) root-id)
    (declare (ignore parents levels))
    order))

(defun dag-index-postorder (index root-id)
  "Return DFS postorder traversal over INDEX's children-graph from ROOT-ID."
  (multiple-value-bind (preord postord)
      (rosette-graph-core:csr-dfs (dag-index->csr index) root-id)
    (declare (ignore preord))
    postord))

(defun print-dag-dot (index &key (stream *standard-output*) (limit 250))
  "Print INDEX as a Graphviz DOT graph.

LIMIT caps the number of unique DAG nodes emitted so large projects can still
produce inspectable graph slices."
  (format stream "digraph rosette_sexpr_dag {~%")
  (format stream "  rankdir=LR;~%")
  (loop for node across (dag-index-nodes index)
        for emitted from 0
        while (< emitted limit)
        do (format stream "  n~D [label=\"~A\\nsize=~D occ=~D\"];~%"
                   (dag-node-id node)
                   (dot-escape
                    (if (eq (dag-node-kind node) :atom)
                        (dag-node-value node)
                        :cons)
                    :readably t)
                   (dag-node-size node)
                   (dag-node-occurrences node))
           (when (eq (dag-node-kind node) :cons)
             (destructuring-bind (car-id cdr-id) (dag-node-children node)
               (when (< car-id limit)
                 (format stream "  n~D -> n~D [label=\"car\"];~%"
                         (dag-node-id node)
                         car-id))
               (when (< cdr-id limit)
                 (format stream "  n~D -> n~D [label=\"cdr\"];~%"
                         (dag-node-id node)
                         cdr-id)))))
  (dolist (root-id (remove-duplicates (dag-index-root-ids index)))
    (when (< root-id limit)
      (format stream "  root~D [shape=point];~%" root-id)
      (format stream "  root~D -> n~D;~%" root-id root-id)))
  (format stream "}~%")
  (values))
