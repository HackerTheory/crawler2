(in-package :crawler2)

(defmethod adjacent-junction-p ((stage labyrinth) cell)
  (let ((nh (cell-nh stage cell (layout :orthogonal))))
    (nmap-short
     nh
     (lambda (x) (featuresp x :junction :door))
     :reduce any
     :return-val t)))

(defmethod make-junction ((stage labyrinth) cell)
  (unless (adjacent-junction-p stage cell)
    (let ((doorp (< (rng 'inc) (door-rate stage))))
      (carve stage cell :region-id nil :feature (if doorp :door :junction))
      (remove-feature cell :connector))))

(defmethod filter-connectable ((stage labyrinth) nh)
  (with-accessors ((n n) (s s) (e e) (w w)) nh
    (flet ((filter (x y)
             (let ((items (mapcar #'region-id (list x y))))
               (and (not (some #'null items))
                    (not (apply #'= items))))))
      (and (not (region-id (origin nh)))
           (or (filter n s)
               (filter e w))))))

(defun connect (connections)
  (lambda (stage nh)
    (declare (ignore stage))
    (let ((regions (remove nil (nmap nh #'region-id)))
          (cell (origin nh)))
      (pushnew cell (gethash regions connections))
      (setf (gethash (reverse regions) connections) (gethash regions connections))
      (add-feature cell :connector))))

(defun make-connection-graph (connections)
  (loop :with graph = (make-hash-table :size 512)
     :with size = 0
     :for (region-a region-b) :in (hash-table-keys connections)
     :do (pushnew region-b (gethash region-a graph))
     (incf size)
     :finally (return (values graph size))))

(defun carve-junctions (stage connections)
  (multiple-value-bind (graph size) (make-connection-graph connections)
    (flet ((carve-tree ()
             (let ((queue (make-queue size)))
               (enqueue *region* queue)
               (loop :with visited = (make-hash-table :size 512)
                  :until (queue-empty-p queue)
                  :for current = (dequeue queue)
                  :do (setf (gethash current visited) t)
                  (loop :for edge :in (gethash current graph)
                     :for pair = (list current edge)
                     :for connectors = (gethash pair connections)
                     :unless (gethash edge visited)
                     :do (let ((cell (rng 'elt :list connectors)))
                           (make-junction stage cell)
                           (setf (gethash edge visited) t)
                           (enqueue edge queue))))))
           (carve-loops ()
             (loop :for source :in (hash-table-keys graph)
                :for target = (rng 'elt :list (gethash source graph))
                :for connectors = (gethash (list source target) connections)
                :do (loop :for connector :in connectors
                       :when (and connector
                                  (< (rng 'inc) (loop-rate stage)))
                       :do (make-junction stage connector)))))
      (carve-tree)
      (carve-loops))))

(defmethod connect-regions (stage)
  (let ((connections (make-hash-table :test 'equal :size 512)))
    (convolve stage (layout :orthogonal) #'filter-connectable (connect connections))
    (carve-junctions stage connections)))

(defmethod connect-regions-new (stage)
  ;; Phase 1: Make junctions simply to honor the MST.

  ;; Phase 2: Relax the MST by adding junctions which cause cycles.

  nil
  )




(defun bfs (adjacency-graph root func &key (successors
                                            (lambda (node adj-graph)
                                              (gethash node adj-graph))))
  (let ((queue (make-queue 1024)) ;; TODO: Having a constant here sucks.
        (visited (make-hash-table))
        (results ()))

    (setf (gethash root visited) t)
    (push (funcall func root) results)
    (enqueue (list root 0) queue)
    (format t ">bfs (~A ~A)~%" root 0)

    ;; Perform the BFS
    (loop :until (queue-empty-p queue) :do
       (destructuring-bind (parent distance) (dequeue queue)
         (loop :for child :in (funcall successors parent adjacency-graph)
            :unless (gethash child visited)
            :do
            (setf (gethash child visited) t)
            (push (funcall func child) results)
            (format t ">bfs (~A ~A)~%" child (1+ distance))
            (enqueue (list child (1+ distance)) queue))))

    ;; returned in BFS level-order traversal.
    (nreverse results)))

(defun dfs (adjacency-graph root func &key (successors
                                            (lambda (node adj-graph)
                                              (gethash node adj-graph))))
  (let ((stack ())
        (visited (make-hash-table))
        (results ()))

    (push (list root 0) stack)
    (format t ">dfs (~A ~A)~%" root 0)

    ;; Perform the DFS
    (loop :until (null stack) :do
       (destructuring-bind (node distance) (pop stack)
         (format t ">dfs-pop (~A ~A)~%" node distance)
         (unless (gethash node visited)
           (setf (gethash node visited) t)
           (format t ">dfs-visit (~A ~A)~%" node distance)
           (push (funcall func node) results)
           (loop :for child :in (funcall successors node adjacency-graph)
              :unless (gethash child visited)
              :do
              (format t ">dfs-add (~A ~A)~%" child (1+ distance))
              (push (list child (1+ distance)) stack)))))

    ;; returned in BFS level-order traversal.
    (nreverse results)))

;; Higher order bfs walk across an adjacency-graph encoded into a hash table.
;; parent-0 -> (child-1 child-1 child-2 ... child-N)
;; ...
;; NOTE: This function assumes the adjacency-graph is encoded in a hash table.
(defun bfs-edge (adjacency-graph root func &key (successors
                                                 (lambda (node adj-graph)
                                                   (gethash node adj-graph))))
  (let ((queue (make-queue 1024)) ;; TODO: Having a constant here sucks.
        (visited (make-hash-table))
        (results ()))

    (setf (gethash root visited) t)
    (enqueue root queue)

    ;; Perform the BFS
    (loop :until (queue-empty-p queue) :do
       (let ((parent (dequeue queue)))
         (loop :for child :in (funcall successors parent adjacency-graph)
            :unless (gethash child visited)
            :do
            (setf (gethash child visited) t)
            (push (funcall func parent child) results)
            (enqueue child queue))))

    ;; returned in BFS level-order traversal.
    (nreverse results)))



(defun dfs-edge (adjacency-graph root func &key (successors
                                                 (lambda (node adj-graph)
                                                   (gethash node adj-graph))))
  (let ((stack ())
        (visited (make-hash-table))
        (results ()))

    (push root stack)

    ;; Perform the DFS
    (loop :until (null stack) :do
       (let ((parent (pop stack)))
         (unless (gethash parent visited)
           (setf (gethash parent visited) t)
           (loop :for child :in (reverse (funcall successors parent adjacency-graph))
              :do
              (unless (gethash child visited)
                (push (funcall func parent child) results)
                (push child stack))))))

    ;; returned in BFS level-order traversal.
    (nreverse results)))



;; It might be worth to put the below into spanning-tree.lisp or something....

;; called in the convolve, for each cell which is connectable, store a
;; reference to the cell in list keyed by the (A B) and (B A) region
;; connection.
(defun accumulate-potential-connection (potential-connections)
  (lambda (stage nh)
    (declare (ignore stage))
    (let ((regions (remove nil (nmap nh #'region-id)))
          (cell (origin nh)))
      ;; Keep track of all cells which satisfy an edge between regions.
      (pushnew cell (gethash regions potential-connections))
      (setf (gethash (reverse regions) potential-connections)
            (gethash regions potential-connections)))))

(defun make-region-adjacency-graph (connections)
  ;; build an edge adjacency graph. hash key is a region, value is a
  ;; list of regions that it would be able to connect to.
  (loop :with adjacency-graph = (make-hash-table :test 'eql)
     :for (region-a region-b) :in (hash-table-keys connections)
     :do
     (pushnew region-b (gethash region-a adjacency-graph))
     (pushnew region-a (gethash region-b adjacency-graph))
     :finally (return adjacency-graph)))

(defun make-spanning-tree (adjacency-graph)
  (let ((spanning (make-hash-table :test 'eql))
        (random-region-id (rng 'int :min 1 :max *region*)))

    ;; Define the root of the MST.
    (setf (gethash :root spanning) random-region-id)

    ;; Then bfs-edge walk the adjacency-graph and assemble the MST.
    (bfs-edge adjacency-graph random-region-id
              (lambda (parent child)
                (pushnew parent (gethash child spanning))
                (pushnew child (gethash parent spanning))))

    spanning))

(defmethod build-mst (stage)
  (let ((potential-connections (make-hash-table :test 'equal)))

    ;; 1. Find all potential connections between regions. The keys of
    ;; the potential-connections hash table represent possible single
    ;; edged betwee regions.
    (convolve stage
              (layout :orthogonal)
              #'filter-connectable
              (accumulate-potential-connection potential-connections))

    ;; 2. Build the region adjacency graph. The key of the graph is a region and
    ;; the value a list of regions to which it has the potential to connect.
    (let* ((adjacency-graph (make-region-adjacency-graph potential-connections))
           ;; 3. Construct a spanning tree.
           (mst (make-spanning-tree adjacency-graph)))

      (format t "Adjacency graph:~%")
      (maphash (lambda (k v)
                 (format t " node ~A -> ~A~%" k v))
               adjacency-graph)

      (format t "BFS of mst:~%")
      (bfs-edge mst (gethash :root mst)
                (lambda (parent child)
                  (format t " parent = ~A child = ~A~%" parent child)))

      (format t "DFS of mst:~%")
      (dfs-edge mst (gethash :root mst)
                (lambda (parent child)
                  (format t " parent = ~A child = ~A~%" parent child)))


      (format t "BFS of adjacency with root ~A~%" (gethash :root mst))
      (bfs adjacency-graph (gethash :root mst)
           (lambda (node)
             (format t " ~A~%" node)))

      (format t "DFS of adjacency with root ~A~%" (gethash :root mst))
      (dfs adjacency-graph (gethash :root mst)
           (lambda (node)
             (format t " ~A~%" node)))

      (display-mst mst)

      ;; 4. assemble this into a defstruct and shove into the stage.
      (setf (connectivity stage)
            (make-connectivity :mst mst
                               :potential-connections potential-connections)))))

(defun display-mst (mst)
  (format t "Spanning tree:~%")
  (loop :for k :being :the :hash-keys :in mst :using (hash-value v) :do
     (format t " node ~A -> ~A~%" k v)))
