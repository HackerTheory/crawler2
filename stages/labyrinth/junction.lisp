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






;; It might be worth to put this into spanning-tree.lisp or something....

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
     :do (pushnew region-b (gethash region-a adjacency-graph))
     :finally (return adjacency-graph)))


(defun make-spanning-tree (stage potential-connections graph)
  (let ((spanning (make-hash-table))
        (queue (make-queue (hash-table-count potential-connections)))
        (visited (make-hash-table)))

    ;; First, we keep track of the root of the MSP.
    (setf (gethash :root spanning) *region*)

    ;; Start the BFS and build the MSP at the same time.
    (setf (gethash *region* visited) t)
    (enqueue *region* queue)

    ;; Perform the BFS
    (loop :until (queue-empty-p queue) :do
       (let ((current (dequeue queue)))
         (loop :for edge :in (gethash current graph)
            :unless (gethash edge visited)
            :do
            (setf (gethash edge visited) t)
            ;; Record an edge in the MST
            (push edge (gethash current spanning))
            (enqueue edge queue))))
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
           (mst (make-spanning-tree
                 stage potential-connections adjacency-graph)))

      ;; assemble this into a defstruct and shove into the stage.
      (display-mst mst)
      (setf (connectivity stage)
            (make-connectivity :mst mst
                               :potential-connections potential-connections)))))

(defun display-mst (mst)
  (format t "Spanning tree:~%")
  (loop :for k :being :the :hash-keys :in mst :using (hash-value v) :do
     (format t " region ~A connected to ~A~%" k v)))
