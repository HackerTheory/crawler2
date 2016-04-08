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
      (setf (gethash (reverse regions) connections)
            (gethash regions connections))
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
               (enqueue (current-region stage) queue)
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
