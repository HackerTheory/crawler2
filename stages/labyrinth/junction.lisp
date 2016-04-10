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
      (add-feature cell :connector))))

(defmethod carve-tree (stage connections)
  (loop :for edge :in (edges (mst (graphs stage)))
        :for cell = (rng 'elt :list (gethash edge connections))
        :do (make-junction stage cell)))

(defmethod carve-loops (stage connections)
  (loop :with graphs = (graphs stage)
        :for edge :in (edges (connectable graphs))
        :for connectors = (gethash edge connections)
        :for distance = (length (apply #'shortest-path (mst graphs) edge))
        :unless (or (<= distance 2)
                    (has-edge-p (final graphs) edge))
          :do (let ((cell (rng 'elt :list (gethash edge connections))))
                (make-junction stage cell)
                (add-edge (final graphs) edge))))

(defmethod connect-regions (stage)
  (let ((connections (make-hash-table :test 'equal)))
    (convolve stage (layout :orthogonal) #'filter-connectable (connect connections))
    (make-graphs stage connections)
    (carve-tree stage connections)
    (carve-loops stage connections)))
