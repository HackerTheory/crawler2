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
      (carve stage cell :region-id nil :feature (if doorp :door :junction)))))

(defmethod filter-connectable ((stage labyrinth) nh)
  (with-accessors ((o origin) (n n) (s s) (e e) (w w)) nh
    (and (not (region-id o))
         (let ((ns (mapcar #'region-id (list n s)))
               (ew (mapcar #'region-id (list e w))))
           (or (and (not (apply #'eql ns))
                    (not (some #'null ns)))
               (and (not (apply #'eql ew))
                    (not (some #'null ew))))))))

(defmethod connect-regions (stage)
  (let ((connections (make-hash-table :test 'equal)))
    (flet ((connect (stage nh)
             (declare (ignore stage))
             (let ((regions (remove nil (nmap nh #'region-id)))
                   (origin (origin nh)))
               (pushnew origin (gethash regions connections))
               (setf (gethash (reverse regions) connections)
                     (gethash regions connections))
               (add-feature origin :connector))))
      (convolve stage (layout :orthogonal) #'filter-connectable #'connect))
    (loop :with connected = (make-hash-table :test 'equal)
          :for pair :in (hash-table-keys connections)
          :unless (gethash pair connected)
            :do (let ((cell (rng 'elt :list (gethash pair connections))))
                  (make-junction stage cell)
                  (deletef (gethash pair connections) cell)
                  (setf (gethash pair connected) t
                        (gethash (reverse pair) connected) t)))))
