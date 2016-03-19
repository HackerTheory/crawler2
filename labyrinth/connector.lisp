(in-package :crawler2)

(defmethod filter-connectable ((stage labyrinth) neighborhood)
  (with-accessors ((o origin) (n n) (s s) (e e) (w w)) neighborhood
    (let ((ns (mapcar #'region-id (list n s)))
          (ew (mapcar #'region-id (list e w))))
      (and (not (region-id o))
           (or (and (not (apply #'eql ns))
                    (not (some #'null ns)))
               (and (not (apply #'eql ew))
                    (not (some #'null ew))))))))

(defmethod connect ((stage labyrinth) neighborhood)
  (let ((cell (origin neighborhood)))
    (with-slots (adjacent-regions) cell
      (setf adjacent-regions (remove nil (nmap neighborhood #'region-id)))
      (dolist (region-id adjacent-regions)
        (push cell (connectors (get-region stage region-id)))))))
