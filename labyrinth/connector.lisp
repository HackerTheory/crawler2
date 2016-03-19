(in-package :crawler2)

(defmethod filter-connectable ((stage labyrinth) neighborhood)
  (with-accessors ((o origin) (n n) (s s) (e e) (w w)) neighborhood
    (let ((ns (mapcar #'cell-region-id (list n s)))
          (ew (mapcar #'cell-region-id (list e w))))
      (and (not (cell-region-id o))
           (or (and (not (apply #'eql ns))
                    (not (some #'null ns)))
               (and (not (apply #'eql ew))
                    (not (some #'null ew))))))))
