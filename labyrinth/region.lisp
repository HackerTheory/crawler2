(in-package :crawler2)

(defstruct (region (:conc-name nil)
                   (:constructor %make-region (id)))
  id connectors)

(defmethod make-region ((stage labyrinth))
  (with-slots (current-region regions) stage
    (let ((id (incf current-region)))
      (setf (gethash id regions) (%make-region id))
      id)))

(defmethod get-region ((stage labyrinth) id)
  (gethash id (regions stage)))

(defmethod create-junctions ((stage labyrinth))
  (loop :with region-id = (rng 'inc :min 1 :max (current-region stage))
        :for connectors = (connectors (get-region stage region-id))
        :while connectors
        :for source = (rng 'elt :list connectors)
        :for target = (first (remove region-id (adjacent-regions source)))
        :do (make-junction stage source)
            (remove-extra-connectors stage region-id target)
            (move-connectors stage target region-id))
  (convolve stage (layout :ortho) #'filter-connectable #'make-extra-junctions))

(defmethod adjacent-junction-p ((stage labyrinth) cell)
  (with-slots (x y) cell
    (let ((neighborhood (nh-realize (layout :ortho) stage x y)))
      (flet ((bail-junction (x)
               (when (featuresp x :junction)
                 (return-from adjacent-junction-p t))))
        (nfilter neighborhood #'bail-junction)))))

(defmethod make-junction ((stage labyrinth) cell)
  (unless (adjacent-junction-p stage cell)
    (carve stage cell :region nil :feature :junction)))

(defmethod make-extra-junctions ((stage labyrinth) neighborhood)
  (when (< (rng 'inc) (junction-rate stage))
    (make-junction stage (origin neighborhood))))
