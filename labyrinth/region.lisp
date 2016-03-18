(in-package :crawler2)

(defvar *current-region* 0)

(defstruct (region (:conc-name nil)
                   (:constructor %make-region (id)))
  id connectors)

(defmethod make-region ((stage labyrinth))
  (with-slots (regions) stage
    (let ((id (incf *current-region*)))
      (setf (gethash id regions) (%make-region id))
      id)))

(defmethod get-region ((stage labyrinth) id)
  (gethash id (regions stage)))
