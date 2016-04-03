(in-package :crawler2)

(defstruct (region (:conc-name nil)
                   (:constructor %make-region (id)))
  id cells)

(defmethod make-region (stage)
  (with-slots (current-region regions) stage
    (let ((id (incf current-region)))
      (setf (gethash id regions) (%make-region id))
      id)))

(defmethod get-region (stage id)
  (gethash id (regions stage)))
