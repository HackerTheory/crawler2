(in-package :crawler2)

(defvar *region* 0)

(defstruct (region (:conc-name nil)
                   (:constructor %make-region (id)))
  id cells)

(defmethod make-region (stage)
  (let ((id (incf *region*)))
    (setf (gethash id (regions stage)) (%make-region id))
    id))

(defmethod get-region (stage id)
  (gethash id (regions stage)))
