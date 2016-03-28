(in-package :crawler2)

(defclass maze (stage)
  ((corridor-windiness :accessor corridor-windiness
                       :initarg :corridor-windiness
                       :initform 0)))

(defmethod ensure-dimensions ((stage maze))
  (with-slots (width height) stage
    (flet ((ensure (dimension)
             (if (evenp dimension)
                 (incf dimension)
                 dimension)))
      (setf width (ensure width)
            height (ensure height)))))

(defmethod build ((stage maze))
  (format t "Creating maze.~%")
  (create-corridors stage))
