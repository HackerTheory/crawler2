(in-package :crawler2)

(defclass labyrinth-cell (cell)
  ((adjacent-regions :accessor adjacent-regions
                     :initform nil)))

(defmethod make-cell ((stage labyrinth) x y buffer)
  (make-instance 'labyrinth-cell :x x :y y))
