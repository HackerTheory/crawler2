(in-package :crawler2)

(defmethod staircase-suitable-p ((stage labyrinth) neighborhood)
  (let ((cell (origin neighborhood)))
    (and (featuresp cell :room)
         (not (featuresp cell :junction))
         (not (adjacent-junction-p stage cell)))))

(defmethod choose-upstairs ((stage labyrinth))
  (with-slots (x1 x2 y1 y2) (rng 'elt :list (rooms stage))
    (rng 'elt :list
         (mapcar
          #'origin
          (collect-cells
           stage
           (layout :square :maximum 0)
           #'staircase-suitable-p
           :x1 x1
           :x2 (- x2 (width stage))
           :y1 y1
           :y2 (- y2 (height stage)))))))

(defmethod make-upstairs ((stage labyrinth))
  (let ((cell (choose-upstairs stage)))
    (add-feature cell :stairs-up)
    (setf (distance cell) 0)
    cell))
