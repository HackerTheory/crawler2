(in-package :crawler2-examples)

(defsketch example-maze (:title "Example Maze"
                         :width (* *cell-size* (width *stage*))
                         :height (* *cell-size* (height *stage*))
                         :y-axis :up
                         :debug :scancode-grave)
    ((attrs))
  (draw))

(defmethod regenerate ((window example-maze))
  (generate 'maze (attrs window)))

(defmethod run ((stage-type (eql 'maze)) &rest attrs)
  (make-instance 'example-maze :attrs attrs))
