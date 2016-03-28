(in-package :crawler2-examples)

(defsketch example-maze (:title "Example Maze"
                         :width (* *cell-size* (width *stage*))
                         :height (* *cell-size* (height *stage*))
                         :y-axis :up
                         :debug :scancode-grave)
    ()
  (draw))

(defmethod regenerate ((window example-maze))
  (generate 'maze *attrs*))

(defmethod run ((stage-type (eql 'maze)) &key)
  (make-instance 'example-maze))
