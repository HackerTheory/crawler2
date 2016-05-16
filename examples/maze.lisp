(in-package :crawler2-examples)

(defsketch example-maze
    ((sketch:title "Example Maze")
     (sketch:width (* *cell-size* (width *stage*)))
     (sketch:height (* *cell-size* (height *stage*)))
     (sketch:y-axis :up)
     (attrs nil :accessor attrs))
  (draw))

(defmethod regenerate ((window example-maze))
  (generate 'maze (attrs window)))

(defmethod run ((stage-type (eql 'maze)) &rest attrs)
  (make-instance 'example-maze :attrs attrs))
