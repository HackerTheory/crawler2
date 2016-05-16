(in-package :crawler2-examples)

(defsketch example-labyrinth
    ((title "Example Labyrinth")
     (width (* *cell-size* (width *stage*)))
     (height (* *cell-size* (height *stage*)))
     (y-axis :up)
     (attrs nil :accessor attrs))
  (draw))

(defmethod regenerate ((window example-labyrinth))
  (generate 'labyrinth (attrs window)))

(defmethod run ((stage-type (eql 'labyrinth)) &rest attrs)
  (make-instance 'example-labyrinth :attrs attrs))
