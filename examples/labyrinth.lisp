(in-package :crawler2-examples)

(defsketch example-labyrinth
    ((sketch:title "Example Labyrinth")
     (sketch:width (* *cell-size* (width *stage*)))
     (sketch:height (* *cell-size* (height *stage*)))
     (sketch:y-axis :up)
     (attrs nil :accessor attrs))
  (draw))

(defmethod regenerate ((window example-labyrinth))
  (generate 'labyrinth (attrs window)))

(defmethod run ((stage-type (eql 'labyrinth)) &rest attrs)
  (make-instance 'example-labyrinth :attrs attrs))
