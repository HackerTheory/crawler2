(in-package :crawler2-examples)

(defsketch example-labyrinth (:title "Example Labyrinth"
                              :width (* *cell-size* (width *stage*))
                              :height (* *cell-size* (height *stage*))
                              :y-axis :up
                              :debug :scancode-grave)
    ()
  (draw))

(defmethod regenerate ((window example-labyrinth))
  (generate 'labyrinth *attrs*))

(defmethod run ((stage-type (eql 'labyrinth)) &key)
  (make-instance 'example-labyrinth))
