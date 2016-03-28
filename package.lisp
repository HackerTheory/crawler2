(in-package :cl-user)

(defpackage #:crawler2
  (:use #:cl
        #:alexandria
        #:cl-variates
        #:cl-speedy-queue)
  (:export #:stage
           #:labyrinth
           #:maze
           #:make-stage
           #:width
           #:height
           #:grid
           #:cell
           #:carvedp
           #:region-id
           #:featuresp))

(in-package :crawler2)

(defparameter *cell-calls* 0)
