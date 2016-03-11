(in-package :cl-user)

(defpackage #:crawler2
  (:use #:cl
        #:alexandria
        #:cl-variates)
  (:export #:stage
           #:labyrinth
           #:make-stage
           #:width
           #:height
           #:grid
           #:cell
           #:walkablep))

(in-package :crawler2)
