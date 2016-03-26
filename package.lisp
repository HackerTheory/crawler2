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
           #:carvedp
           #:region-id))

(in-package :crawler2)

(defparameter *cell-calls* 0)
