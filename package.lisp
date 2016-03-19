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
           #:carvedp))

(in-package :crawler2)
