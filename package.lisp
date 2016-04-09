(in-package :cl-user)

(defpackage #:crawler2
  (:use #:cl
        #:alexandria
        #:cl-speedy-queue)
  (:import-from #:cl-variates
               #:ranq1-random-number-generator
               #:random-seed
               #:random-element
               #:integer-random
               #:random-range-inclusive
               #:random-boolean)
  (:export #:stage
           #:labyrinth
           #:maze
           #:make-stage
           #:width
           #:height
           #:grid
           #:cell
           #:x
           #:y
           #:carvedp
           #:region-id
           #:featuresp))

(in-package :crawler2)

(defparameter *cell-calls* 0)
