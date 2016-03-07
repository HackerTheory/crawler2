(in-package :cl-user)

(defpackage #:crawler2-examples
  (:use #:cl
        #:sdl2.kit
        #:crawler2)
  (:import-from #:sketch
                #:defsketch
                #:rect
                #:with-pen
                #:make-pen
                #:gray
                #:rgb
                #:background)
  (:export #:run))

(in-package :crawler2-examples)
