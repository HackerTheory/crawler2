(defsystem #:crawler2
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A procedural dungeon generation library."
  :depends-on (#:alexandria
               #:graph
               #:cl-variates
               #:cl-speedy-queue)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "random")
   (:file "neighborhood")
   (:file "region")
   (:file "graph")
   (:file "corridor")
   (:file "stage-basic")
   (:file "stage-buffered")
   (:file "cell")
   (:module "stages"
    :components
    ((:module "maze"
      :components
      ((:file "stage")))
     (:module "labyrinth"
      :components
      ((:file "stage")
       (:file "junction")
       (:file "room")
       (:file "stairs")))))))
