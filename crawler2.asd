(defsystem #:crawler2
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A procedural dungeon generation library."
  :depends-on (#:alexandria
               #:cl-variates)
  :serial t
  :components
  ((:file "package")
   (:file "random")
   (:file "buffer")
   (:file "cell")
   (:file "neighborhood")
   (:file "stage")
   (:module "labyrinth"
    :components
    ((:file "stage")
     (:file "region")
     (:file "room")
     (:file "corridor")))))
