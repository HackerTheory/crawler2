(defsystem #:crawler2-examples
  :name "Crawler Examples"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "Crawler stage examples."
  :depends-on (#:crawler2
               #:sketch)
  :serial t
  :pathname "examples"
  :components ((:file "package")
               (:file "example")
               (:file "labyrinth")
               (:file "maze")))
