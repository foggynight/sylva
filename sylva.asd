(asdf:defsystem #:sylva
  :description "Library for various types of trees with a unified interface in
Common Lisp."
  :author "Robert Coffey"
  :license "GPLv2"
  :version "0.1.0"


  :serial t
  :pathname "src/"
  :components ((:file "package")

               (:file "general-tree")
               (:file "binary-tree")))
