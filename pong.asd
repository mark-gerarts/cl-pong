(asdf:defsystem :pong
  :description "Pong implementation in Common Lisp using trivial-gamekit"
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:trivial-gamekit
               :cl-bodge)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "vars")
                             (:file "ui")
                             (:file "game")
                             (:file "pong")))))
