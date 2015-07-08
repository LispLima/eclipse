(in-package #:asdf-user)

(defsystem #:sm-lib
  :name "sm-lib"
  :pathname "lib/sm/"
  :depends-on (#:ice-lib
               #:alexandria)
  :components ((:file "package")
               (:file "sm")))
