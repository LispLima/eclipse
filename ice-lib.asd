(in-package #:asdf-user)

(defsystem #:ice-lib
  :name "ice-lib"
  :pathname "lib/ice/"
  :depends-on (#:alexandria)
  :components (
               (:file "package")
               (:file "dependent")
               (:file "ICE")
               (:file "ICE-buffer")
               (:file "ICE-macros")
               (:file "ICE-request")
               (:file "ICE-auth")
               (:file "ICE-lib")))
