(in-package #:asdf-user)

(defsystem #:eclipse-lib
  :depends-on (#:alexandria
               #:clx)
  :pathname "lib/"
  :components ((:file "image-reader")
               (:file "manager-commons")
               (:file "netwm-manager")
               (:file "gnome-manager")))
