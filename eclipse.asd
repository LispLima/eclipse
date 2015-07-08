(in-package #:asdf-user)

(defsystem #:eclipse
  :name "eclipse"
  :depends-on (#:alexandria
               #:sm-lib
               #:clx-ext
               #:eclipse-lib)
  :components ((:file "config")
               (:file "programmed-tasks")
               (:file "package")
               (:file "global")
               (:file "misc")
               (:file "themer")
               (:file "menu")
               (:file "gestures")
               (:file "widgets")
               (:file "virtual-screen")
               (:file "rectangles")
               (:file "wm")
               (:file "input")
               (:file "move-resize")
               (:file "eclipse")))
