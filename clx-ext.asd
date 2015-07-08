(in-package #:asdf-user)

(defsystem #:clx-ext
  :name "clx-ext"
  :depends-on (#:clx
               #:alexandria)
  :pathname "lib/clx-ext/"
  :components ((:file "clx-patch")
               (:file "xvidmode")
               (:file "package")
               (:file "clx-extensions")
               (:file "cursordef")
               (:file "cursor")
               (:file "keysyms")
               (:file "keysymdef")
               (:file "event")))
