(common-lisp:in-package :common-lisp-user)

(defpackage "COOLCLEAN-ECLIPSE-THEME"
  (:use eclipse clx-ext common-lisp)
  (:size 10)
  (:export repaint initialize-frame)
  (:documentation ""))

(in-package "COOLCLEAN-ECLIPSE-THEME")

(define-theme ("CoolClean")
  ((:default-style
     ()
     (:parts-to-redraw-on-focus (:title-bar))
     (:top ("top-inactive" "top-active"))
     (:top-right ("top-right"))
     (:menu-button ("menu-button-active"))
     (:close ("close-active"))
     (:icon-b ("minimize-active"))
     (:maximize ("maximize-active"))
     (:bottom ("bottom"))
     (:bottom-right ("bottom-right"))
     (:bottom-left ("bottom-left"))
     (:right ("right"))
     (:left ("left"))
     )
   (:transient-style
     ()
     (:parts-to-redraw-on-focus (:title-bar))
     (:top ("top-inactive" "top-active"))
     (:close ("t-close-active"))
     (:menu-button ("menu-button-active"))
     (:top-right ("top-right"))
     (:bottom ("bottom"))
     (:bottom-right ("bottom-right"))
     (:bottom-left ("bottom-left"))
     (:right ("right"))
     (:left ("left")))))

(defmethod repaint ((widget title-bar) (name (eql "CoolClean")) (focus t))
  (declare (ignorable name focus))
  (with-slots ((window eclipse::window)
	       (item-to-draw eclipse::item-to-draw)
	       (gcontext eclipse::gcontext)
	       (master eclipse::master)) widget
    (multiple-value-bind (width height) (drawable-sizes window)
      (with-slots (frame-style) master
	(let ((top-pix (get-pixmap frame-style :top-active)))
	  (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	    (xlib:draw-rectangle window gcontext 0 0 width height t)))))
    (draw-centered-text window gcontext item-to-draw :color *white* :x 5)))

(defmethod repaint ((widget title-bar) (name (eql "CoolClean")) (focus null))
  (declare (ignorable name focus))
  (with-slots ((window eclipse::window)
	       (item-to-draw eclipse::item-to-draw)
	       (gcontext eclipse::gcontext)) widget
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white* :x 5)))
