(in-package :ECLIPSE-INTERNALS)

(define-theme ("Step")
  ((:default-style
     ()
     (:parts-to-redraw-on-focus (:close :title-bar :menu-button))
     (:top ("top-inactive" "top-active"))
     (:menu-button ("menu-button-inactive" "menu-button-active"))
     (:close ("close-inactive" "close-active"))
     (:bottom ("bottom"))
     (:bottom-right ("bottom-right"))
     (:bottom-left ("bottom-left"))
     (:right ("side"))
     (:left ("side"))
     )
   (:transient-style
     ()
     (:parts-to-redraw-on-focus (:close :title-bar :top-left))
     (:close ("close-inactive" "close-active"))
     (:top ("top-inactive" "top-active"))
     (:top-left ("t-top-left-i" "t-top-left-a"))
     (:bottom ("bottom"))
     (:bottom-right ("bottom-right"))
     (:bottom-left ("bottom-left"))
     (:right ("side"))
     (:left ("side")))))

(defmethod draw-on-focus-in ((button title-bar))
  (with-slots (window item-to-draw gcontext master) button
    (multiple-value-bind (width height) (drawable-sizes window)
      (with-slots (frame-style) master
	(let ((top-pix (get-pixmap frame-style :top-active)))
	  (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	    (xlib:draw-rectangle window gcontext 0 0 width height t)))))
    (draw-centered-text window gcontext item-to-draw :color *white*)))

(defmethod draw-on-focus-out ((button title-bar))
  (with-slots (window item-to-draw gcontext) button
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white*)))