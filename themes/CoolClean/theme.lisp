(in-package :ECLIPSE-INTERNALS)

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

(defmethod draw-on-focus-in ((button title-bar))
  (with-slots (window item-to-draw gcontext master) button
    (multiple-value-bind (width height) (drawable-sizes window)
      (with-slots (frame-style) master
	(let ((top-pix (get-pixmap frame-style :top-active)))
	  (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	    (xlib:draw-rectangle window gcontext 0 0 width height t)))))
    (draw-centered-text window gcontext item-to-draw :color *white* :x 5)))

(defmethod draw-on-focus-out ((button title-bar))
  (with-slots (window item-to-draw gcontext) button
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white* :x 5)))