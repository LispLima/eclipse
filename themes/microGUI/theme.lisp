(in-package :ECLIPSE-INTERNALS)

(define-theme ("microGUI")
  ((:default-style
     ()
     (:parts-to-redraw-on-focus 
       (:close :icon-b :maximize 
	:title-bar :top-left :menu-button))
     (:top ("center"))
     (:menu-button ("menu-normal" "menu-active"))
     (:icon-b ("iconify-normal" "iconify-active"))
     (:maximize ("maximize-normal" "maximize-active"))
     (:close ("close-normal" "close-active"))
     (:top-left ("top-left-inactive" "top-left"))
     (:top-right ("top-right"))
     (:bottom ("bottom"))
     (:bottom-right ("bottom-right"))
     (:bottom-left ("bottom-left"))
     (:right ("right"))
     (:left ("left"))
     (:custom ("top-curves-inactive" "top-curves" 
	       "top-blue" "top-blue-inactive"))
     )
   (:transient-style
     (:title-bar-position :right)
     (:parts-to-redraw-on-focus (:close))
     (:top ("t-top"))
     (:close ("t-close-normal" "t-close-active"))
     (:top-left ("t-top-left"))
     (:top-right ("t-top-right"))
     (:bottom ("t-bottom"))
     (:bottom-right ("t-bottom-right"))
     (:bottom-left ("t-bottom-left"))
     (:right ("t-right"))
     (:left ("t-left")))))

(defun redraw-title-bar (button top-pix curv-pix)
  (with-slots (window gcontext item-to-draw master) button
    (xlib:clear-area window)
    (let ((w (+ 10 (text-width (xlib:gcontext-font gcontext) item-to-draw)))
	  (h (xlib:drawable-height window))
	  (pix-w (xlib:drawable-width curv-pix)))
      (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	(xlib:draw-rectangle window gcontext 0 0 w h t))
      (xlib:with-gcontext (gcontext :tile curv-pix :fill-style :tiled :ts-x w)
	(xlib:draw-rectangle window gcontext w 0 pix-w h t))
      (draw-centered-text window gcontext item-to-draw :color *white* :x 5))))

(defmethod draw-on-focus-in ((button title-bar))
  (with-slots (frame-style) (button-master button)
    (when (default-style-p frame-style)
      (redraw-title-bar button
			(get-pixmap frame-style :top-blue)
			(get-pixmap frame-style :top-curves)))))

(defmethod draw-on-focus-out ((button title-bar))
  (with-slots (frame-style) (button-master button)
    (when (default-style-p frame-style)
      (redraw-title-bar button
			(get-pixmap frame-style :top-blue-inactive)
			(get-pixmap frame-style :top-curves-inactive)))))
