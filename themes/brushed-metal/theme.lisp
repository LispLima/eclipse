(in-package :ECLIPSE-INTERNALS)

(define-theme ("brushed-metal")
  ((:default-style
     ()
     (:parts-to-redraw-on-focus :all)
     (:top ("top-i" "top-a"))
     (:top-left ("top-left-i" "top-left-a"))
     (:top-right ("top-right-i" "top-right-a"))
     (:menu-button ("menu-button-i" "menu-button-a"))
     (:close ("close-i" "close-a"))
     (:icon-b ("minimize-i" "minimize-a"))
     (:maximize ("maximize-i" "maximize-a"))
     (:bottom ("bottom-i" "bottom-a"))
     (:bottom-right ("bottom-right-i" "bottom-right-a"))
     (:bottom-left ("bottom-left-i" "bottom-left-a"))
     (:right ("right-i" "right-a"))
     (:left ("left-i" "left-a"))
     (:custom ("title-left-i" "title-left-a" "title-right-i" "title-right-a"))
     )
   (:transient-style
     ()
     (:parts-to-redraw-on-focus :all)
     (:top ("t-top-i" "t-top-a"))
     (:top-left ("t-top-left-i" "t-top-left-a"))
     (:top-right ("t-top-right-i" "t-top-right-a"))
     (:bottom ("t-bottom-i" "t-bottom-a"))
     (:bottom-right ("t-bottom-right-i" "t-bottom-right-a"))
     (:bottom-left ("t-bottom-left-i" "t-bottom-left-a"))
     (:right ("t-right-i" "t-right-a"))
     (:left ("t-left-i" "t-left-a")))))

(defun default-draw-on-focus-in (title-bar frame-style)
  (with-slots (window item-to-draw gcontext) title-bar
    (xlib:clear-area window)
    (let ((top-pix (get-pixmap frame-style :top-a))
	  (left (get-pixmap frame-style :title-left-a))
	  (right (get-pixmap frame-style :title-right-a)))
      (multiple-value-bind (width height) (drawable-sizes window)
	(xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	  (xlib:draw-rectangle window gcontext 0 0 width height t))
	(multiple-value-bind (w h) (drawable-sizes left)
	  (xlib:copy-area left gcontext 0 0 w h window 0 0))
	(multiple-value-bind (w h) (drawable-sizes right)
	  (xlib:copy-area right gcontext 0 0 w h window (- width w) 0))))
    (draw-centered-text window gcontext item-to-draw :color *black* :x 12)))

(defun default-draw-on-focus-out (title-bar frame-style)
  (with-slots (window item-to-draw gcontext) title-bar
    (xlib:clear-area window)
    (let ((left (get-pixmap frame-style :title-left-i))
	  (right (get-pixmap frame-style :title-right-i))
	  (width (xlib:drawable-width window)))
      (multiple-value-bind (w h) (drawable-sizes left)
	(xlib:copy-area left gcontext 0 0 w h window 0 0))
      (multiple-value-bind (w h) (drawable-sizes right)
	(xlib:copy-area right gcontext 0 0 w h window (- width w) 0)))
    (draw-centered-text window gcontext item-to-draw :color *black* :x 12)))

(defun transient-draw-on-focus-in (title-bar frame-style)
  (with-slots (window item-to-draw gcontext) title-bar
    (xlib:clear-area window)
    (xlib:with-gcontext 
	(gcontext :tile (get-pixmap frame-style :t-top-a) :fill-style :tiled)
      (multiple-value-bind (width height) (drawable-sizes window)
	(xlib:draw-rectangle window gcontext 0 0 width height t)))))

(defun transient-draw-on-focus-out (title-bar)
  (xlib:clear-area (widget-window title-bar)))

(defmethod draw-on-focus-in ((button title-bar))
  (with-slots (frame-style) (button-master button)
    (typecase frame-style
      (default-style (default-draw-on-focus-in button frame-style))
      (transient-style (transient-draw-on-focus-in button frame-style)))))

(defmethod draw-on-focus-out ((button title-bar))
  (with-slots (frame-style) (button-master button)
    (typecase frame-style
      (default-style (default-draw-on-focus-out button frame-style))
      (transient-style (transient-draw-on-focus-out button)))))