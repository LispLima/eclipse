(common-lisp:in-package :common-lisp-user)

(defpackage "MICROGUI-ECLIPSE-THEME"
  (:use eclipse clx-ext common-lisp)
  (:size 10)
  (:export repaint initialize-frame)
  (:documentation ""))

(in-package "MICROGUI-ECLIPSE-THEME")

(defvar +microGUI+ "microGUI")

(eclipse:define-theme (+microGUI+)
  ((:default-style
     (:parts-to-redraw-on-focus 
      (:close :icon-b :maximize :title-bar :top-left :menu-button))
     ((:top ("center"))
      (:menu-button ("menu-normal"
		     "menu-active"
		     "menu-normal-clicked"
		     "menu-active-clicked"))
      (:icon-b ("iconify-normal"
		"iconify-active"
		"iconify-normal-clicked"
		"iconify-active-clicked"))
      (:maximize ("maximize-normal"
		  "maximize-active"
		  "maximize-normal-clicked"
		  "maximize-active-clicked"))
      (:close ("close-normal"
	       "close-active"
	       "close-normal-clicked"
	       "close-active-clicked"))
      (:top-left ("top-left-inactive" "top-left"))
      (:top-right ("top-right"))
      (:bottom ("bottom"))
      (:bottom-right ("bottom-right"))
      (:bottom-left ("bottom-left"))
      (:right ("right"))
      (:left ("left"))
      (:custom ("top-curves-inactive"
		"top-curves" 
		"top-blue"
		"top-blue-inactive"))))
   (:transient-style
     (:title-bar-position :right)
     (:parts-to-redraw-on-focus (:close))
     ((:top ("t-top"))
      (:close ("t-close-normal"
	       "t-close-active"
	       "t-close-normal-clicked"
	       "t-close-active-clicked"))
      (:top-left ("t-top-left"))
      (:top-right ("t-top-right"))
      (:bottom ("t-bottom"))
      (:bottom-right ("t-bottom-right"))
      (:bottom-left ("t-bottom-left"))
      (:right ("t-right"))
      (:left ("t-left"))))))

(defun redraw-title-bar (button top-pix curv-pix)
  (with-slots ((window eclipse::window)
	       (gcontext eclipse::gcontext)
	       (item-to-draw eclipse::item-to-draw)
	       (master eclipse::master)) button
    (xlib:clear-area window)
    (let ((w (+ 10 (text-width (xlib:gcontext-font gcontext) item-to-draw)))
	  (h (xlib:drawable-height window))
	  (pix-w (xlib:drawable-width curv-pix)))
      (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	(xlib:draw-rectangle window gcontext 0 0 w h t))
      (xlib:with-gcontext (gcontext :tile curv-pix :fill-style :tiled :ts-x w)
	(xlib:draw-rectangle window gcontext w 0 pix-w h t))
      (draw-centered-text window gcontext item-to-draw :color *white* :x 5))))

(defmethod repaint ((widget title-bar) (name (eql +microGUI+)) (focus t))
  (declare (ignorable name focus))
  (with-slots ((frame-style eclipse::frame-style)) (button-master widget)
    (when (default-style-p frame-style)
      (redraw-title-bar widget
			(get-pixmap frame-style :top-blue)
			(get-pixmap frame-style :top-curves)))))

(defmethod repaint ((widget title-bar) (name (eql +microGUI+)) (focus null))
  (declare (ignorable name focus))
  (with-slots ((frame-style eclipse::frame-style)) (button-master widget)
    (when (default-style-p frame-style)    
      (redraw-title-bar widget
			(get-pixmap frame-style :top-blue-inactive)
			(get-pixmap frame-style :top-curves-inactive)))))
