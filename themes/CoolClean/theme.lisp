(common-lisp:in-package :common-lisp-user)

(defpackage "COOLCLEAN-ECLIPSE-THEME"
  (:use eclipse clx-ext common-lisp)
  (:size 10)
  (:export repaint initialize-frame)
  (:documentation ""))

(in-package "COOLCLEAN-ECLIPSE-THEME")

(defvar +CoolClean+ "CoolClean")

(define-theme (+CoolClean+)
  ((:default-style
     (:parts-to-redraw-on-focus (:title-bar))
     ((:top ("top-inactive" "top-active"))
      (:top-right ("top-right"))
      (:menu-button ("menu-button-active"))
      (:close ("close-active"
	       "close-active"
	       "close-active-clicked"
	       "close-active-clicked"))
      (:icon-b ("minimize-active"
		"minimize-active"
		"minimize-active-clicked"
		"minimize-active-clicked"))
      (:maximize ("maximize-active"
		  "maximize-active"
		  "maximize-active-clicked"
		  "maximize-active-clicked"))
      (:bottom ("bottom"))
      (:bottom-right ("bottom-right"))
      (:bottom-left ("bottom-left"))
      (:right ("right"))
      (:left ("left"))))
   (:transient-style
     (:parts-to-redraw-on-focus (:title-bar))
     ((:top ("top-inactive" "top-active"))
      (:close ("t-close-active"
	       "t-close-active"
	       "t-close-active-clicked"
	       "t-close-active-clicked"))
      (:menu-button ("menu-button-active"))
      (:top-right ("top-right"))
      (:bottom ("bottom"))
      (:bottom-right ("bottom-right"))
      (:bottom-left ("bottom-left"))
      (:right ("right"))
      (:left ("left"))))))

(defmethod repaint ((widget title-bar) (theme CoolClean) (focus t))
  (declare (ignorable theme focus))
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

(defmethod repaint ((widget title-bar) (theme CoolClean) (focus null))
  (declare (ignorable theme focus))
  (with-slots ((window eclipse::window)
	       (item-to-draw eclipse::item-to-draw)
	       (gcontext eclipse::gcontext)) widget
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white* :x 5)))
