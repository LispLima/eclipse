(common-lisp:in-package :common-lisp-user)

(defpackage "STEP-ECLIPSE-THEME"
  (:use eclipse clx-ext common-lisp)
  (:size 10)
  (:export repaint initialize-frame)
  (:documentation ""))

(in-package "STEP-ECLIPSE-THEME")

(defvar +Step+ "Step")

(define-theme (+Step+)
  ((:default-style
     (:parts-to-redraw-on-focus (:close :title-bar :menu-button))
     ((:top ("top-inactive" "top-active"))
      (:menu-button ("menu-button-inactive"
		     "menu-button-active"
		     "menu-button-inactive-clicked"
		     "menu-button-active-clicked"))
      (:close ("close-inactive"
	       "close-active"
	       "close-inactive-clicked"
	       "close-active-clicked"))
      (:bottom ("bottom"))
      (:bottom-right ("bottom-right"))
      (:bottom-left ("bottom-left"))
      (:right ("side"))
      (:left ("side"))))
   (:transient-style
     (:parts-to-redraw-on-focus (:close :title-bar :top-left))
     ((:close ("close-inactive"
	       "close-active"
	       "close-inactive-clicked"
	       "close-active-clicked"))
      (:top ("top-inactive" "top-active"))
      (:top-left ("t-top-left-i" "t-top-left-a"))
      (:bottom ("bottom"))
      (:bottom-right ("bottom-right"))
      (:bottom-left ("bottom-left"))
      (:right ("side"))
      (:left ("side"))))))

(defmethod repaint ((widget title-bar) (theme Step) (focus t))
  (declare (ignorable theme focus))
  (with-slots ((window eclipse::window)
	       (item-to-draw eclipse::item-to-draw)
	       (gcontext eclipse::gcontext)
	       (master eclipse::master)) widget
    (multiple-value-bind (width height) (drawable-sizes window)
      (with-slots ((frame-style eclipse::frame-style)) master
	(let ((top-pix (get-pixmap frame-style :top-active)))
	  (xlib:with-gcontext (gcontext :tile top-pix :fill-style :tiled)
	    (xlib:draw-rectangle window gcontext 0 0 width height t)))))
    (draw-centered-text window gcontext item-to-draw :color *white*)))

(defmethod repaint ((widget title-bar) (theme Step) (focus null))
  (declare (ignorable theme focus))
  (with-slots ((window eclipse::window)
	       (item-to-draw eclipse::item-to-draw)
	       (gcontext eclipse::gcontext)) widget
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white*)))
