;;; -*- Mode: Lisp -*-
;;; $Id: clx-patch.lisp,v 1.2 2002/06/24 07:30:33 james Exp $
;;;
;;; This file contains the patch fixing a bug in CLX as distributed
;;; with vanilla CMUCL versions up to 18d.

(xlib:declare-event :configure-request
  ((xlib::data (xlib::member8 :above :below :top-if :bottom-if :opposite))
   stack-mode)
  (xlib:card16 sequence)
  (xlib:window (parent event-window) window)
  ((or null xlib:window) above-sibling)
  (xlib:int16 x y)
  (xlib:card16 width height border-width value-mask))

(defun xlib:input-focus (display) 
  (declare (type xlib:display display)) 
  (declare (xlib::clx-values focus revert-to)) 
  (xlib::with-buffer-request-and-reply
      (display xlib::*x-getinputfocus* 16 :sizes (8 32)) 
    () 
    (values 
      (xlib::or-get 8 window (member :none :pointer-root)) 
      (xlib::member8-get 1 :none :pointer-root :parent))))
