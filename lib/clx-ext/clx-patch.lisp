;;; -*- Mode: Lisp -*-
;;; $Id: clx-patch.lisp,v 1.1 2002/11/07 14:22:42 hatchond Exp $
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

#|

This patch is also buggy: trying to find a window returns unexisting window
structure at X server level. But as already said 
(see http://www.mail-archive.com/cmucl-help@cons.org/msg00173.html) reversing 
(xlib::or-get 8 window (member :none :pointer-root)) for 
(xlib::or-get 8 (member :none :pointer-root) window) doesn't work.
Indeed O or 1 are inappropriated ID's.

(defun xlib:input-focus (display) 
  (declare (type xlib:display display)) 
  (declare (xlib::clx-values focus revert-to)) 
  (xlib::with-buffer-request-and-reply
      (display xlib::*x-getinputfocus* 16 :sizes (8 32)) 
    () 
    (values 
      (xlib::or-get 8 window (member :none :pointer-root)) 
      (xlib::member8-get 1 :none :pointer-root :parent))))

|#

(defun xlib:input-focus (display) 
  (declare (type xlib:display display)) 
  (declare (xlib::clx-values focus revert-to)) 
  (xlib::with-buffer-request-and-reply
      (display xlib::*x-getinputfocus* 16 :sizes (8 32)) 
    ()     
    (values 
      (let ((id (xlib::card29-get 8)))
	(declare (type xlib::card29 id))
	(case id 
	  (0 :none)
	  (1 :pointer-root)
	  (t (xlib::lookup-window display id))))
      (xlib::member8-get 1 :none :pointer-root :parent))))
