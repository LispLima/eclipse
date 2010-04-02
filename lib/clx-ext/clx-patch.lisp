;;; -*- Mode: Lisp -*-
;;; $Id: clx-patch.lisp,v 1.7 2009-11-17 17:29:13 ihatchondo Exp $
;;;
;;; This file contains the patch fixing a bug in CLX as distributed
;;; with vanilla CMUCL versions up to 18d.

(in-package :xlib)

(xlib:declare-event :configure-request
  ((xlib::data (xlib::member8 :above :below :top-if :bottom-if :opposite))
   stack-mode)
  (xlib:card16 sequence)
  (xlib:window (parent event-window) window)
  ((or null xlib:window) above-sibling)
  (xlib:int16 x y)
  (xlib:card16 width height border-width value-mask))

(defconstant _x_getinputfocus_ 43)

#|

This patch is also buggy: trying to find a window returns unexisting window
structure at X server level. But as already said 
(see http://www.mail-archive.com/cmucl-help@cons.org/msg00173.html) reversing 
(xlib::or-get 8 window (member :none :pointer-root)) for 
(xlib::or-get 8 (member :none :pointer-root) window) doesn't work.
Indeed O or 1 are inappropriated ID's.

(defun xlib:input-focus (dpy) 
  (declare (type xlib:display dpy)) 
  (declare (xlib::clx-values focus revert-to)) 
  (xlib::with-buffer-request-and-reply (dpy _x_getinputfocus_ 16 :sizes (8 32))
    () 
    (values 
      (xlib::or-get 8 window (member :none :pointer-root)) 
      (xlib::member8-get 1 :none :pointer-root :parent))))

|#

(defun xlib:input-focus (dpy) 
  (declare (type xlib:display dpy)) 
  (declare (xlib::clx-values focus revert-to)) 
  (xlib::with-buffer-request-and-reply (dpy _x_getinputfocus_ 16 :sizes (8 32))
    ()     
    (values 
      (let ((id (xlib::card29-get 8)))
	(declare (type xlib::card29 id))
	(case id 
	  (0 :none)
	  (1 :pointer-root)
	  (t (xlib::lookup-window dpy id))))
      (xlib::member8-get 1 :none :pointer-root :parent))))

;; This is a patch for the equality functions. Not accepting at least
;; nil arguments is pretty anoying. It will still complain if at least
;; one is not of the proper type but this should save us from lots of 
;; testing.

;; (macrolet ((make-mumble-equal (type)
;; 	     ;; Since caching is only done for objects created by the
;; 	     ;; client, we must always compare ID and display for
;; 	     ;; non-identical mumbles.
;; 	     (let ((predicate (xintern type '-equal))
;; 		   (id (xintern type '-id))
;; 		   (dpy (xintern type '-display)))
;; 		`(within-definition (,type make-mumble-equal)
;; 		   (defun ,predicate (a b)
;; 		     (declare (type (or null ,type) a b))
;;                      (when (and a b)
;;                        (or (eql a b)
;;                            (and (= (,id a) (,id b))
;;                                 (eq (,dpy a) (,dpy b))))))))))
;;   (make-mumble-equal window)
;;   (make-mumble-equal pixmap)
;;   (make-mumble-equal cursor)
;;   (make-mumble-equal font)
;;   (make-mumble-equal gcontext)
;;   (make-mumble-equal colormap)
;;   (make-mumble-equal drawable))

;; It seems that sometimes some id are still present in the clx display
;; internal cache even when those resources have been destroyed. This has
;; for effect that when the X server reallocate this id to a resource of
;; another type than the previous one it provokes some internal error inside
;; CLX. This patch correct this strange behavior. I know this not very 
;; satisfying but I can't see another solution at the present time.

(defun lookup-window (display id)
   (declare (type display display) (type resource-id id))
   (declare (clx-values window))
   (let ((window (lookup-resource-id display id)))
     (cond
      ((null window) (setq window (make-window :display display :id id))
       (save-id display id window))
      ((not (window-p window))
       (deallocate-resource-id-internal display id)
       (setq window (make-window :display display :id id))
       (save-id display id window))
      (t window))))

(defun lookup-pixmap (display id)
  (declare (type display display) (type resource-id id))
  (declare (clx-values pixmap))
  (let ((pixmap (lookup-resource-id display id)))
    (cond
      ((null pixmap) (setq pixmap (make-pixmap :display display :id id))
       (save-id display id pixmap))
      ((not (pixmap-p pixmap))
       (deallocate-resource-id-internal display id)
       (setq pixmap (make-pixmap :display display :id id))
       (save-id display id pixmap))
      (t pixmap))))
