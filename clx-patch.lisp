;;; -*- Mode: Lisp -*-
;;; $Id$
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

