;;; -*- Mode: Lisp; Package: CLX-EXTENSIONS -*-
;;; $Id: clx-extensions.lisp,v 1.2 2003/08/28 14:44:40 hatchond Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2001, 2002 Iban HATCHONDO
;;; contact : hatchond@yahoo.fr
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Some CLX extensions.

(in-package :clx-extensions)

(declaim (inline window-geometry window-size window-position))

(defun window-geometry (window)
  (values (xlib:drawable-x window) (xlib:drawable-y window)
	  (xlib:drawable-width window) (xlib:drawable-height window)))

(defun drawable-sizes (window)
  (values (xlib:drawable-width window) (xlib:drawable-height window)))

(defun window-position (window) 
  (values (xlib:drawable-x window) (xlib:drawable-y window)))

(defsetf drawable-sizes (window) (width height)
  `(setf (xlib:drawable-width ,window) ,width
         (xlib:drawable-height ,window) ,height))

(defsetf window-position (window) (x y)
  `(setf (xlib:drawable-x ,window) ,x
	 (xlib:drawable-y ,window) ,y))

(defmacro with-event-mask ((window &optional (ev-mask 0)) &body body)
  (let ((original-mask (gensym)))
    `(let ((,original-mask (xlib:window-event-mask ,window)))
       (unwind-protect
	    (progn 
	      (setf (xlib:window-event-mask ,window) ,ev-mask)
	      ,@body)
	 (setf (xlib:window-event-mask ,window) ,original-mask)))))

(defmacro with-pointer-grabbed
    ((window pointer-event-mask &key confine-to cursor owner-p) &body body)
  "Grabs the display pointer only within the dynamic extent of the body. 
   Ungrab-pointer is automatically called upon exit from the body. 
   Arguments and values are exactly those described in grab-pointer."
  `(unwind-protect
	(progn      
	  (xlib:grab-pointer 
	      ,window
	      ,pointer-event-mask
	      :confine-to ,confine-to
	      :cursor ,cursor
	      :owner-p ,owner-p)
	  ,@body)
     (xlib:ungrab-pointer (xlib:drawable-display ,window))))

(defun get-environment-variable (&optional (string "DISPLAY"))
  ;; Add some other system.
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #-(or excl cmu clisp sbcl) (error "GET-ENVIRONMENT-VARIABLE not implemented")
  )

(defun open-clx-display (&optional string)
  "Parses a display specification including display and screen numbers.
   This returns nil when there is no DISPLAY environment variable. If string
   is non-nil, and any fields are missing in the specification, this signals an
   error. If you specify a screen, then this sets XLIB:DISPLAY-DEFAULT-SCREEN
   to that screen since CLX initializes this form to the first of
   XLIB:SCREEN-ROOTS. This returns the display and screen objects."
  (unless string (setf string (get-environment-variable)))
  (unless string (error "No display specification available"))
  (let* ((string (coerce string 'simple-string))
	 (length (length string))
	 (host-name "")
	 (protocol #+sbcl :local #-sbcl :unix)
	 (auth-name nil)
	 (auth-data nil)
	 (dpy-num nil)
	 (screen-num nil))
    (declare (simple-string string))
    (let* ((colon (position #\: string :test #'char=))
	   (dot (position #\. string :test #'char= :start (1+ (or colon 0))))
	   (dot-2 (position #\. string :test #'char= :start (1+ (or dot 0)))))
      (cond ((null colon)
	     (error "Missing display number in DISPLAY env variable."))
	    ((= (1+ colon) (or dot length))
	     (error "Badly formed display number in DISPLAY env variable."))
	    ((and dot (= (1+ dot) (or dot-2 length)))
	     (error "Badly formed screen number in DISPLAY env variable."))
	    (t
	     (unless (zerop colon) (setf host-name (subseq string 0 colon)))
	     (incf colon)
	     (setf dpy-num (parse-integer string :start colon :end dot))
	     (when dot
	       (setf screen-num
		     (parse-integer string :start (1+ dot) :end dot-2))))))
    (if (or (equal host-name "unix") (equal host-name ""))
	(multiple-value-setq (auth-name auth-data)
	  (xlib::get-best-authorization (machine-instance) dpy-num protocol))
	(setf protocol :internet))
    (let ((display (xlib:open-display host-name
				      :display dpy-num
				      :protocol protocol
				      :authorization-name auth-name
				      :authorization-data auth-data)))
      (when screen-num
	(let* ((screens (xlib:display-roots display))
	       (num-screens (length screens)))
	  (when (>= screen-num num-screens)
	    (xlib:close-display display)
	    (error "No such screen number (~D)." screen-num))
	  (setf (xlib:display-default-screen display)
		(elt screens screen-num))))
      (values display (xlib:display-default-screen display)))))


(defun grab-button (window button event-mask
		    &key (modifiers #x8000) owner-p 
		         sync-pointer-p sync-keyboard-p 
		         confine-to cursor)
  (xlib:grab-button window button event-mask
		    :modifiers modifiers :owner-p owner-p
		    :sync-pointer-p sync-pointer-p 
		    :sync-keyboard-p sync-keyboard-p
		    :confine-to confine-to :cursor cursor))

(defun draw-glyphs (drawable gctxt x y seq
		    &key (start 0) end 
		         width (size :default) 
		         (translate #'translate))
  (xlib:draw-glyphs drawable gctxt x y seq
		    :start start :end end
		    :width width :size size
		    :translate translate))

(defun translate (src src-start src-end afont dst dst-start)
  ;; This is for replacing the clx-translate-default-function
  ;; who does'nt know about accentated characters because
  ;; of a call to cl:graphic-char-p that return nil with accentated characters.
  ;; For further informations, on a clx-translate-function, see the clx-man.
  (declare (type sequence src)
	   (type xlib:array-index src-start src-end dst-start)
	   (type (or null xlib:font) afont)
	   (type vector dst))
  (declare
      (xlib::clx-values integer (or null integer xlib:font) (or null integer)))
  afont
  (loop with min-char-index = (xlib:font-min-char afont)
	and max-char-index = (xlib:font-max-char afont)
	and str-p = (stringp src)
	for i of-type xlib:array-index from src-start below src-end
	for j of-type xlib:array-index from dst-start
	for char = (if str-p (char src i) (elt src i))
	when (characterp char) do (setq char (xlib:char->card8 char))
	if (and (integerp char) (<= min-char-index char max-char-index))
	do (setf (aref dst j) char)
	else do (loop-finish)
	finally (return i)))

(defun text-width (font seq)
  (xlib:text-width font seq :translate #'translate))

(defun draw-centered-text (window gctxt seq &key color x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (or null (signed-byte 16)) x y))
  (declare (inline draw-glyphs))
  (multiple-value-bind (text-w text-h)
      (xlib:text-extents (xlib:gcontext-font gctxt) seq :translate #'translate)
    (declare (type (unsigned-byte 16) text-w text-h))
    (multiple-value-bind (width height) (drawable-sizes window)
      (declare (type (unsigned-byte 16) width height))
      (unless x (setq x (ash (- width text-w) -1)))
      (unless y (setq y (- height (ash (- height text-h) -1)))))
    (xlib:with-gcontext (gctxt :foreground color)
      (draw-glyphs window gctxt (max x 0) y seq :width text-w))))

(defun send-configuration-notify (window)
  (multiple-value-bind (x y)
      (xlib:translate-coordinates window 0 0 (xlib:drawable-root window))
    (xlib:send-event window
		     :configure-notify
		     (xlib:make-event-mask :structure-notify)
		     :event-window window :window window
		     :x x :y y
		     :override-redirect-p nil
		     :border-width (xlib:drawable-border-width window)
		     :width (xlib:drawable-width window)
		     :height (xlib:drawable-height window)
		     :propagate-p nil)))

(defun wm-hints-icon-pixmap (window)
  (let ((hint (xlib:get-property window :WM_HINTS :result-type 'vector)))
    (when (and hint (logbitp 2 (aref hint 0)))
      (xlib::lookup-pixmap (xlib:window-display window) (aref hint 3)))))
