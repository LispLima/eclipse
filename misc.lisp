;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: misc.lisp,v 1.13 2003/11/13 11:12:28 ihatchondo Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2002 Iban HATCHONDO
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

(in-package :ECLIPSE-INTERNALS)

(defun home-subdirectory (directory)
  (concatenate
       'string
       #+:cmu "home:"
       #-:cmu (let ((homedir (user-homedir-pathname)))
		(or (when homedir (namestring homedir))
		    "~/"))
       directory))


;;;; Helpers macros.

(defmacro with-gensym (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defmacro screen-width ()
  `(xlib:screen-width (xlib:display-default-screen *display*)))

(defmacro screen-height ()
  `(xlib:screen-height (xlib:display-default-screen *display*)))

(defmacro id->atom-name (id)
  `(xlib:atom-name *display* ,id))

(defmacro atom-name->id (name)
 `(xlib:find-atom *display* ,name))

(defmacro with-root-cursor ((new-cursor) &body body)
  `(unwind-protect
       (progn
	 (setf (xlib:window-cursor *root-window*) ,new-cursor)
	 ,@body)
     (setf (xlib:window-cursor *root-window*) (root-default-cursor *root*))))

(defmacro make-error-handler ((type &key throw return (verbose t)) &body body)
  `(defun ,(intern (with-standard-io-syntax
		       (format nil "HANDLE-~a-CONDITION" type)))
       (condition)
     (declare (ignorable condition))
     ,@(when verbose
	`((format *stderr* "error - ~A - : ~A~%" ',type condition)
	  (finish-output *stderr*)))
     ,(unless return `(throw ',(or throw type) ,@(or body '(nil))))))

(make-error-handler (already-handled-xerror :verbose nil :throw general-error))
(make-error-handler (general-error))
(make-error-handler (invalid-wm-property))
(make-error-handler (end-of-file :throw end))
(make-error-handler (wrong-name :verbose nil)
  (timed-message-box *root-window* "Wrong application name"))

;;;; Property readers.
;; They are not defined in the clx-ext package, because we customize them
;; to allow a single manipulation for all properties that appears in the 
;; gnome and netwm protocols, or because it partially reads a property to
;; returns the part that is interesting in a window manager point of view.

(defun window-transient-p (app-window)
  (or (xlib:transient-for app-window)
      (member :_net_wm_window_type_dialog (netwm:net-wm-state app-window))))

(defun wm-state (window)
  (xlib:get-property window :WM_STATE))

(defsetf wm-state (window &key icon-id) (state)
  (let ((net-wm-state (gensym)))
    `(let ((,net-wm-state (netwm:net-wm-state ,window)))
       (if (or (= ,state 3) (= ,state 0))
	   (pushnew :_net_wm_state_hidden ,net-wm-state)
           (setf ,net-wm-state (delete :_net_wm_state_hidden ,net-wm-state)))
       (setf (netwm:net-wm-state ,window) ,net-wm-state)
       (xlib:change-property ,window
			     :WM_STATE
			     (list ,state ,icon-id)
			     :WM_STATE
			     32))))

(defun workspace-names (window)
  (or (netwm:net-desktop-names window) (gnome:win-workspace-names window)))

(defsetf workspace-names () (names)
  `(when ,names
     (setf (netwm:net-desktop-names *root-window*) ,names
           (gnome:win-workspace-names *root-window*) ,names)))

(defun wm-name (window)
  (or (ignore-errors (netwm:net-wm-name window))
      (ignore-errors (xlib:wm-name window))
      "incognito"))

(defun wm-icon-name (window)
  (or (ignore-errors (netwm:net-wm-icon-name window))
      (ignore-errors (xlib:wm-icon-name window))
      "incognito"))

(defun window-desktop-num (window)
  (or (netwm:net-wm-desktop window) (gnome:win-workspace window)))

(defsetf window-desktop-num (window) (n)
  `(setf (gnome:win-workspace ,window) ,n
	 (netwm:net-wm-desktop ,window) ,n))

(defun motif-wm-decoration (window)
  "Returns the state (or :on :off) of the :_MOTIF_WM_HINT property that
  indicates the application wants or not to be decorated."
  (let ((prop (xlib:get-property window :_MOTIF_WM_HINTS)))
    (or (and prop (logbitp 1 (first prop)) (zerop (third prop)) :OFF) :ON)))

(defun stick-p (window)
  (or (= (or (window-desktop-num window) -1) +any-desktop+)
      (logbitp 0 (or (gnome:win-state window :result-type t) 0))))

;;;; Miscellaneous functions.

(defun send-wm-protocols-client-message (window atom &rest data)
  "Send a client-message of type :wm-protocol to the specified window
  with data being the given atom plus the rest of the function args."
  (xlib:send-event window
		   :client-message
		   nil
		   :window window
		   :type :WM_PROTOCOLS
		   :format 32
		   :data (cons (atom-name->id atom) data)))

(defun screen-windows-layers (window &aux (i (window-desktop-num window)))
  "Returns, as multiple value, three window lists that corresponds to the
  three layers (:_net_wm_state_below none :_net_wm_state_above) of the
  virtual screen that the given `window' argument belongs to. The given
  window will be filtered."
  (loop with n = (if (eql i +any-desktop+) (current-desk) i)
	for w in (screen-content n)
        for nwm-state = (netwm:net-wm-state w)
        unless (xlib:window-equal w window)
          if (member :_net_wm_state_above nwm-state) collect w into aboves
	  else if (member :_net_wm_state_below nwm-state) collect w into belows
	  else collect w into no-stack-state
        finally (return (values belows no-stack-state aboves))))

(defsetf window-priority (window &optional sibling) (priority)
  "Set the window priority such as if done by (setf xlib:window-priority) and
  guaranty that stacking order constraints described in the extended window
  manager protocol will be respected. Then invokes update-client-list-stacking
  to reflect the new order in all the root properties that are involved in."
  (with-gensym (above-p wnwm-state snwm-state win sib b m a %priority)
    `(flet ((lookup-app-w (widget)
	      (when (decoration-p widget)
		(get-child widget :application :window t)))
	    (first (windows &optional above-p)
	      (car (if above-p (last windows) windows))))
       (let* ((,%priority ,priority)
	      (,win (or (lookup-app-w (lookup-widget ,window)) ,window))
	      (,sib (or (lookup-app-w (lookup-widget ,sibling)) ,sibling))
	      (,above-p (eq ,priority :above))
	      (,wnwm-state (netwm:net-wm-state ,win))
	      (,snwm-state (and ,sib (netwm:net-wm-state ,sib))))
	 (if (not (application-p (lookup-widget ,win)))
	     (setf (xlib:window-priority ,window ,sibling) ,priority)
	     (multiple-value-bind (,b ,m ,a) (screen-windows-layers ,win)
	       (cond ((member :_net_wm_state_below ,wnwm-state)
		      (unless (member ,sib ,b)
			(setf ,sib (first (or ,b ,m ,a) (and ,b ,above-p)))
			(unless ,b (setf ,%priority :below))))
		     ((member :_net_wm_state_above ,wnwm-state)
		      (unless (member ,sib ,a)
			(unless (member :_net_wm_state_fullscreen ,snwm-state)
			  (setf ,sib (first ,a ,above-p))
			  (unless ,a (setf ,%priority :above)))))
		     ((member :_net_wm_state_fullscreen ,wnwm-state)
		      (when (member ,sib ,b)
			(setf ,sib (first (or ,m ,a)))
			(setf ,%priority :below)))
		     ((not (member ,sib ,m))
		      (setf ,sib (first (or ,m ,b ,a) (if ,m ,above-p ,b)))
		      (unless ,m (setf ,%priority (if ,b :above :below)))))
	       (when (and ,sib (application-master (lookup-widget ,sib)))
		 (with-slots (master) (lookup-widget ,sib)
		   (setf ,sib (widget-window master))))
	       (when (or ,b ,m ,a)
		 (setf (xlib:window-priority ,window ,sib) ,%priority)
		 (update-client-list-stacking *root*))))))))

(defun grab-root-pointer (&key cursor owner-p confine-to)
  (xlib:grab-pointer
      *root-window*
      +pointer-event-mask+
      :confine-to confine-to
      :cursor (or cursor (root-default-cursor *root*))
      :owner-p owner-p))

(defun query-application-tree (root-window)
  "Returns the children of the specified root-window as if all applications
  where undecorated. The children are returned as a sequence of windows in
  current stacking order, from bottom-most (first) to top-most (last)."
  (loop for window in (xlib:query-tree root-window)
	for obj = (lookup-widget window)
	for appw = (typecase obj
		     (application window)
		     (decoration (get-child obj :application :window t)))
	when appw collect appw))

(defun delete-properties (window properties)
  "Delete a list of properties on the specified window."
  (mapc #'(lambda (prop) (xlib:delete-property window prop)) properties))

(defun run-application (program &rest arguments)
  "Returns a lambda of zero arguments which when funcalled will try to 
  run the program named `program' with arguments `arguments'. If the 
  invocation failed a pop-up window will appear reporting the error."
  (lambda ()
    (catch 'wrong-name
      (handler-bind ((error #'handle-wrong-name-condition))
	(%run-program% program arguments)))))

(defun make-viewport-property (n)
  (make-list (* 2 n) :initial-element 0))

;;;; Geometry structure and accessors.

(defstruct geometry
  (x 0 :type (signed-byte 16))
  (y 0 :type (signed-byte 16))
  (w 0 :type (unsigned-byte 16))
  (h 0 :type (unsigned-byte 16)))

(defun geometry-coordinates (geometry) 
  (values (geometry-x geometry) (geometry-y geometry)))

(defun geometry-sizes (geometry)
  (values (geometry-w geometry) (geometry-h geometry)))

(defsetf geometry (geometry) (x y w h)
  `(setf (geometry-x ,geometry) ,x   (geometry-y ,geometry) ,y
         (geometry-w ,geometry) ,w   (geometry-h ,geometry) ,h))

;;; application inspection functions

(defun application-list ()
  "Return the applications objects as a list."
  (loop for val being each hash-value in *widget-table*
	when (application-p val) collect val))

(defun application-name (app)
  (xlib:wm-name (widget-window app)))

(defun application-find (name)
  (car (member name (application-list) :test #'equal :key #'application-name)))

(defun application-class (app)
  (multiple-value-bind (name type) (xlib:get-wm-class (widget-window app))
    (cons name type)))

(defun application-class-name (app)
  (car (application-class app)))

(defun application-class-type (app)
  (cdr (application-class app)))

;;;; misc.lisp ends here.
