;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: misc.lisp,v 1.48 2010-04-02 09:57:53 ihatchondo Exp $
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

(defmacro current-desk () `(current-vscreen *root-window*))

(defmacro id->atom-name (id)
  `(when (typep ,id 'xlib:card29)
     (xlib:atom-name *display* ,id)))

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
          ;; #+:cmu (debug::backtrace most-positive-fixnum *stderr*)
          ;; #+:sbcl (sb-debug:backtrace most-positive-fixnum *stderr*)
          ;; #+:clisp (system::print-backtrace :out *stderr*)
	  (finish-output *stderr*)))
     ,(unless return `(throw ',(or throw type) ,@(or body '(nil))))))

(make-error-handler (error :return t))
(make-error-handler (end-of-file :return t))

;;;; Window hashtable
;; Wrapper functions over hashtable using xlib:window as hash keys. 
;; Since we can't pass a custom predicate to make-hash-table, those
;; wrappers are there to deal with it using window-id and null window.

(defun getwinhash (window hashtable &optional default)
  (declare (type (or null xlib:window) window))
  (declare (type hash-table hashtable))
  (declare (optimize (speed 3) (safety 0)))
  (gethash (and window (xlib:window-id window)) hashtable default))

(defun remwinhash (window hashtable)
  (declare (type (or null xlib:window) window))
  (declare (type hash-table hashtable))
  (declare (optimize (speed 3) (safety 0)))
  (remhash (and window (xlib:window-id window)) hashtable))

(defsetf getwinhash (window hashtable &optional default) (value)
  (let ((key (gensym)))
    `(let ((,key ,window))
       (setf (gethash (and ,key (xlib:window-id ,key)) ,hashtable ,default)
             ,value))))

;;;; Property readers.
;; They are not defined in the clx-ext package, because we customize them
;; to allow a single manipulation for all properties that appears in the 
;; gnome and netwm protocols, or because it partially reads a property to
;; returns the part that is interesting in a window manager point of view.

(defun window-transient-p (app-window)
  "Returns true if the window is considered as transient for another one.
   A window is considered as transient if its wm_transient_for property is
   set, or if it has the _net_wm_window_type_dialog atom present in its 
   _net_wm_state property."
  (or (xlib:transient-for app-window)
      (member :_net_wm_window_type_dialog (netwm:net-wm-state app-window))))

(defun wm-normal-hints (window)
  "Returns the window WM_NORMAL_HINTS property with the obsolete values 
   reset to the window geometry."
  (let ((hint (ignore-errors (xlib:wm-normal-hints window))))
    (unless (null hint)
      (multiple-value-bind (x y w h) (window-geometry window)
        (setf (xlib:wm-size-hints-x hint) x
              (xlib:wm-size-hints-y hint) y
              (xlib:wm-size-hints-width hint) w
              (xlib:wm-size-hints-height hint) h)))
    hint))

(defun wm-state (window)
  "Returns the wm_state property of a window as xlib:get-property would."
  (xlib:get-property window :WM_STATE))

(defsetf wm-state (window &key (icon-id 0)) (state)
  "Sets the wm_state property of a window. Note that its _net_wm_state property
   will be updated accordingly to the value given for the wm_state."
  (let ((net-wm-state (gensym)))
    `(let* ((,net-wm-state (netwm:net-wm-state ,window)))
       (if (or (= ,state 3) (= ,state 0))
	   (pushnew :_net_wm_state_hidden ,net-wm-state)
           (setf ,net-wm-state (delete :_net_wm_state_hidden ,net-wm-state)))
       (setf (netwm:net-wm-state ,window) ,net-wm-state)
       (xlib:change-property ,window :WM_STATE
	   (list ,state ,icon-id)
	   :WM_STATE
	   32))))

(defun workspace-names (window)
  "Returns the workspace names according to the first property that is set
   between _net_wm_desktop_names and _win_workspace_names respectively."
  (or (netwm:net-desktop-names window) (gnome:win-workspace-names window)))

(defsetf workspace-names (window) (names)
  "Sets both the _win_workspace_names and the _net_wm_desktop_names properties
   to the given list of name."
  `(when ,names
     (setf (netwm:net-desktop-names ,window) ,names
           (gnome:win-workspace-names ,window) ,names)))

(defun wm-name (window)
  "Returns the name of the window according to the first property that is set
   between _net_wm_name and wm_name respectively. If none, \"incognito\" is
   returned."
  (or (ignore-errors (netwm:net-wm-name window))
      (ignore-errors (xlib:wm-name window))
      "incognito"))

(defun wm-icon-name (window)
  "Returns the icon name of the window according to the first property that
   is set between _net_wm_icon_name and wm_icon_name respectively. If none,
   \"incognito\" is returned."
  (or (ignore-errors (netwm:net-wm-icon-name window))
      (ignore-errors (xlib:wm-icon-name window))
      "incognito"))

(defun decode-netwm-icon-pixmap (window property-vector)
  "Returns a pixmap containing the first icon of the property or NIL."
  ;;(declare (optimize (speed 3) (safety 1)))
  (declare (type (or null (simple-vector *)) property-vector))
  (unless property-vector (return-from decode-netwm-icon-pixmap nil))
  (loop with depth of-type ppm::card-8 = (xlib:drawable-depth window)
	with bits-per-pixel = (ppm::find-bits-per-pixel depth)
	with type = `(unsigned-byte ,bits-per-pixel)
	with width of-type ppm::card-16 = (aref property-vector 0)
	with height of-type ppm::card-16 = (aref property-vector 1)
	with size of-type ppm::card-32 = (* width height)
	with data = (make-array `(,height ,width) :element-type type)
	with tmp = (make-array size :displaced-to data :element-type type)
	for i of-type ppm::card-32 from 2 below (+ 2 size)
	for argb of-type ppm::card-32 = (aref property-vector i)
	do (setf (aref tmp (- i 2))
		 (ppm::color->x-color (logand argb #xFFFFFF)))
	finally (return
		  (xlib:image-pixmap
		      window
		      (xlib:create-image
		          :width width :height height :depth depth
			  :bits-per-pixel bits-per-pixel :data data)))))

(defun window-desktop-num (window)
  "Returns the desktop number according to the first property that is set
   between _net_wm_desktop and _win_workspace respectively."
  (or (netwm:net-wm-desktop window) (gnome:win-workspace window)))

(defsetf window-desktop-num (window) (n)
  "Sets both the _win_workspace and the _net_wm_desktop properties of the 
   specified window to n."
  `(setf (gnome:win-workspace ,window) ,n
	 (netwm:net-wm-desktop ,window) ,n))

(defun motif-wm-decoration (window)
  "Returns the state (or :on :off) of the :_MOTIF_WM_HINT property that
   indicates the application wants or not to be decorated."
  (let ((prop (xlib:get-property window :_MOTIF_WM_HINTS)))
    (or (and prop (logbitp 1 (first prop)) (zerop (third prop)) :OFF) :ON)))

(defun stick-p (window)
  "Returns T if the window has a desktop-number equal to +any-desktop+ or
   if it has win_state_sticky on."
  (or (= (or (window-desktop-num window) -1) +any-desktop+)
      (logbitp 0 (or (gnome:win-state window :result-type t) 0))))

(defun net-wm-user-time (window) 
  "Returns the _net_wm_user_time property using the _net_wm_user_time_window
   if present. If the property is not defined return NIL."
  (let ((user-time-window (netwm:net-wm-user-time-window window)))
    (netwm:net-wm-user-time (or user-time-window window))))

(defsetf net-wm-user-time (window) (timestamp)
  "Sets the _net_wm_user_time property using the _net_wm_user_time_window
   if present, otherwise window is the property holder."
  (let ((time-window (gensym)))
    `(let ((,time-window (netwm:net-wm-user-time-window ,window)))
       (setf (netwm:net-wm-user-time (or ,time-window ,window)) ,timestamp))))

;;;; Miscellaneous functions.

(defun grab-root-pointer (&key cursor owner-p confine-to)
  (xlib:grab-pointer
      *root-window*
      +pointer-event-mask+
      :confine-to confine-to
      :cursor (or cursor (root-default-cursor *root*))
      :owner-p owner-p))

(defun send-wm-protocols-client-message (window atom &rest data)
  "Send a client-message of type :wm-protocol to the specified window
   with data being the given atom plus the rest of the function args."
  (xlib:send-event window :client-message
      nil
      :window window
      :type :WM_PROTOCOLS
      :format 32
      :data (cons (atom-name->id atom) data)))

(deftype stack-mode () `(member :above :below :bottom-if :opposite :top-if))

(defun set-window-priority (stack-mode window &optional sibling)
  (declare (type stack-mode stack-mode))
  (declare (type xlib:window window))
  (declare (type (or null xlib:window) sibling))
  (when (member :_net_wm_window_type_desktop (netwm:net-wm-window-type window))
    (return-from set-window-priority stack-mode))
  (flet ((lookup-app-w (widget)
	   (when (decoration-p widget)
	     (get-child widget :application :window t)))
	 (layer-member (window windows)
	   (when (xlib:window-p window)
	     (member window windows :test #'xlib:window-equal)))	     
	 (first-win (windows &optional above-p)
	   (car (if above-p (last windows) windows)))
	 (restack (app sib-app priority)
           (unless (application-netwm-type-p app :_net_wm_window_type_desktop)
             (let* ((window (widget-window (or (application-master app) app)))
                    (sm (when sib-app (application-master sib-app)))
                    (sibling (when sib-app (widget-window (or sm sib-app)))))
               (unless (and sibling (xlib:window-equal window sibling))
                 (setf (xlib:window-priority window sibling) priority))))))
    (let* ((win (or (lookup-app-w (lookup-widget window)) window))
	   (sib (or (lookup-app-w (lookup-widget sibling)) sibling))
	   (widget (lookup-widget win))
	   (above-p (eq stack-mode :above))
	   (wnwm-state (netwm:net-wm-state win))
	   (snwm-state (and sib (netwm:net-wm-state sib))))
      (if (not (application-p widget))
	  (setf (xlib:window-priority window sibling) stack-mode)
	  (multiple-value-bind (below-layer std-layer above-layer)
	      (screen-windows-layers win)
	    ;; Be aware of the net-wm-state of the client leader if any.
	    (when (application-transient-for widget)
	      (with-slots ((lw window)) (application-leader widget)
		(setf wnwm-state (nconc wnwm-state (netwm:net-wm-state lw)))))
	    ;; Find the correct sibling and reset the priority if needed.
	    (cond ((member :_net_wm_state_below wnwm-state)
		   (unless (layer-member sib below-layer)
		     (setf sib (first-win (or below-layer std-layer above-layer)
                                          (and below-layer above-p)))
		     (unless below-layer (setf stack-mode :below))))
		  ((member :_net_wm_state_above wnwm-state)
		   (unless (layer-member sib above-layer)
		     (unless (member :_net_wm_state_fullscreen snwm-state)
		       (setf sib (first-win above-layer above-p))
		       (unless above-layer (setf stack-mode :above)))))
		  ((member :_net_wm_state_fullscreen wnwm-state)
		   (when (layer-member sib below-layer)
		     (setf sib (first-win (or std-layer above-layer)))
		     (setf stack-mode :below)))
		  ((not (layer-member sib std-layer))
		   (setf sib (first-win (or std-layer below-layer above-layer)
					(if std-layer above-p below-layer)))
		   (unless std-layer
		     (setf stack-mode (if below-layer :above :below)))))
	    ;; If application is not alone, update its priority.
	    (when (or below-layer std-layer above-layer)
	      (with-slots ((tr transient-for)) widget
		;; Restack all application transient window if any.
		;; If it has a client leader then don't restack it 
		;; at the end because it has already been restacked.
		(loop with seq = (application-dialogs widget)
		      for sib-app = (lookup-widget sib) then ap
		      for priority = stack-mode then :below
		      for ap in (reverse seq) do (restack ap sib-app priority)
		      finally (unless tr (restack widget sib-app priority))))
	      (update-client-list-stacking *root*))
	    stack-mode)))))

(defsetf window-priority (window &optional sibling) (priority)
  "Sets the window priority such as if done by (setf xlib:window-priority) and
   guaranty that stacking order constraints described in the extended window
   manager protocol will be respected. Then invokes update-client-list-stacking
   to reflect the new order in all the root properties that are involved in.

   - window (xlib:window): The window to be restacked.
   - sibling (or null xlib:window): An optional argument specifying that
      window is to be restacked relative to the given sibling window. 
   - priority (member :below :bottom-if :opposite :top-if): The new priority
      of the window. 

   Changes the stacking priority element of the window to the specified value.
   It is an error if the sibling argument is specified and is not actually a
   sibling of the window. Note that the priority of an existing window cannot
   be returned.

   When changing the priority of a window, if the :_net_wm_window_type_desktop
   is present in its _net_wm_window_type property then no further processing
   is performed. Otherwise, the priority is changed."
  `(set-window-priority ,priority ,window ,sibling))

(defun configure-window (win &key x y width height stack-mode sibling gravity)
  "Configures a window. The coordinate system in which the location is
   expressed is that of the root (irrespective of any reparenting that may
   have occurred). The coordinates will be updated according to the given
   gravity position hint, or to the most recently requested by the client.

   - win (xlib:window): The window to be reconfigured. 
   - sibling (or null xlib:window): An optional argument specifying that
      window is to be restacked relative to the given sibling window. 
   - stack-mode (or null (member :above :below)): If given, then window
      will be restacked. See (setf window-priority) for further details
      about the stacking order.
   - x, y (or null xlib:int16): The new coordinates of the given window.
   - width, height (or null xlib:card16): The new size of the window in
      pixels. Note that the given sizes might be reajust according to the
      window constraints."
  (let* ((widget (lookup-widget win))
	 (application (when (application-p widget) widget))
	 (master (when application (application-master application)))
	 (parent (when master (widget-window master)))
	 (top-margin 0) (left-margin 0)
	 (g (or gravity	(and master (decoration-application-gravity master))
		(let ((h (ignore-errors (wm-normal-hints win))))
		  (if h (xlib:wm-size-hints-win-gravity h) :north-west)))))
    (when master
      (setf top-margin (style-top-margin (decoration-frame-style master)))
      (setf left-margin (style-left-margin (decoration-frame-style master))))
    (multiple-value-bind (delta-x delta-y)
	(multiple-value-bind (w h) (drawable-sizes (or parent win))
	  (case g
	    (:north (values (ash w -1) 0))
	    (:north-east (values w 0))
	    (:east (values w (ash h -1)))
	    (:south-east (values w h))
	    (:south (values (ash w -1) h))
	    (:south-west (values 0 (ash h -1)))
	    (:center (values (ash w -1) (ash h -1)))
	    (:static (values left-margin top-margin))
	    (t (values 0 0))))
      ;; update coordinates.
      (xlib:with-state ((or parent win))
	(when x (setf (xlib:drawable-x (or parent win)) (- x delta-x)))
	(when y (setf (xlib:drawable-y (or parent win)) (- y delta-y))))
      ;; update sizes.
      (when (or width height)
	(with-event-mask ((or parent win))
          (when (and application (not (application-panel-p application)))
            ;; ensure width or height are compatible with wm-size-hints.
            (let* ((prop (netwm:net-wm-state win))
                   (horz-p (member :_net_wm_state_maximized_horz prop))
                   (vert-p (member :_net_wm_state_maximized_vert prop))
                   (dir (if vert-p (if horz-p 1 2) 3)))
              (multiple-value-bind (w h)
                  (geometry-sizes (find-max-geometry application dir nil))
                (unless (member :_net_wm_state_fullscreen prop)
                  (when width (setf width (if horz-p w (min width w))))
                  (when height (setf height (if vert-p h (min height h))))))))
	  (xlib:with-state (win)
	    (when width (setf (xlib:drawable-width win) width))
	    (when height (setf (xlib:drawable-height win) height)))
	  (when parent
	    (with-window-gravity (parent g)
	      (resize-from application))
	    (update-edges-geometry master))))
      ;; restack.
      (when stack-mode
	(let ((tmp (lookup-widget sibling)))
	  (when (and (application-p tmp) (application-master tmp))
	    (setf sibling (widget-window (application-master tmp)))))
	(setf (window-priority (or parent win) sibling) stack-mode)))
    ;; Acording to the ICCCM: send a synthetic configure-notify,
    ;; when we moved a application window without resizing it.
    (when (and (or x y) (not (or width height)))
      (if (decoration-p widget)
	  (setf win (get-child widget :application :window t))
	  (unless (application-p widget) (setf win nil)))
      (and win (send-configuration-notify win)))))

(defun screen-windows-layers (window &aux (i (window-desktop-num window)))
  "Returns, as multiple value, three window lists that corresponds to the
   three layers (:_net_wm_state_below none :_net_wm_state_above) of the
   virtual screen that the given `window' argument belongs to. The given
   window will be filtered."
  (loop with n = (if (eql (or i +any-desktop+) +any-desktop+) (current-desk) i)
	for w in (screen-content n :skip-taskbar nil)
        for nwm-state = (netwm:net-wm-state w)
        unless (xlib:window-equal w window)
          if (member :_net_wm_state_above nwm-state) collect w into aboves
	  else if (member :_net_wm_state_below nwm-state) collect w into belows
	  else collect w into no-stack-state
        finally (return (values belows no-stack-state aboves))))

(defun update-workarea-property (root)
  "Computes and sets the _net_workarea property for the root-window."
  (with-slots (window) root
    (loop for i from 0 below (number-of-virtual-screens window)
	  nconc (multiple-value-bind (ulx uly llx lly)
		    (multiple-value-bind (w h) (drawable-sizes window)
		      (rectangle-coordinates
                          (or (car (find-empty-rectangles
				       (make-rectangle :lrx w :lry h)
                                       (find-all-panel-rectangles i)
                                       #'rectangle-surface>=))
                              (window->rectangle window))))
		  (list ulx uly (- llx ulx) (- lly uly)))
	  into workareas
	  finally (xlib:change-property
		      window :_net_workarea workareas :CARDINAL 32))))

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
    (handler-case (run-program program arguments)
      (error () (timed-message-box *root-window* "Wrong application name")))))

(defun eclipse-desktop-pointer-positions (window &optional desk-num)
  "Returns the value of the property named _eclipse_desktop_pointer_positions
   if no desk-num is suplied. Otherwise returns as a multiple value the
   previous pointer position for the given desk-num."
  (let ((prop (xlib:get-property
	          window
		  :_eclipse_desktop_pointer_positions
		  :start (if desk-num (* 2 desk-num) 0)
		  :end  (when desk-num (+ 2 (* 2 desk-num))))))
      (if (and prop desk-num) (values-list prop) prop)))

(defsetf eclipse-desktop-pointer-positions (window desk-num) (x y)
  "Sets to x y the pointer position for the given desk-num."
  (with-gensym (prop)
    `(let ((,prop (eclipse-desktop-pointer-positions ,window)))
       (setf (elt ,prop (* ,desk-num 2)) ,x)
       (setf (elt ,prop (1+ (* ,desk-num 2))) ,y)
       (xlib:change-property 
	   ,window :_eclipse_desktop_pointer_positions ,prop :CARDINAL 32))))

(defun initialize-eclipse-desktop-pointer-positions (root)
  "Initialize the _eclipse_desktop_pointer_positions property. If the 
   property already exists then previous value is keeped and extended or
   cuted according to the number of virtual screens."
  (with-slots ((rw window)) root
    (xlib:intern-atom 
        (xlib:drawable-display rw)
	"_ECLIPSE_DESKTOP_POINTER_POSITIONS")
    (let* ((prop (eclipse-desktop-pointer-positions rw))
	   (n (* 2 (number-of-virtual-screens rw)))
	   (diff (- n (length prop))))
      (if (>= diff 0)
	  (setf prop (nconc prop (make-list diff :initial-element 0)))
	  (setf prop (subseq prop 0 n)))
      (xlib:change-property
          rw :_eclipse_desktop_pointer_positions prop :CARDINAL 32))))

(defun make-viewport-property (n)
  (declare (ignore n))
  (make-list (* 2 1) :initial-element 0))

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
  "Returns the applications objects as a list."
  (loop for val being each hash-value in *widget-table*
	when (application-p val) collect val))

(defun application-name (app)
  "Returns the name of an application widget."
  (wm-name (widget-window app)))

(defun application-find (name)
  "Returns the first application widget that is named by the specified name."
  (car (member name (application-list) :test #'equal :key #'application-name)))

(defun application-class (app)
  "Returns the class of an application widget.
   (see the WM_CLASS property in ICCCM section 4.1.2.5)."
  (multiple-value-bind (name type) (xlib:get-wm-class (widget-window app))
    (cons name type)))

(defun application-class-name (app)
  "Returns the class name of an application widget.
   (see the WM_CLASS property in ICCCM section 4.1.2.5)."
  (car (application-class app)))

(defun application-class-type (app)
  "Returns the class type of an application widget.
   (see the WM_CLASS property in ICCCM section 4.1.2.5)."
  (cdr (application-class app)))
