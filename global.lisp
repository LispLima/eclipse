;;; -*- Mode: Lisp; Package: User -*-
;;; $Id$
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

(common-lisp:in-package :common-lisp-user)

;; This constant represant all the gnome protocol and extended, that we are
;; actually dealling with.
(defconstant +gnome-protocols+
  '(:_win_workspace :_win_workspace_count :_win_client_list
    :_win_workspace_names))
(defconstant +extended-gnome-protocol+
  '(:_net_client_list :_net_client_list_stacking :_net_number_of_desktops
    :_net_current_desktop :_net_active_window :_net_close_window
    :_net_wm_desktop :_net_wm_window_type :_net_wm_state :_net_desktop_names))

(defconstant +pointer-event-mask+
  '(:button-press :button-release :button-motion :enter-window :leave-window))

(defconstant +any-desktop+ #xFFFFFFFF)

(defvar *default-root-cursor* :none)
(defvar *cursor-2* nil)
(defvar *title-bar-cursor* nil)
(defvar *display* nil)
(defvar *root* nil)
(defvar *root-window* nil)
(defvar *gcontext* nil)
(defvar *object-table* (make-hash-table))
(defvar *keystrock-table* (make-hash-table :test #'equal))
(defparameter *button-width* 0)
(defparameter *button-height* 0)
(defparameter top-height 0)
(defparameter bottom-height 0)
(defparameter *margin* 0)
(declaim (type (unsigned-byte 16) *button-width* *button-height*))

;; Default value of all the "customisable" environement variables
(defparameter *menu-1-items* nil)
(defparameter *font-color* (xlib:make-color :red 0.00 :green 0.00 :blue 0.25))
(defparameter *nb-vscreen* 4)
(defparameter *theme* "microGUI")
(defparameter *default-cursor* :xc_left_ptr)
(defparameter *change-desktop-message-active-p* t)
(defparameter *move-type* :opaque)
;; value is :none :when-switch :on-click
(defparameter *focus-type* :none)

;; This is for personal use (an help for resize and move)
(defparameter *delta-x* nil)
(defparameter *delta-y* nil)
(defparameter *card-point* nil)

;; debug purpose
(defparameter *stderr* t)

(defun init-log-file ()
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignorable s m h))
    (setf *stderr* (open (format nil "eclipse-~A-~A-~A.log" year month day)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create))))
		
;;;; Input protocol

(defgeneric event-process (event object))

;;

(declaim (inline get-window-geometry
		 gnome-desktop-num
		 wm-name
		 workspace-names
		 stick-p
		 motif-wm-decoration
		 ))

(defun %quit% (&optional code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-code (typecase code (number code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code))
  )

(defun %run-program% (program arguments)
  #+:lucid (run-program program :arguments arguments)
  #+:allegro (excl:run-shell-command
	      (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+KCL (system (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+:cmu (extensions:run-program program arguments :wait nil)
  #+:lispworks (foreign:call-system-showing-output
		(format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+clisp (lisp:run-program program :arguments arguments)
  )

(defun get-environment-variable (&optional (string "DISPLAY"))
  ;; Add some other system.
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #-(or excl cmu clisp sbcl) (error "GET-ENVIRONMENT-VARIABLE not implemented")
  )

(defun make-view-port-property ()
  (make-list (* 2 *nb-vscreen*) :initial-element 0))

(defun home-subdirectory (directory)
  (concatenate
       'string
       #+:cmu "home:"
       #-:cmu (let ((homedir (user-homedir-pathname)))
		(or (when homedir (namestring homedir))
		    "~/"))
       directory))

(defun eclipse-path (&rest names)
  (apply #'concatenate 'string *eclipse-directory* names))

(defun ensure-theme-directory-exists (theme)
  (unless (char= (char theme (1- (length theme))) #\/)
    (setf theme (format nil "~A/" theme)))
  (unless (directory theme)
    (setf theme (if (directory (eclipse-path "themes/" theme))
		    (eclipse-path "themes/" theme)
		    (eclipse-path "themes/microGUI/"))))
  theme)

(defun get-window-geometry (window)
  (values (xlib:drawable-x window) (xlib:drawable-y window)
	  (xlib:drawable-width window) (xlib:drawable-height window)))

(defsetf window-size (window) (width height)
  `(xlib:with-state (,window)
     (and ,width (setf (xlib:drawable-width ,window) ,width))
     (and ,height (setf (xlib:drawable-height ,window) ,height))))

(defsetf window-position (window) (x y)
  `(xlib:with-state (,window)
     (and ,x (setf (xlib:drawable-x ,window) ,x))
     (and ,y (setf (xlib:drawable-y ,window) ,y))))

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
	 (host-name "unix")
	 (auth-name nil)
	 (auth-data nil)
	 (display-num nil)
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
	     (setf display-num (parse-integer string :start colon :end dot))
	     (when dot
	       (setf screen-num
		     (parse-integer string :start (1+ dot) :end dot-2))))))
    (if (equal host-name "unix")
	(multiple-value-setq (auth-name auth-data)
	  (xlib::get-best-authorization (machine-instance) display-num :tcp)))
    (let ((display (xlib:open-display host-name
				      :display display-num
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

(defmacro screen-width ()
  `(xlib:screen-width (xlib:display-default-screen *display*)))
(defmacro screen-height ()
  `(xlib:screen-height (xlib:display-default-screen *display*)))
(defmacro id->atom-name (id) `(xlib:atom-name *display* ,id))
(defmacro atom-name->id (name) `(xlib:find-atom *display* ,name))

(defmacro draw-glyphs (drawable gctxt x y seq
		       &key (start 0) end width (size :default))
  `(xlib:draw-glyphs
       ,drawable ,gctxt ,x ,y ,seq
       :start ,start :end ,end :width ,width :size ,size
       :translate #'translate))

(defmacro with-event-mask ((window &optional (ev-mask 0)) &body body)
  (let ((original-mask (gensym)))
    `(let ((,original-mask (xlib:window-event-mask ,window)))
       (unwind-protect
	 (setf (xlib:window-event-mask ,window) ,ev-mask)
	 ,@body
	 (setf (xlib:window-event-mask ,window) ,original-mask)))))

(defmacro with-root-cursor ((new-cursor) &body body)
  `(unwind-protect
     (setf (xlib:window-cursor *root-window*) ,new-cursor)
     ,@body
     (setf (xlib:window-cursor *root-window*) *default-root-cursor*)))

(defmacro make-error-handler ((type &key throw return (verbose t)) &body body)
  `(defun ,(intern (with-standard-io-syntax
		       (format nil "HANDLE-~a-CONDITION" type)))
       (condition)
     (declare (ignorable condition))
     ,(when verbose
	`(progn
	   (format *stderr* "error - ~A - : ~A~%" ',type condition)
	   (finish-output *stderr*)))
     ,(if return `nil `(throw ',(or throw type) ,@body))))

(make-error-handler (general-error) nil)
(make-error-handler (invalid-wm-property) nil)
(make-error-handler (end-of-file :throw end) nil)

(defmacro handle-wm-property (&body body)
  `(catch 'invalid-wm-property
     (handler-bind ((error #'handle-invalid-wm-property-condition))
       ,@body)))

(defun wm-state (window)
  (xlib:get-property window :WM_STATE))

(defsetf wm-state (window &key icon-id) (state)
  `(xlib:change-property ,window
			 :WM_STATE
			 (list ,state ,icon-id)
			 :WM_STATE
			 32))

(defun workspace-names (window)
  (or (gnome:net-desktop-names window) (gnome:win-workspace-names window)))

(defsetf workspace-names () (names)
  `(when ,names
     (setf (gnome:net-desktop-names *root-window*) ,names
           (gnome:win-workspace-names *root-window*) ,names)))

(defun wm-name (window)
  (or (gnome:net-wm-name window)
      (xlib:wm-name window)
      (format nil "incognito")))

(defun gnome-desktop-num (window)
  (or (gnome:net-wm-desktop window) (gnome:win-workspace window)))

(defsetf window-desktop-num (window) (n)
  `(setf (gnome:win-workspace ,window) ,n
	 (gnome:net-wm-desktop ,window) ,n
	 (application-desktop-number (gethash ,window *object-table*)) ,n))

(defun motif-wm-decoration (window)
  (let ((prop (xlib:get-property window :_MOTIF_WM_HINTS)))
    (or (and prop (logbitp 1 (first prop)) (zerop (third prop)) :OFF) :ON)))

(defun stick-p (window)
  (or (= (or (gnome-desktop-num window) -1) +any-desktop+)
      (logbitp 0 (or (gnome:win-state window :result-type t) 0))))

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

(defun draw-centered-text (window gctxt seq &key (color *font-color*))
  (multiple-value-bind (text-w text-h)
      (xlib:text-extents (xlib:gcontext-font gctxt) seq :translate #'translate)
    (multiple-value-bind (x y width height) (get-window-geometry window)
      (setq x (round (- width text-w) 2)
	    y (- height (round (- height text-h) 2)))
      (xlib:with-gcontext (gctxt :foreground color)
	(draw-glyphs window gctxt (max x 0) y seq)))))

(defun grab-root-pointer (&key (cursor *default-root-cursor*) owner-p)
  (xlib:grab-pointer
      *root-window*
      +pointer-event-mask+
      :cursor cursor
      :owner-p owner-p))

(defun query-application-tree ()
  (loop for window in (xlib:query-tree *root-window*)
	for obj = (gethash window *object-table*)
	for app = (typecase obj
		    (application (object-window obj))
		    (decoration (get-child obj :application :window t)))
	when app collect app))

(defun default-handler (dpy err &rest keys &key resource-id &allow-other-keys)
  (format *stderr* "error ~A ~:[~;with id~]~%=> ~{~A ~}~%" err resource-id keys)
  (when resource-id
    (let* ((resource (xlib::lookup-window dpy resource-id))
	   (obj (gethash resource *object-table*)))
      (when (and obj (application-p obj))
	(format *stderr* "Dead window removed from table~%")
	(event-process (make-event :destroy-notify :window resource)
		       (or (application-master obj) *root*)))))
  (finish-output *stderr*)
  (throw 'general-error nil))

(defun dismiss-move (root)
  (with-slots (move-status current-active-decoration) root
    (when (and (eql *move-type* :opaque) (eql move-status :ON))
      (setf move-status :OFF
	    current-active-decoration nil)
      (xlib:ungrab-pointer *display*))))

(defun delete-root-properties ()
  (mapc #'(lambda (prop) (xlib:delete-property *root-window* prop))
	(concatenate 'list +gnome-protocols+ +extended-gnome-protocol+)))
