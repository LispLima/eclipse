;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: global.lisp,v 1.21 2004/03/04 14:51:48 ihatchondo Exp $
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

(in-package :ECLIPSE-INTERNALS)

(defparameter *eclipse-directory* (directory-namestring *load-truename*))

(defun eclipse-path (&rest names)
  (apply #'concatenate 'string
	 (or cl-user::*eclipse-eclipsedir* *eclipse-directory*)
	 names))

;; The two following constants represent all the gnome protocols 
;; and Extended Window Manager Hints actions we want to be responsible for.
(defconstant +gnome-protocols+
  '(:_win_workspace :_win_workspace_count :_win_client_list
    :_win_workspace_names))
(defconstant +netwm-protocol+
  '(:_net_client_list :_net_client_list_stacking :_net_number_of_desktops
    :_net_current_desktop :_net_active_window :_net_close_window :_net_wm_state
    :_net_wm_desktop :_net_wm_window_type :_net_desktop_names :_net_wm_strut
    :_net_wm_strut_partial :_net_workarea :_net_moveresize_window
    :_net_wm_window_type_desktop :_net_wm_window_type_dock
    :_net_wm_window_type_toolbar :_net_wm_window_type_menu
    :_net_wm_window_type_utility :_net_wm_window_type_splash
    :_net_wm_window_type_dialog :_net_wm_window_type_normal
    :_net_wm_state_maximized_horz :_net_wm_state_maximized_vert
    :_net_wm_state_skip_taskbar :_net_wm_state_skip_pager :_net_wm_state_shaded
    :_net_wm_state_sticky :_net_wm_state_fullscreen :_net_wm_state_hidden
    :_net_wm_state_above :_net_wm_state_below))

(defconstant +root-event-mask+
  '(:substructure-redirect :substructure-notify :button-press :button-release
    :owner-grab-button :key-press :key-release :enter-window :leave-window
    :focus-change))

(defconstant +pointer-event-mask+
  '(:button-press :button-release :pointer-motion :pointer-motion-hint
    :enter-window :leave-window))

(defconstant +any-desktop+ #xFFFFFFFF)

(defconstant +default-font-name+ 
  "-misc-fixed-medium-r-normal--14-110-100-100-c-70-iso8859-1")

(defvar *cursor-2* nil)
(defvar *display* nil)
(defvar *root* nil)
(defvar *root-window* nil)
(defvar *gcontext* nil)
(defvar *widget-table* (make-hash-table))
(defvar +xa-wm+ nil)

;; Default value of all the "customisable" environment variables
(defparameter *menu-1-exit-p* t)
(defparameter *close-display-p* t)
(defparameter *menu-1-items* nil)
(defparameter *change-desktop-message-active-p* t)
(defparameter *save-and-restore-pointer-position-during-workspace-switch* nil)
(defparameter *verbose-move* t)
(defparameter *verbose-resize* t)
(defparameter *verbose-window-cycling* t)
(defparameter *warp-pointer-when-cycle* t)
(defparameter *cycle-icons-p* t "Alt-Tab shows or not iconified windows.")
(defparameter *focus-new-mapped-window* t)
(defparameter *focus-when-window-cycle* t)
(defparameter *screen-edge-resistant-p* t)
(defparameter *standard-window-edge-resistant-p* t)
(defparameter *double-click-speed* 200 "the speed of the double click")
(defparameter *move-mode* :opaque "values are: :box :opaque")
(defparameter *resize-mode* :opaque "values are: :box :opaque")
(defparameter *focus-type* :none "values are: :none :on-click")
(defparameter *maximize-modifer* :SHIFT-LEFT 
  "If modifier is down when pressing on a maximize button then it will be 
  equivalent to maximizing the window with (not *maximize-fill*).")
(defparameter *maximize-fill* nil
 "Indicate if the action of maximizing window should making it filling whether
 the largest area around (excluding overlapped windows) or screen area.")
(defparameter *icon-hints* t
  "if you don't want eclipse to display miniature window for icons say nil")
(defparameter *icon-box* '#(-75 5 -5 -5)
  "top left and bottom right corner coordinates of the icon box area")
(defparameter *icon-box-sep* 2)
(defparameter *icon-box-fill* :top-right
  "icon box fill strategy, one of :{top,bottom}-{left,right}")
(defparameter *icon-box-sort-function* nil
  "Function determining icon order within the box.
  NIL corresponds to the default which is to sort on order of creation
  \(aka `icon-sort-creation-order'\).")

(defsetf font-name () (name)
  "Set the title bar font to the font named by `name'.
  The following pattern characters can be used for wildcard matching:
   #\* Matches any sequence of zero or more characters. 
   #\? Matches any single character."
  `(if (xlib:list-font-names *display* ,name)
       (setf (xlib:gcontext-font *gcontext*) (xlib:open-font *display* ,name))
       (format *stderr* "~a is not a valid font name or pattern~%" ,name)))

(defsetf decoration-theme (&key free-old-theme-p) (name)
  `(with-slots (decoration-theme) *root*
     (let ((theme (load-theme *root-window* ,name)))
       (when decoration-theme
	 (loop with old-name = (theme-name decoration-theme)
	       for val being each hash-value in *widget-table*
	       when (and (application-p val) (application-master val)) do
	        (with-slots (window master) val
		  (setf (decoration-frame-style master)
			(find-decoration-frame-style theme window)))
	       finally (and ,free-old-theme-p (free-theme old-name))))
       (setf decoration-theme theme))))

(defsetf maximize-modifier () (modifier-key)
  `(if (member ,modifier-key (kb:modifiers))
       (setf *maximize-modifer* ,modifier-key)
       (error "Invalid modifier key: ~a is not a (member ~a)~%"
	      ,modifier-key (kb:modifiers))))
    
(defmacro deftypedparameter (type symbol value &optional documentation)
  "define a parameter with the same syntax and behavior as defparameter 
  except that its type must be given first."
  `(progn
     (defparameter ,symbol ,value ,documentation)
     (declaim (type ,type ,symbol))))

;;;; debug purpose.
(defparameter *stderr* t)

(defun init-log-file ()
  (multiple-value-bind (s m h day month year) (get-decoded-time)
    (declare (ignorable s m h))
    (setf *stderr* (open (format nil "eclipse-~A-~A-~A.log" year month day)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create))))

;;;; Input protocol.

(defgeneric event-process (event widget))

;;;; System dependent functions.

(defun %quit% (&optional code)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (unix:unix-exit (or code 0))
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit :unix-status (typecase code (number code) (null 0) (t 1)))
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'quit code))
  )

(defun %run-program% (program arguments)
  #+:lucid (run-program program :arguments arguments)
  #+:allegro (excl:run-shell-command
	      (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+KCL (system (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+:cmu (extensions:run-program program arguments :wait nil)
  #+:sbcl (sb-ext:run-program program arguments :wait nil :search t)
  #+:lispworks (foreign:call-system-showing-output
		(format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+clisp (lisp:run-program program :arguments arguments)
  )

(defun get-username ()
  "Returns the real user name (a string) associated with the current process."
  #+sbcl (sb-unix:uid-username (sb-unix:unix-getuid))
  #+cmu (unix:user-info-name (unix:unix-getpwuid (unix:unix-getuid)))
  #+allegro-v6.2 (excl.osi:pwent-name (excl.osi:getpwent (excl.osi:getuid)))
  #-(or sbcl cmu allegro-v6.2) "nobody")

;;;; Error handler.
;; The X errors handler.
;; For debug purpose: it use *stderr* as output stream. 
;; The default value for *stderr* is t (standard output) but can be set,
;; through init-log-file, to a file. (named eclipse-year-month-day.log)

(define-condition already-handled-xerror (error) ())

(defun default-handler (dpy err
			&rest keys 
			&key resource-id asynchronous 
			&allow-other-keys)
  (format *stderr* 
	  "X error ~A ~:[~;with id~]~%=> ~{~A ~}~%" 
	  err resource-id keys)
  (when (and resource-id (not asynchronous))
    (let* ((resource (xlib::lookup-window dpy resource-id))
	   (widget (lookup-widget resource)))
      (when (and widget (application-p widget))
	(event-process (make-event :destroy-notify :window resource)
		       (or (application-master widget) *root*))
	(format *stderr* "Dead window removed from table~%"))))
  (finish-output *stderr*)
  (error 'already-handled-xerror))
