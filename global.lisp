;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: global.lisp,v 1.38 2010-04-27 08:12:20 ihatchondo Exp $
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
(define-constant +gnome-protocols+
  '(:_win_workspace :_win_workspace_count :_win_client_list
    :_win_workspace_names) :test #'equalp)

(define-constant +netwm-protocol+
  '(:_net_client_list :_net_client_list_stacking :_net_number_of_desktops
    :_net_current_desktop :_net_active_window :_net_close_window :_net_workarea
    :_net_wm_desktop :_net_wm_window_type :_net_desktop_names
    :_net_restack_window :_net_moveresize_window :_net_wm_user_time
    :_net_wm_user_time_window :_net_request_frame_extents :_net_frame_extents 
    :_net_wm_strut_partial :_net_wm_state :_net_wm_strut
    :_net_wm_window_type_desktop :_net_wm_window_type_dock
    :_net_wm_window_type_toolbar :_net_wm_window_type_menu
    :_net_wm_window_type_utility :_net_wm_window_type_splash
    :_net_wm_window_type_dialog :_net_wm_window_type_normal
    :_net_wm_state_maximized_horz :_net_wm_state_maximized_vert
    :_net_wm_state_skip_taskbar :_net_wm_state_skip_pager :_net_wm_state_shaded
    :_net_wm_state_sticky :_net_wm_state_fullscreen :_net_wm_state_hidden
    :_net_wm_state_above :_net_wm_state_below :_net_wm_state_demands_attention)
  :test #'equalp)

(define-constant +root-event-mask+
  '(:substructure-redirect :substructure-notify :button-press :button-release
    :owner-grab-button :key-press :key-release :enter-window :leave-window
    :focus-change) :test #'equalp)

(define-constant +pointer-event-mask+
  '(:button-press :button-release :pointer-motion :pointer-motion-hint
    :enter-window :leave-window) :test #'equalp)

(defconstant +any-desktop+ #xFFFFFFFF)

(define-constant +default-font-name+ 
  "-misc-fixed-medium-r-normal--14-110-100-100-c-70-iso8859-1" :test #'string=)

(defvar *cursor-2* nil)
(defvar *display* nil)
(defvar *root* nil)
(defvar *root-window* nil)
(defvar *gcontext* nil)
(defvar *widget-table* (make-hash-table :test #'eq))
(defvar +xa-wm+ nil)

;; Default value of all the "customisable" environment variables
(defparameter *menu-1-exit-p* t)
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
 "Indicates if the action of maximizing window should making it filling whether
  the largest area around (excluding overlapped windows) or screen area.")
(defparameter *icon-hints* t
  "If you don't want eclipse to display miniature window for icons say nil")
(defparameter *icon-box* '#(-75 5 -5 -5)
  "Top left and bottom right corner coordinates of the icon box area")
(defparameter *icon-box-sep* 2)
(defparameter *icon-box-fill* :top-right
  "Icon box fill strategy, one of :{top,bottom}-{left,right}")
(defparameter *icon-box-sort-function* nil
  "Function determining icon order within the box.
   NIL corresponds to the default which is to sort on order of creation
   \(aka `icon-sort-creation-order'\).")

(defsetf font-name () (name)
  "Sets the title bar font to the font named by `name'.
   The following pattern characters can be used for wildcard matching:
    #\* Matches any sequence of zero or more characters. 
    #\? Matches any single character."
  `(if (xlib:list-font-names *display* ,name)
       (setf (xlib:gcontext-font *gcontext*) (xlib:open-font *display* ,name))
       (format *stderr* "~a is not a valid font name or pattern~%" ,name)))

(defsetf decoration-theme (&key free-old-theme-p) (name)
  "Sets the theme that must be used for window decoration. This theme will 
   be used for all existing applications as well as futur one." 
  `(set-decoration-theme ,name ,free-old-theme-p))

(defun set-decoration-theme (name free-old-theme-p)
  (with-slots (decoration-theme window) *root*
    (let ((theme (load-theme window name)))
      (when decoration-theme
        (loop with old-name = (theme-name decoration-theme)
              for val being each hash-value in *widget-table*
              when (and (application-p val) (application-master val)) do
                (with-slots (window master) val
		  (setf (decoration-frame-style master)
			(find-decoration-frame-style theme window)))
              finally (and free-old-theme-p (free-theme old-name))))
      (setf decoration-theme theme))))

(defsetf maximize-modifier () (modifier-key)
  "Sets the modifier to use to activate window maximization second behavior."
  `(if (member ,modifier-key (kb:modifiers))
       (setf *maximize-modifer* ,modifier-key)
       (error "Invalid modifier key: ~a is not a (member ~a)~%"
	      ,modifier-key (kb:modifiers))))
    
(defmacro deftypedparameter (type symbol value &optional documentation)
  "Defines a parameter with the same syntax and behavior as defparameter 
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

(defgeneric event-process (event widget)
  (:documentation "Input protocol method. This method will be invoke inside
  an infinite loop that represent the heart of the window manager. Override
  this method to handle as many widget/event combination that is needed.")
  (:method (event widget) nil))

;;;; System dependent functions.

(define-condition not-implemented (error)
  ((proc :initarg :proc))
  (:report (lambda (condition stream)
             (format stream
                     "Function ~a is not yet implemented on ~a release ~a~%"
                     (slot-value condition 'proc)
                     (lisp-implementation-type)
                     (lisp-implementation-version)))))

(defun quit (&optional code)
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

(defun run-program (program arguments)
  #+:lucid (run-program program :arguments arguments)
  #+:allegro (excl:run-shell-command
	      (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+KCL (system (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+:cmu (extensions:run-program program arguments :wait nil)
  #+:sbcl (sb-ext:run-program program arguments :wait nil :search t)
  #+:lispworks (foreign:call-system-showing-output
		(format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+clisp (ext:run-program program :arguments arguments :wait nil)
  #-(or lucid allegro KCL cmu sbcl lispworks clisp)
  (error 'not-implemented :proc (list 'run-program arguments))
  )

(defun get-username ()
  "Returns the real user name (a string) associated with the current process."
  #+sbcl (sb-unix:uid-username (sb-unix:unix-getuid))
  #+(or cmu18e cmu19) (unix:user-info-name
		       (unix:unix-getpwuid (unix:unix-getuid)))
  #+allegro-v6.2 (excl.osi:pwent-name (excl.osi:getpwent (excl.osi:getuid)))
  #-(or sbcl cmu allegro-v6.2) "nobody")

(defun getenv (var)
  "Returns shell environment variable named var."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-posix:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl)
  (error 'not-implemented :proc (list 'getenv var)))

(defun (setf getenv) (val var)
  "Sets the value of the environment variable named var to val."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list '(setf getenv) var)))

(defun getpid ()
  "Returns the unix process-id of the current lisp process."
  #+cmu (unix:unix-getpid)
  #+sbcl (sb-posix:getpid)
  #+allegro (excl::getpid)
  #+mcl (ccl::getpid)
  #+clisp (let ((getpid (or (find-symbol "PROCESS-ID" :system)
                            ;; old name prior to 2005-03-01, clisp <= 2.33.2
                            (find-symbol "PROGRAM-ID" :system)
                            #+win32 ; integrated into the above since 2005-02-24
                            (and (find-package :win32) ; optional modules/win32
                                 (find-symbol "GetCurrentProcessId" :win32)))))
            (funcall getpid))
  #-(or cmu sbcl allegro clisp) -1)

(defun user-homedir ()
  #+cmu (extensions:unix-namestring (user-homedir-pathname))
  #-cmu (namestring (user-homedir-pathname)))

(defun file-exists-p (filename)
  "Returns true if the given filename is an existing file and not a directory."
  (and #+clisp (not (probe-directory (make-pathname :directory filename)))
       #-clisp (let ((pathname (probe-file (make-pathname :directory filename))))
                 ;; When NIL is returned by probe-file, it indicates that NO
                 ;; directory exists under this filename.
                 ;; But when a valid pathname is returned, it does not 
                 ;; necessarily indicate that it is a directory.
                 ;; In this case, one needs to check if the returned pathname
                 ;; has a type or a name, what a directory pathname doesn't
                 ;; have.
                 ;; This last case concerns systems like SBCL, while the former
                 ;; case corresponds at least to CMUCL.
                 (if pathname
                     (let ((name (pathname-name pathname))
                           (type (pathname-type pathname)))
                       (or (and type (not (eql type :unspecific)))
                           (and name (not (eql type :unspecific)))))
                     t))
       (probe-file filename)))

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
  (unless asynchronous
    ;; #+:cmu (debug::backtrace most-positive-fixnum *stderr*)
    ;; #+:sbcl (sb-debug::backtrace most-positive-fixnum *stderr*)
    ;; #+:clisp (system::print-backtrace :out *stderr*)
    )
  (when resource-id
    (let* ((resource (xlib::lookup-window dpy resource-id))
	   (widget (lookup-widget resource)))
      (if widget
	  (when (and (not asynchronous) (application-p widget))
	    (event-process (make-event :destroy-notify :window resource)
			   (or (application-master widget) *root*))
	    (format *stderr* "Dead window removed from table~%"))
	  (when (member resource-id (netwm:net-client-list *root-window*))
	    (remove-window-from-client-lists resource *root*)))))
  ;; #+:cmu (debug::backtrace)
  ;; #+:sbcl (sb-debug:backtrace)
  ;; #+:clisp (system::print-backtrace)
  (finish-output *stderr*)
  (error 'already-handled-xerror))
