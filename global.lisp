;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: global.lisp,v 1.3 2002/11/07 14:54:26 hatchond Exp $
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

(use-package :clx-extensions)

(defparameter *eclipse-directory* (directory-namestring *load-truename*))

(defun eclipse-path (&rest names)
  (apply #'concatenate 'string *eclipse-directory* names))

;; This constant represant all the gnome protocol and extended, that we are
;; actually dealling with.
(defconstant +gnome-protocols+
  '(:_win_workspace :_win_workspace_count :_win_client_list
    :_win_workspace_names))
(defconstant +netwm-protocol+
  '(:_net_client_list :_net_client_list_stacking :_net_number_of_desktops
    :_net_current_desktop :_net_active_window :_net_close_window
    :_net_wm_desktop :_net_wm_window_type :_net_wm_state :_net_desktop_names))

(defconstant +pointer-event-mask+
  '(:button-press :button-release :button-motion :enter-window :leave-window))

(defconstant +any-desktop+ #xFFFFFFFF)

(defmacro deftypedparameter (type symbol value &optional documentation)
  `(progn
     (defparameter ,symbol ,value ,documentation)
     (declaim (type ,type ,symbol))))

(defvar *cursor-2* nil)
(defvar *display* nil)
(defvar *root* nil)
(defvar *root-window* nil)
(defvar *gcontext* nil)
(defvar *widget-table* (make-hash-table))

;; Default value of all the "customisable" environement variables
(defparameter *menu-1-items* nil)
(defparameter *nb-vscreen* 4)
(defparameter *change-desktop-message-active-p* t)
(defparameter *verbose-move* t)
(defparameter *verbose-resize* t)
(defparameter *wrap-pointer-when-cycle* t)
(defparameter *focus-new-mapped-window* t)
(defparameter *focus-when-window-cycle* t)
(defparameter *move-mode* :opaque "values are: :box :opaque")
(defparameter *resize-mode* :opaque "values are: :box :opaque")
(defparameter *focus-type* :none "values are: :none :on-click")
(defparameter *font-name* 
  "-misc-fixed-medium-r-normal--14-110-100-100-c-70-iso8859-1")

(defsetf font-name () (name)
  `(setf *font-name* ,name
         (xlib:gcontext-font *gcontext*) (xlib:open-font *display* ,name)))

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

;;;; System dependant functions.

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

;;;; Error handler.
;; The X errors handler.
;; For debug purpose: it use *stderr* as output stream. 
;; The default value for *stderr* is t (standard output) but can be set,
;; through init-log-file, to a file. (named eclipse-year-month-day.log)
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
  (throw 'general-error nil))
