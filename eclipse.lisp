;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: eclipse.lisp,v 1.31 2010-04-23 14:36:49 ihatchondo Exp $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA

(in-package :ECLIPSE-INTERNALS)

(defun load-config-file (pathname)
  (load pathname))

;;; Initializations and Main.

;; ICE & SM.
(defun sm-init (sm-conn dpy)
  "Sets the xsmp properties that are required by the protocols."
  (declare (type (or null string) dpy))
  (let ((id (format nil "--sm-client-id=~a" (sm-lib:sm-client-id sm-conn)))
	(display (when dpy (format nil "--display=~a" dpy))))
    (ice-lib:post-request :set-properties sm-conn
      :properties
      (list (sm-lib:make-property
	     :name sm-lib:+program+
	     :type sm-lib:+ARRAY8+
	     :values (sm-lib:strings->array8s "eclipse"))
	    (sm-lib:make-property
	     :name sm-lib:+user-id+
	     :type sm-lib:+array8+
	     :values (sm-lib:strings->array8s (get-username)))
            (sm-lib:make-property
	     :name sm-lib:+restart-style-hint+
             :type sm-lib:+card8+
             ;; RestartImmediately
             :values (list (sm-lib:make-array8 1 :initial-element 2)))
            (sm-lib:make-property
	     :name sm-lib:+process-id+
             :type sm-lib:+array8+
             :values (sm-lib:strings->array8s (format nil "~a" (getpid))))
            (sm-lib:make-property
	     :name sm-lib:+current-directory+
             :type sm-lib:+array8+
             :values (sm-lib:strings->array8s (user-homedir)))
            (sm-lib:make-property
	     :name sm-lib:+clone-command+
	     :type sm-lib:+list-of-array8+
	     :values (if display
                         (sm-lib:strings->array8s "eclipse" display)
                         (sm-lib:strings->array8s "eclipse")))
	    (sm-lib:make-property
	     :name sm-lib:+restart-command+
	     :type sm-lib:+list-of-array8+
	     :values (if display
                         (sm-lib:strings->array8s "eclipse" display id)
                         (sm-lib:strings->array8s "eclipse" id)))
            ;; Only for Gnome Session Manager
            (sm-lib:make-property
             :name "_GSM_Priority"
             :type  sm-lib:+card8+
             :values (list (sm-lib:make-array8 1 :initial-element 20)))))))

(defun connect-to-session-manager (dpy-name &optional previous-id)
  "Try to connect us to the session manager. If connected set xsmp
   properties and returns the sm-connection instance."  
  (unless previous-id
    (setf previous-id (getenv "DESKTOP_AUTOSTART_ID"))
    ;; unset $DESKTOP_AUTOSTART_ID in order to avoid
    ;; child processes to use the same client id.
    (setf (getenv "DESKTOP_AUTOSTART_ID") ""))
  (handler-case
      (let ((sm-conn (sm-lib:open-sm-connection :previous-id previous-id)))
        (sm-init sm-conn dpy-name)
	sm-conn)
    (error (condition) (format *error-output* "~&~A~&" condition))))

(defun handle-session-manager-request (sm-conn root-widget)
  "Handles xsmp requests. If a DIE request is received then invoke
   close-sm-connection and propagate the exit-eclipse condition."
  (handler-case
      (ice-lib:request-case (sm-conn :timeout 0)
	(sm-lib:save-yourself ()
          (ice-lib:post-request :save-yourself-done sm-conn :success-p t)
	  t)
	(sm-lib:die () (close-sm-connection root-widget :exit-p t) nil)
	(t t))
    (exit-eclipse (condition) (signal condition))
    (error (condition)
      #+:cmu (debug::backtrace)
      #+:sbcl (sb-debug::backtrace)
      #+:clisp (system::print-backtrace)
      (format *error-output* "~&~A~&" condition))))

(defun initialize-manager (display root-window)
  ;; ICCCM section 2.8
  (setf +xa-wm+ (format nil "WM_S~A" (xlib:display-display display)))
  (xlib:intern-atom display +xa-wm+)
  (let ((managing-since)
	(old-wm (xlib:selection-owner display +xa-wm+))
	(manager (xlib:create-window :parent root-window 
				     :override-redirect :on
				     :width 1 :height 1
				     :x 0 :y 0)))
    (declare (type xlib:window manager))
    (declare (type (or null xlib:window) old-wm))
    (when old-wm
      (setf (xlib:window-event-mask old-wm) '(:structure-notify)))

    ;; Get a valid timestamp.
    (with-event-mask (manager '(:property-change))
      (xlib:change-property manager :wm_name '(0) :string 8)
      (xlib:event-case (display :force-output-p t :discard-p t)
        (:property-notify (window time)
	  (when (xlib:window-equal window manager) (setf managing-since time)))
	(t nil)))

    ;; Ask for selection ownership, and wait for the old owner destruction.
    (setf (xlib:selection-owner display +xa-wm+ managing-since) manager)
    (when old-wm
      (xlib:event-case (display :force-output-p t :discard-p t :timeout 10)
        (:destroy-notify (window) (xlib:window-equal window old-wm))
	(t nil)))

    ;; Are we the selection owner after all ?
    (let ((owner (xlib:selection-owner display +xa-wm+)))
      (declare (type (or null xlib:window) owner))
      (unless (and owner (xlib:window-equal manager owner))
        (error "ICCCM Error: failed to aquire selection ownership~%")))

    ;; Check if a non ICCCM complient window manager is not running.
    (handler-case
	(progn
	  (setf (xlib:window-event-mask root-window) +root-event-mask+)
	  (xlib:display-finish-output display))
      (error () (error "Redirect error: another WM is running~%")))

    ;; Notify all the other X clients of the new manager.
    (xlib:send-event root-window :client-message '(:structure-notify)
      :window root-window
      :type :MANAGER
      :format 32
      :data (list managing-since 
		  (xlib:find-atom display +xa-wm+)
		  (xlib:window-id manager)))
    (setf (xlib:window-event-mask manager) '(:property-change))
    (make-instance 'standard-property-holder :window manager)))

(defun init-gnome-compliance (display window manager)
  (gnome:intern-gnome-atom display)
  (netwm:intern-atoms display)
  (let ((first-desknum (current-vscreen window))
	(nb-vs (number-of-virtual-screens window))
	(srcw (screen-width)) (srch (screen-height)))
    (xlib:with-server-grabbed (display)
      (delete-properties window +netwm-protocol+)
      (unless (< -1 first-desknum nb-vs) (setf first-desknum 0))
      (setf (gnome:win-protocols window) +gnome-protocols+
            (gnome:win-supporting-wm-check manager) manager
            (gnome:win-supporting-wm-check window) manager
            (gnome:win-workspace-count window) nb-vs
            (gnome:win-workspace window) first-desknum)

      (setf (netwm:net-supported window) +netwm-protocol+
            (netwm:net-supporting-wm-check window) manager
            (netwm:net-supporting-wm-check manager) manager
            (netwm:net-wm-name manager) "eclipse"
            (netwm:net-number-of-desktops window) nb-vs
            (netwm:net-current-desktop window) first-desknum
            (netwm:net-desktop-viewport window) (make-viewport-property nb-vs)
            (netwm:net-desktop-geometry window) (list srcw srch)
            (netwm:net-workarea window) (make-list nb-vs
                                            :initial-element
                                            (manager-commons:make-geometry-hint
                                             :x 0 :y 0 :width srcw :height srch))

	  ))))

(defun initialize (display-specification sm-client-id)
  (multiple-value-bind (display screen)
      (open-clx-display display-specification)
    (let* ((colormap (xlib:screen-default-colormap screen))
	   (root-window (xlib:screen-root screen))
	   (manager (initialize-manager display root-window))
	   (menu-font (xlib:open-font display "fixed")))
      (setf *display* display)
      ;; Specific for X display
      (setf (xlib:display-error-handler display) #'default-handler
	    (xlib:display-after-function display) #'xlib:display-force-output)
      (setf *root* (make-instance 'root :window root-window :manager manager)
	    *root-window* root-window
	    (root-default-cursor *root*) (get-x-cursor display :xc_left_ptr)
	    (root-sm-conn *root*) (connect-to-session-manager
				      display-specification sm-client-id))
      ;; init all gnome properties on root.
      (init-gnome-compliance display root-window (widget-window manager))
      (ppm:initialize colormap)
      ;; Eclipse globals vars.
      (setf *black* (xlib:screen-black-pixel screen)
	    *white* (xlib:screen-white-pixel screen)
	    *background1* (xlib:alloc-color colormap *menu-color*)
	    *background2* (xlib:alloc-color colormap *menu-hilighted-color*)
	    *cursor-2* (get-x-cursor display :xc_fleur)
	    *gctxt* (xlib:create-gcontext :drawable root-window :font menu-font)
	    *max-char-width* (xlib:max-char-width menu-font)
	    *gcontext* (xlib:create-gcontext
			  :drawable root-window
			  :foreground *white* :background *black*
			  :fill-style :solid  :line-style :solid
			  :line-width 1 :exposures :OFF))
      ;; load personal configuration file, or the default one.
      (labels ((load-if (f) (and (file-exists-p f) (load-config-file f))))
	(or (load-if (home-subdirectory cl-user::*eclipse-initfile*))
	    (load-if (eclipse-path "eclipserc"))
	    (error "Unable to read a configuration file.~%")))
      (setf (xlib:window-cursor root-window) (root-default-cursor *root*))
      (setf (slot-value *root* 'gcontext) *gcontext*)
      (unless (xlib:gcontext-font *gcontext*)
	(setf (font-name) +default-font-name+))
      (unless (root-decoration-theme *root*)
	(setf (decoration-theme) "microGUI")))))

(defun eclipse (&key display sm-client-id die-on-init-error activate-log)
  "Starts the Eclipse window manager.
    - :display (or null string): if given it is expected it respects the 
      standard X DISPLAY specification: hostname:displaynumber.screennumber
      (for more about the display string format see the X manual page).
      If NIL then the DISPLAY environment variable will be used.
    - :sm-client-id (or null string): if Eclipse is restarted from a previous
      session, should contain the previous client-id of that previous session.
      If :sm-client-id is specified, but is determined to be invalid by the
      session manager, we will re-register the client with a sm-client-id set
      to NIL. If the client is first joining the session :sm-client-id can be
      NIL (default) or the empty string.
    - :die-on-init-error (boolean): indicates if Eclipse should prevent
      from debugger or not during initialisation phase.
    - :activate-log (boolean): indicates if errors should be logged.
      If T then errors will be logged in a file named: eclipse-yyyy-mm-dd.log
   If neither the DISPLAY environment variable is defined nor the :display
   argument is defined then Eclipse will not be able to starts."
  (declare (type (or null string) display sm-client-id))
  (declare (type boolean activate-log die-on-init-error))
  (when *display* 
    (xlib:close-display *display*)
    (setf *display* nil))
  (if die-on-init-error
      (handler-case (initialize display sm-client-id)
	(error (condition)
	  (format *error-output* "~A~%" condition)
	  (quit)))
      (initialize display sm-client-id))
  (when activate-log
    (init-log-file))

  ;; This is for releasing any previous pointer grab.
  (ignore-errors
    (grab-root-pointer)
    (xlib:display-finish-output *display*))
  (xlib:ungrab-pointer *display*)

  ;; Create a socket connection to communicate with the window manager.
  ;; Works only for CMUCL -x86- (unless you compile this mp package).
  #+:mp (progn
	  (setf mp::*idle-process* mp::*initial-process*)
	  (mp::start-lisp-connection-listener :port 6789 :password "clara"))

  (unwind-protect
       (handler-case (eclipse-internal-loop)
         (end-of-file (c) (handle-end-of-file-condition c)))
    (ignore-errors (xlib:close-display *display*))
    (format t "Eclipse exited. Bye.~%")
    (quit)))
