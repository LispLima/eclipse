;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: eclipse.lisp,v 1.4 2003/04/07 13:35:32 hatchond Exp $
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
  (multiple-value-bind (loaded-p error)
      (ignore-errors (load pathname :verbose t))
    (or loaded-p (format *error-output* "~A~%" error))))

;;; Initializations and Main.

;; ICCCM section 2.8
(defun initialize-manager (display root-window)
  (setf +xa-wm+ (format nil "WM_S~A" (xlib:display-display display)))
  (xlib:intern-atom display +xa-wm+)
  (let ((managing-since)
	(old-wm (xlib:selection-owner display +xa-wm+))
	(manager (xlib:create-window :parent root-window 
				     :override-redirect :on
				     :width 1 :height 1
				     :x -5 :y -5)))
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
    (unless (xlib:window-equal manager (xlib:selection-owner display +xa-wm+))
      (format *error-output* 
	      "ICCCM Error : failed to aquire selection ownership~%")
      (%quit%))

    ;; Check if a non ICCCM complient window manager is not running.
    (flet ((handle-redirect-error (condition)
	   (declare (ignorable condition))
	   (format *error-output* "Redirect error - another WM is running~%")
	   (xlib:close-display display)
	   (%quit%)))
      (handler-bind ((error #'handle-redirect-error)) ; xlib:access-error
	(setf (xlib:window-event-mask root-window)
	      '(:substructure-redirect :button-press :button-release
		:focus-change :key-release :substructure-notify
		:owner-grab-button :key-press :enter-window :leave-window))
      (xlib:display-finish-output display)))

    ;; Notify all the other X clients of the new manager.
    (xlib:send-event root-window
		     :client-message
		     '(:structure-notify)
		     :window root-window
		     :type :MANAGER
		     :format 32
		     :data (list managing-since 
				 (xlib:find-atom display +xa-wm+)
				 (xlib:window-id manager)))
    manager))

(defun initialize-root (display root-window)
  (flet ((handle-redirect-error (condition)
	   (declare (ignorable condition))
	   (format *error-output* "Redirect error - another WM is running~%")
	   (xlib:close-display display)
	   (%quit%)))
    (handler-bind ((error #'handle-redirect-error)) ; xlib:access-error
      (setf (xlib:window-event-mask root-window)
	    '(:substructure-redirect :button-press :button-release :focus-change
	      :key-release :substructure-notify :owner-grab-button :key-press
	      :enter-window :leave-window))
      (xlib:display-finish-output display))))

(defun initialize (display-specification)
  (multiple-value-bind (display screen)
      (open-clx-display display-specification)
    (let* ((colormap (xlib:screen-default-colormap screen))
	   (root-window (xlib:screen-root screen))
	   (manager (initialize-manager display root-window))
	   (menu-font (xlib:open-font display "fixed")))
      (initialize-root display root-window)
      (setf *display* display)
      ;; Specific for X display
      (setf (xlib:display-error-handler display) #'default-handler
	    (xlib:display-after-function display) #'xlib:display-force-output)
      (setf *root* (make-instance 'root :window root-window :manager manager)
	    *root-window* root-window
	    (root-default-cursor *root*) (get-x-cursor *display* :xc_left_ptr))
      ;; init all gnome properties on root.
      (init-gnome-compliance display)
      (keyboard:init-keyboard display)
      (ppm:initialize colormap)
      ;; load personal configuration file, or the default one.
      (or (load-config-file (home-subdirectory cl-user::*eclipse-initfile*))
	  (load-config-file (eclipse-path "eclipserc"))
	  (format *error-output* "Unable to read a configuration file.~%")
	  (%quit%))
      ;; Eclipse globals vars.
      (setf *black* (xlib:screen-black-pixel screen)
	    *white* (xlib:screen-white-pixel screen)
	    *background1* (xlib:alloc-color colormap *background1*)
	    *background2* (xlib:alloc-color colormap *background2*)	    
	    *cursor-2* (get-x-cursor *display* :xc_fleur)
	    *gctxt* (xlib:create-gcontext :drawable root-window :font menu-font)
	    *max-char-width* (xlib:max-char-width menu-font)
	    *gcontext* (xlib:create-gcontext
			  :drawable root-window
			  :foreground *white*
			  :background *black*
			  :fill-style :solid
			  :line-style :solid
			  :line-width 1
			  :exposures :OFF
			  :font (xlib:open-font display *font-name*)))
      (setf (xlib:window-cursor root-window) (root-default-cursor *root*))
      (unless (root-decoration-theme *root*) 
	(setf (decoration-theme) "microGUI"))
      (init-edges-cursors)
      (initialize-clone))))

(defun init-gnome-compliance (display)
  (gnome:intern-gnome-atom display)
  (netwm:intern-atoms display)
  (let ((win (xlib:create-window :parent *root-window*
				 :override-redirect :on
				 :width 1 :height 1 :x -5 :y -5))
	(first-desknum (or (netwm:net-current-desktop *root-window*)
			   (gnome:win-workspace *root-window*) 0)))
    (delete-root-properties)
    (unless (< -1 first-desknum *nb-vscreen*) (setf first-desknum 0))
    (setf (vs:current-screen (root-vscreens *root*)) first-desknum
	  (gnome:win-protocols *root-window*) +gnome-protocols+
	  (gnome:win-supporting-wm-check win) win
	  (gnome:win-supporting-wm-check *root-window*) win
	  (gnome:win-workspace-count *root-window*) *nb-vscreen*
	  (gnome:win-workspace *root-window*) first-desknum

	  (netwm:net-supported *root-window*) +netwm-protocol+
	  (netwm:net-supporting-wm-check *root-window*) win
	  (netwm:net-supporting-wm-check win) win
	  (netwm:net-number-of-desktops *root-window*) *nb-vscreen*
	  (netwm:net-current-desktop *root-window*) first-desknum
	  (netwm:net-desktop-viewport *root-window*) (make-view-port-property)
	  (netwm:net-desktop-geometry *root-window*)
	  (list (screen-width) (screen-height))
	  )))

(defun eclipse (&optional display-specification)
  (initialize display-specification)
  ;(init-log-file)

  ;; Create a socket connection to communicate with the window manager.
  ;; Works only for CMUCL -x86- (unless you compile this mp package).
  #+:mp (progn
	  (setf mp::*idle-process* mp::*initial-process*)
	  (mp::start-lisp-connection-listener :port 6789 :password "clara"))

  (unwind-protect
    (catch 'end
      (handler-bind ((end-of-file #'handle-end-of-file-condition))
	(eclipse-internal-loop)
	(xlib:close-display *display*)))
    (format t "Eclipse exited. Bye.~%")
    (%QUIT%)
    ))
#|
;; should be part of the handler-bind form ; use it for "halting" the WM.
    (loop for val being each hash-value in *widget-table*
	  when (application-p val) do
	  (kill-client-window (widget-window val)))
|#
