;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: eclipse.lisp,v 1.12 2003/12/02 19:07:06 ihatchondo Exp $
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
				     :x 0 :y 0)))
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

(defun init-gnome-compliance (display window manager)
  (gnome:intern-gnome-atom display)
  (netwm:intern-atoms display)
  (let ((first-desknum (current-vscreen window))
	(nb-vs (number-of-virtual-screens window)))
    (delete-properties window (append +gnome-protocols+ +netwm-protocol+))
    (unless (< -1 first-desknum nb-vs) (setf first-desknum 0))
    (setf (gnome:win-protocols window) +gnome-protocols+
	  (gnome:win-supporting-wm-check manager) manager
	  (gnome:win-supporting-wm-check window) manager
	  (gnome:win-workspace-count window) nb-vs
	  (gnome:win-workspace window) first-desknum

	  (netwm:net-supported window) +netwm-protocol+
	  (netwm:net-supporting-wm-check window) manager
	  (netwm:net-supporting-wm-check manager) manager
	  (netwm:net-wm-name manager) "eclipse"
	  (netwm:net-number-of-desktops window) nb-vs
	  (netwm:net-current-desktop window) first-desknum
	  (netwm:net-desktop-viewport window) (make-viewport-property nb-vs)
	  (netwm:net-desktop-geometry window)
	  (list (screen-width) (screen-height))
	  )))

(defun initialize (display-specification)
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
	    (root-default-cursor *root*) (get-x-cursor *display* :xc_left_ptr))
      ;; init all gnome properties on root.
      (init-gnome-compliance display root-window manager)
      (ppm:initialize colormap)
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
			  :foreground *white* :background *black*
			  :fill-style :solid  :line-style :solid
			  :line-width 1 :exposures :OFF))
      ;; load personal configuration file, or the default one.
      (or (load-config-file (home-subdirectory cl-user::*eclipse-initfile*))
	  (load-config-file (eclipse-path "eclipserc"))
	  (format *error-output* "Unable to read a configuration file.~%")
	  (%quit%))
      (setf (xlib:window-cursor root-window) (root-default-cursor *root*))
      (setf (slot-value *root* 'gcontext) *gcontext*)
      (unless (xlib:gcontext-font *gcontext*)
	(setf (font-name) +default-font-name+))
      (unless (root-decoration-theme *root*)
	(setf (decoration-theme) "microGUI")))))

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
	  (ignore-errors (xlib:close-display *display*))))
    (progn
      (format t "Eclipse exited. Bye.~%")
      (%QUIT%)
      )))
