;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: eclipse.lisp,v 1.2 2002/11/12 17:01:58 hatchond Exp $
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
    (let ((colormap (xlib:screen-default-colormap screen))
	  (root-window (xlib:screen-root screen))
	  (menu-font (xlib:open-font display "fixed")))
      (initialize-root display root-window)
      (setf *display* display)
      ;; Specific for X display
      (setf (xlib:display-error-handler display) #'default-handler
	    (xlib:display-after-function display) #'xlib:display-force-output)
      (setf *root* (make-instance 'root :window root-window)
	    *root-window* root-window
	    (root-default-cursor *root*) (get-x-cursor *display* :xc_left_ptr))
      ;; init all gnome properties on root.
      (init-gnome-compliance display)
      (keyboard:init-keyboard display)
      (ppm:initialize colormap)
      ;; load personal configuration file, or the default one.
      (or (load-config-file (home-subdirectory ".eclipse"))
	  (load-config-file (eclipse-path ".eclipse"))
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
