;;; -*- Mode: Lisp; Package: EXTENDED-WINDOW-MANAGER-HINTS -*-
;;; $Id: netwm-manager.lisp,v 1.1 2002/11/07 14:23:31 hatchond Exp $
;;;
;;; This is the CLX support for the managing with gnome.
;;;
;;; Copyright (C) 2002 Iban HATCHONDO
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
;;;
;;; This package implements:
;;;  - the extended window manager hints.
;;;    http://www.freedesktop.org/standards/wm-spec/

(common-lisp:in-package :common-lisp-user)

(defpackage extended-window-manager-hints
  (:use common-lisp manager-commons)
  (:nicknames netwm)
  (:size 50)
  (:import-from :xlib get-property change-property)
  (:import-from :manager-commons 
		card-32 card-16 card-8 int-16
		get-geometry-hint set-geometry-hint
		encode-string-property get-text-property
		set-workspace-names 
		set-atoms-property get-atoms-property
		get-window-property
		make-window-list-seter)
  (:export
   net-supported             net-client-list
   net-client-list-stacking  net-number-of-desktops
   net-current-desktop       net-desktop-geometry
   net-desktop-viewport      net-desktop-names
   net-active-window         net-workarea
   net-supporting-wm-check   net-virtual-roots
   net-wm-name               net-wm-visible-name
   net-wm-icon-name          net-wm-visible-icon-name
   net-wm-desktop            net-wm-window-type
   net-wm-state              net-wm-strut
   net-wm-icon-geometry      net-wm-icon
   net-wm-pid                net-wm-handled-icons
   net-wm-allowed-actions

   intern-atoms)
  (:documentation
   "This package implements :
The Extended Window Manager Hints (from Freedesktop.org).
When you use it I recommend to call (intern-gnome-atom display),
before anything else, to be sure that all the atoms you will use
exist in the server. - version 1.2 (October 7, 2002) -
In order to use it, you should first call intern-atoms to be sure all
 atoms are in the server."))

(in-package :EXTENDED-WINDOW-MANAGER-HINTS)

(declaim (optimize (speed 3)
		   (safety 1)
		   (debug 1)
		   (compilation-speed 0)))

(defconstant +exwm-atoms+
  (list "_NET_SUPPORTED"              "_NET_CLIENT_LIST"
	"_NET_CLIENT_LIST_STACKING"   "_NET_NUMBER_OF_DESKTOPS"
	"_NET_CURRENT_DESKTOP"        "_NET_DESKTOP_GEOMETRY"
	"_NET_DESKTOP_VIEWPORT"       "_NET_DESKTOP_NAMES"
	"_NET_ACTIVE_WINDOW"          "_NET_WORKAREA"
	"_NET_SUPPORTING_WM_CHECK"    "_NET_VIRTUAL_ROOTS"
	"_NET_CLOSE_WINDOW"	      "_NET_WM_MOVERESIZE"
	"_NET_DESKTOP_LAYOUT"         "_NET_MOVERESIZE_WINDOW"

	"_NET_WM_NAME"                "_NET_WM_VISIBLE_NAME"
	"_NET_WM_ICON_NAME"           "_NET_WM_VISIBLE_ICON_NAME"
	"_NET_WM_DESKTOP"             "_NET_WM_WINDOW_TYPE"
	"_NET_WM_STATE"               "_NET_WM_STRUT"
	"_NET_WM_ICON_GEOMETRY"       "_NET_WM_ICON"
	"_NET_WM_PID"                 "_NET_WM_HANDLED_ICONS"
	"_NET_WM_PING"                ; "_NET_WM_MOVE_ACTIONS"

	"_NET_WM_WINDOW_TYPE_DESKTOP" "_NET_WM_STATE_MODAL"
	"_NET_WM_WINDOW_TYPE_DOCK"    "_NET_WM_STATE_STICKY"
	"_NET_WM_WINDOW_TYPE_TOOLBAR" "_NET_WM_STATE_MAXIMIZED_VERT"
	"_NET_WM_WINDOW_TYPE_MENU"    "_NET_WM_STATE_MAXIMIZED_HORZ"
	"_NET_WM_WINDOW_TYPE_UTILITY" "_NET_WM_STATE_SHADED"
	"_NET_WM_WINDOW_TYPE_SPLASH"  "_NET_WM_STATE_SKIP_TASKBAR"
	"_NET_WM_WINDOW_TYPE_DIALOG"  "_NET_WM_STATE_SKIP_PAGER"
	"_NET_WM_WINDOW_TYPE_NORMAL"  "_NET_WM_STATE_HIDDEN"
	                              "_NET_WM_STATE_FULLSCREEN"
			
	"_NET_WM_ALLOWED_ACTIONS"
	"_NET_WM_ACTION_MOVE"
	"_NET_WM_ACTION_RESIZE"
	"_NET_WM_ACTION_SHADE"
	"_NET_WM_ACTION_STICK"
	"_NET_WM_ACTION_MAXIMIZE_HORZ"
	"_NET_WM_ACTION_MAXIMIZE_VERT"
	"_NET_WM_ACTION_FULLSCREEN"
	"_NET_WM_ACTION_CHANGE_DESKTOP"
	"_NET_WM_ACTION_CLOSE"

	))

;; General initialisation
(defun intern-atoms (display)
  (declare (type xlib:display display))
  (mapcar #'(lambda (atom-name) (xlib:intern-atom display atom-name))
	  +exwm-atoms+)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The Extended Window Manager Hints.

;;;; All the following properties are root properties.

;; _NET_SUPPORTED
;; see _WIN_PROTOCOLS for this settings.

(defun net-supported (window &key atom-list)
  (get-atoms-property window :_NET_SUPPORTED atom-list))

(defsetf net-supported (window &key (mode :replace)) (protocols)
  `(set-atoms-property ,window ,protocols :_NET_SUPPORTED :mode ,mode))

;; _NET_CLIENT_LIST

(defun net-client-list (window &key window-list)
  (get-window-property window :_NET_CLIENT_LIST window-list))

(make-window-list-seter net-client-list :_NET_CLIENT_LIST)

(defsetf net-client-list (window &key (mode :replace)) (win)
  " To set this property give or a single window or a list of window.
    you can add or remove one window from the property or
    simply replace the actual value by a new list."
  `(set-net-client-list ,window ,win ,mode))

;; _NET_CLIENT_LIST_STACKING

(defun net-client-list-stacking (window &key window-list)
  (get-window-property window :_NET_CLIENT_LIST_STACKING window-list))

(make-window-list-seter net-client-list-stacking :_NET_CLIENT_LIST_STACKING)

(defsetf net-client-list-stacking (window &key (mode :replace)) (win)
  " To set this property give a single window or a list of window.
    you can add/remove one window from the property or
    simply replace the actual value by a new list."
  `(set-net-client-list-stacking ,window ,win ,mode))

;; _NET_NUMBER_OF_DESKTOPS

(defun net-number-of-desktops (window)
  (first (get-property window :_NET_NUMBER_OF_DESKTOPS)))

(defsetf net-number-of-desktops (window) (n)
  `(change-property ,window :_NET_NUMBER_OF_DESKTOPS (list ,n) :CARDINAL 32))

;; _NET_CURRENT_DESKTOP

(defun net-current-desktop (window)
  (first (get-property window :_NET_CURRENT_DESKTOP)))

(defsetf net-current-desktop (window) (i)
  `(change-property ,window :_NET_CURRENT_DESKTOP (list ,i) :CARDINAL 32))

;; _NET_DESKTOP_GEOMETRY

;; A desktop geometry consists in a list of two elements (width height).

(defun net-desktop-geometry (window)
  (get-property window :_NET_DESKTOP_GEOMETRY))

(defsetf net-desktop-geometry (window) (size)
  `(change-property ,window :_NET_DESKTOP_GEOMETRY ,size :CARDINAL 32))

;; _NET_DESKTOP_VIEWPORT
;; List of cardinals that define the top left corner of each desktops viewport.
;; For window managers that don't support large desktops,
;; this MUST always be set to (0,0, ...).

(defun net-desktop-viewport (window)
  (get-property window :_NET_DESKTOP_VIEWPORT))

(defsetf net-desktop-viewport (window) (point)
 `(change-property ,window :_NET_DESKTOP_VIEWPORT ,point :CARDINAL 32))

;; _NET_DESKTOP_NAMES

(defun net-desktop-names (window) 
  (get-text-property window :_NET_DESKTOP_NAMES))

(defsetf net-desktop-names (window &key (mode :replace)) (names)
  `(set-workspace-names ,window ,names :UTF8_STRING ,mode :_NET_DESKTOP_NAMES))

;; _NET_ACTIVE_WINDOW

(defun net-active-window (window &key window-list)
  (get-window-property window :_NET_ACTIVE_WINDOW window-list))

(defsetf net-active-window (window) (win)
  `(change-property ,window
		    :_NET_ACTIVE_WINDOW
		    (list (if (eql ,win :none) 0 (xlib:window-id ,win)))
		    :WINDOW 32))

;; _NET_WORKAREA

(defun net-workarea (window)
  (get-geometry-hint window :_NET_WORKAREA))

(defsetf net-workarea (window) (workarea)
  `(set-geometry-hint ,window ,workarea :_NET_WORKAREA))

;; _NET_SUPPORTING_WM_CHECK

(defun net-supporting-wm-check (window &key window-list)
  (get-window-property window :_NET_SUPPORTING_WM_CHECK window-list))

(defsetf net-supporting-wm-check (window) (win)
  `(change-property ,window
		    :_NET_SUPPORTING_WM_CHECK
		    (list (xlib:window-id ,win))
		    :WINDOW 32))

;; _NET_VIRTUAL_ROOTS

(defun net-virtual-roots (window &key window-list)
  (get-window-property window :_NET_VIRTUAL_ROOTS window-list))

(make-window-list-seter net-virtual-roots :_NET_VIRTUAL_ROOTS :CARDINAL)

(defsetf net-virtual-roots (window &key (mode :replace)) (windows)
  " To set this property give or a single window or a list of window.
    you can add or remove one window from the property or
    simply replace the actual value by a new list."
  `(set-net-virtual-roots ,window ,windows ,mode))

;; _NET_SHOWING_DESKTOP

(defun net-showing-desktop (window)
  (zerop (the card-32 (first (get-property window :_NET_SHOWING_DESKTOP)))))

(defsetf net-showing-desktop (window) (mode-p)
  `(change-property ,window 
                    :_NET_SHOWING_DESKTOP 
                    (list (if ,mode-p 1 0))
                    :CARDINAL 32))

;; _NET_DESKTOP_LAYOUT (orientation, x, y, starting_corner)

;; NOTE: In order to interoperate with Pagers implementing an earlier
;; draft of this document, Window Managers should accept a
;; _NET_DESKTOP_LAYOUT property of length 3 and use _NET_WM_TOPLEFT as
;; the starting corner in this case.

(defstruct desktop-layout
  (orientation nil)
  (x 0 :type card-16)
  (y 0 :type card-16)
  (starting-corner nil))

(defun net-desktop-layout (window)
  (let ((data (get-property window :_NET_DESKTOP_LAYOUT)))
    (make-desktop-layout 
        :orientation (if (= (the card-32 (first data)) 0) 
			 :_net_wm_orientation_horz
			 :_net_wm_orientation_vert)
	:x (second data) :y (third data)
	:starting-corner (case (the card-32 (fourth data))
			   (1 :_net_wm_topright)
			   (2 :_net_wm_bottomright)
			   (3 :_net_wm_bottomleft)
			   (t :_net_wm_topleft)))))

(defsetf net-desktop-layout (window) (layout)
  `(with-slots (orientation x y starting-corner) ,layout
     (change-property ,window 
                      :_NET_DESKTOP_LAYOUT
                      (list orientation x y starting-corner)
                      :CARDINAL 32)))    
    
;;;; Other root window messages. (those are messages not properties)
;; _NET_CLOSE_WINDOW

;; _NET_WM_MOVERESIZE

;; _NET_MOVERESIZE_WINDOW

;;;; All the following are clients properties.
;; _NET_WM_NAME

(defun set-utf8-property (window property string)
  (change-property window 
		   property 
		   (string->utf8 string :null-terminated nil) 
		   :utf8_string 8))

(defun net-wm-name (window)
  (car (get-text-property window :_net_wm_name)))
  
(defsetf net-wm-name (window) (name)
  `(set-utf8-property ,window :_NET_WM_NAME ,name))

;; _NET_WM_VISIBLE_NAME

(defun net-wm-visible-name (window)
  (car (get-text-property window :_NET_WM_VISIBLE_NAME )))
		
(defsetf net-wm-visible-name (window) (name)
  `(set-utf8-property ,window :_NET_WM_VISIBLE_NAME ,name))

;; _NET_WM_ICON_NAME

(defun net-wm-icon-name (window)
  (car (get-text-property window :_NET_WM_ICON_NAME)))

(defsetf net-wm-icon-name (window) (name)
  `(set-utf8-property ,window :_NET_WM_ICON_NAME ,name))

;; _NET_WM_VISIBLE_ICON_NAME

(defun net-wm-visible-icon-name (window)
  (car (get-text-property window :_NET_WM_VISIBLE_ICON_NAME)))

(defsetf net-wm-visible-icon-name (window) (name)
  `(set-utf8-property ,window :_NET_WM_VISIBLE_ICON_NAME ,name))

;; _NET_WM_DESKTOP

(defun net-wm-desktop (window)
  (first (get-property window :_NET_WM_DESKTOP)))

(defsetf net-wm-desktop (window) (n)
  `(change-property ,window :_NET_WM_DESKTOP (list ,n) :CARDINAL 32))

;; _NET_WM_WINDOW_TYPE

(defun net-wm-window-type (window)
  (get-property
      window
      :_NET_WM_WINDOW_TYPE
      :transform #'(lambda (id)
		     (xlib:atom-name (xlib:drawable-display window) id))))

(defsetf net-wm-window-type (window &key (mode :replace)) (types)
  `(set-atoms-property ,window ,types :_NET_WM_WINDOW_TYPE :mode ,mode))

;; _NET_WM_STATE

(defun net-wm-state (window)
  (get-property
      window
      :_NET_WM_STATE
      :transform #'(lambda (id)
		     (xlib:atom-name (xlib:drawable-display window) id))))

(defsetf net-wm-state (window &key (mode :replace)) (states)
  `(set-atoms-property ,window ,states :_NET_WM_STATE :mode ,mode))

;; _NET_WM_STRUT

(defun net-wm-strut (window)
  (get-geometry-hint window :_NET_WM_STRUT))

(defsetf net-wm-strut (window) (strut)
  `(set-geometry-hint ,window ,strut :_NET_WM_STRUT))

;; _NET_WM_ICON_GEOMETRY

(defun net-wm-icon-geometry (window)
  (get-geometry-hint window :_NET_WM_ICON_GEOMETRY))

(defsetf net-wm-strut (window) (strut)
  `(set-geometry-hint ,window ,strut :_NET_WM_ICON_GEOMETRY))

;; _NET_WM_ICON

;; FIXME:
(defun net-wm-icon (window)
  (declare (ignore window))
  (values))

;; _NET_WM_HANDLED_ICONS

(defun net-wm-handled-icons (window)
  (multiple-value-bind (data type format byte-after)
      (get-property window :_NET_WM_HANDLED_ICONS)
    (declare (type (member 8 16 32) format))
    (declare (type card-32 byte-after))
    (not (and (not (or data type)) (= 0 format byte-after)))))

(defsetf net-wm-handled-icons (window) (suported-p)
  `(if ,suported-p
       (change-property ,window :_net_wm_handled_icons '(0) :CARDINAL 8)
       (xlib:delete-property ,window :_net_wm_handled_icons)))

;; _NET_WM_PID

(defun net-wm-pid (window)
  (first (get-property window :_NET_WM_PID)))

(defsetf net-wm-pid (window) (pid)
  `(change-property ,window :_NET_WM_PID (list ,pid) :CARDINAL 32))

;; _NET_WM_ALLOWED_ACTIONS
;; This is a client window property sets by the window manager,
;; indicating user operations that the window manager supports for this window.
;; Atoms present in the list indicate allowed actions, atoms not present in the
;; list indicate actions that are not supported for this window.

(defun net-wm-allowed-actions (window)
  (get-property
      window
      :_NET_WM_ALLOWED_ACTIONS
      :transform #'(lambda (id)
		     (xlib:atom-name (xlib:drawable-display window) id))))

(defsetf net-wm-allowed-actions (window &key (mode :replace)) (actions)
  `(set-atoms-property ,window ,actions :_NET_WM_ALLOWED_ACTIONS :mode ,mode))
