;;; -*- Mode: Lisp; Package: EXTENDED-WINDOW-MANAGER-HINTS -*-
;;; $Id: netwm-manager.lisp,v 1.18 2004/05/10 22:20:56 ihatchondo Exp $
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
  (:import-from :xlib #:get-property #:change-property)
  (:import-from :manager-commons #:card-32 #:card-16 #:card-8 #:int-16)
  (:export
   #:net-supported             #:net-client-list
   #:net-client-list-stacking  #:net-number-of-desktops
   #:net-current-desktop       #:net-desktop-geometry
   #:net-desktop-viewport      #:net-desktop-names
   #:net-active-window         #:net-workarea
   #:net-supporting-wm-check   #:net-virtual-roots
   #:net-frame-extents
   #:net-wm-name               #:net-wm-visible-name
   #:net-wm-icon-name          #:net-wm-visible-icon-name
   #:net-wm-desktop            #:net-wm-window-type
   #:net-wm-state              #:net-wm-strut
   #:net-wm-icon-geometry      #:net-wm-icon
   #:net-wm-pid                #:net-wm-handled-icons
   #:net-wm-allowed-actions    #:net-wm-strut-partial
   #:net-wm-user-time          

   #:intern-atoms)
  (:documentation
   "This package implements the Extended Window Manager Hints
   (from Freedesktop.org). - version 1.3 (June 19, 2003) -
   In order to use it, you should first call intern-atoms to be sure
   all atoms are in the server."))

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
	"_NET_DESKTOP_LAYOUT"         

        "_NET_RESTACK_WINDOW"         "_NET_REQUEST_FRAME_EXTENTS"
        "_NET_MOVERESIZE_WINDOW"      "_NET_CLOSE_WINDOW"
        "_NET_WM_MOVERESIZE"

	"_NET_WM_SYNC_REQUEST"        "_NET_WM_PING"    

	"_NET_WM_NAME"                "_NET_WM_VISIBLE_NAME"
	"_NET_WM_ICON_NAME"           "_NET_WM_VISIBLE_ICON_NAME"
	"_NET_WM_DESKTOP"             "_NET_WM_WINDOW_TYPE"
	"_NET_WM_STATE"               "_NET_WM_STRUT"
	"_NET_WM_ICON_GEOMETRY"       "_NET_WM_ICON"
	"_NET_WM_PID"                 "_NET_WM_HANDLED_ICONS"
	"_NET_WM_USER_TIME"           "_NET_FRAME_EXTENTS"
        ;; "_NET_WM_MOVE_ACTIONS"

	"_NET_WM_WINDOW_TYPE_DESKTOP" "_NET_WM_STATE_MODAL"
	"_NET_WM_WINDOW_TYPE_DOCK"    "_NET_WM_STATE_STICKY"
	"_NET_WM_WINDOW_TYPE_TOOLBAR" "_NET_WM_STATE_MAXIMIZED_VERT"
	"_NET_WM_WINDOW_TYPE_MENU"    "_NET_WM_STATE_MAXIMIZED_HORZ"
	"_NET_WM_WINDOW_TYPE_UTILITY" "_NET_WM_STATE_SHADED"
	"_NET_WM_WINDOW_TYPE_SPLASH"  "_NET_WM_STATE_SKIP_TASKBAR"
	"_NET_WM_WINDOW_TYPE_DIALOG"  "_NET_WM_STATE_SKIP_PAGER"
	"_NET_WM_WINDOW_TYPE_NORMAL"  "_NET_WM_STATE_HIDDEN"
	                              "_NET_WM_STATE_FULLSCREEN"
				      "_NET_WM_STATE_ABOVE"
				      "_NET_WM_STATE_BELOW"
				      "_NET_WM_STATE_DEMANDS_ATTENTION"
			
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

(define-window-list-property-accessor (net-client-list)
  :property-atom :_NET_CLIENT_LIST
  :reader-documentation
  "Returns the _net_client_list property. 
    - window: a window
    - window-list: if true the returned list is a list of window. Otherwise 
      the returned list is the list of window-id."
  :writer-documentation
  "To set this property give or a single window or a list of window.
   You can add or remove one window from the property or simply
   replace the actual value by a new list.")

;; _NET_CLIENT_LIST_STACKING

(define-window-list-property-accessor (net-client-list-stacking)
  :property-atom :_NET_CLIENT_LIST_STACKING
  :reader-documentation
  "Returns the _net_client_list_stacking property. 
    - window: a window
    - window-list: if true the returned list is a list of window. Otherwise 
      the returned list is the list of window-id."
  :writer-documentation 
  "To set this property give a single window or a list of window.
   You can add/remove one window from the property or
   simply replace the actual value by a new list.")

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
  `(set-multiple-text-property
       ,window ,names :UTF8_STRING ,mode :_NET_DESKTOP_NAMES))

;; _NET_ACTIVE_WINDOW

(defun net-active-window (window &key window-list)
  (first (get-window-property window :_NET_ACTIVE_WINDOW window-list)))

(defsetf net-active-window (window) (win)
  `(change-property ,window
		    :_NET_ACTIVE_WINDOW
		    (list (if (eql ,win :none) 0 (xlib:window-id ,win)))
		    :WINDOW 32))

;; _NET_WORKAREA

(defun net-workarea (window)
  "Returns a vector of the workareas geometry: [x y width height]* ."
  (get-property window :_NET_WORKAREA :result-type 'vector))

(defsetf net-workarea
    (window &key (mode :replace) (start 0) end) (workareas)
  "workareas must be a list of workareas geometry.
   :mode One of :replace, :append, or :prepend (default is :replace).
   :start valid index in the property
   :end valid index in the property or NIL.
   Specifies the subsequence of previous data replaced when mode is :replace."
  `(change-property ,window :_NET_WORKAREA
       (loop for e in ,workareas
	     collect (geometry-hint-x e) collect (geometry-hint-y e)
             collect (geometry-hint-width e) collect (geometry-hint-height e))
       :CARDINAL 32 :mode ,mode :start ,start :end ,end))

;; _NET_SUPPORTING_WM_CHECK

(defun net-supporting-wm-check (window &key (window-list t))
  (car (get-window-property window :_NET_SUPPORTING_WM_CHECK window-list)))

(defsetf net-supporting-wm-check (window) (win)
  `(change-property ,window
		    :_NET_SUPPORTING_WM_CHECK
		    (list (xlib:window-id ,win))
		    :WINDOW 32))

;; _NET_VIRTUAL_ROOTS

(define-window-list-property-accessor (net-virtual-roots)
  :property-atom :_NET_VIRTUAL_ROOTS
  :reader-documentation
  "Returns the _net_virtual_roots property. 
    - window: a window
    - window-list: if true the returned list is a list of window. Otherwise 
      the returned list is the list of window-id."
  :writer-documentation 
  "To set this property give or a single window or a list of window.
   You can add or remove one window from the property or simply
   replace the actual value by a new list.")

;; _NET_SHOWING_DESKTOP

(defun net-showing-desktop (window)
  (= 1 (the card-32 (first (get-property window :_NET_SHOWING_DESKTOP)))))

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

(defconstant +desktop-layout-orientation+
  '#(:horzontal :vertical))

(defconstant +desktop-layout-starting-corner+
  '#(:top-left :top-right :bottom-right :bottom-left))

(defstruct desktop-layout
  (orientation :veritcal)
  (x 0 :type card-16)
  (y 0 :type card-16)
  (starting-corner :top-left))

(defun net-desktop-layout (window)
  (let ((data (get-property window :_NET_DESKTOP_LAYOUT)))
    (make-desktop-layout 
        :orientation (svref +desktop-layout-orientation+ (first data))
	:x (second data) :y (third data)
	:starting-corner (let ((index (fourth data)))
			   (declare (type card-32 index))
			   (if (<= 0 index 4)
			       (svref +desktop-layout-starting-corner+ index)
			       :top-left)))))

(defsetf net-desktop-layout (window) (desktop-layout)
  `(with-slots (orientation x y starting-corner) ,desktop-layout
     (change-property 
         ,window :_NET_DESKTOP_LAYOUT
         (list (position orientation +desktop-layout-orientation+)
	       x y 
	       (position starting-corner +desktop-layout-starting-corner+))
         :CARDINAL 32)))
    
;;;; Other root window messages. (those are messages not properties)
;; _NET_CLOSE_WINDOW

;; _NET_WM_MOVERESIZE

;; _NET_MOVERESIZE_WINDOW

;; _NET_RESTACK_WINDOW

;; _NET_REQUEST_FRAME_EXTENTS

;;;; All the following are clients properties.
;; _NET_WM_NAME

(defun net-wm-name (window)
  (car (get-text-property window :_net_wm_name)))
  
(defsetf net-wm-name (window) (name)
  `(set-simple-text-property ,window ,name :NET-WM-USER-TIME :utf8_string))

;; _NET_WM_VISIBLE_NAME

(defun net-wm-visible-name (window)
  (car (get-text-property window :_NET_WM_VISIBLE_NAME )))
		
(defsetf net-wm-visible-name (window) (name)
  `(set-simple-text-property ,window ,name :NET-WM-USER-TIME :utf8_string))

;; _NET_WM_ICON_NAME

(defun net-wm-icon-name (window)
  (car (get-text-property window :_NET_WM_ICON_NAME)))

(defsetf net-wm-icon-name (window) (name)
  `(set-simple-text-property ,window ,name :NET-WM-USER-TIME :utf8_string))

;; _NET_WM_VISIBLE_ICON_NAME

(defun net-wm-visible-icon-name (window)
  (car (get-text-property window :_NET_WM_VISIBLE_ICON_NAME)))

(defsetf net-wm-visible-icon-name (window) (name)
  `(set-simple-text-property ,window ,name :NET-WM-USER-TIME :utf8_string))

;; _NET_WM_DESKTOP

(defun net-wm-desktop (window)
  (first (get-property window :_NET_WM_DESKTOP)))

(defsetf net-wm-desktop (window) (n)
  `(change-property ,window :_NET_WM_DESKTOP (list ,n) :CARDINAL 32))

;; _NET_WM_WINDOW_TYPE

(defun net-wm-window-type (window)
  (get-atoms-property window :_NET_WM_WINDOW_TYPE t))

(defsetf net-wm-window-type (window &key (mode :replace)) (types)
  `(set-atoms-property ,window ,types :_NET_WM_WINDOW_TYPE :mode ,mode))

;; _NET_WM_STATE

(defun net-wm-state (window)
  (get-atoms-property window :_NET_WM_STATE t))

(defsetf net-wm-state (window &key (mode :replace)) (states)
  `(set-atoms-property ,window ,states :_NET_WM_STATE :mode ,mode))

;; _NET_WM_STRUT

(defun net-wm-strut (window)
  "return the strut property as a multiple value (left right top bottom)."
  (let ((v (get-property window :_NET_WM_STRUT :result-type 'vector)))
    (declare (type (or null (simple-vector 4)) v))
    (when v (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)))))

(defsetf net-wm-strut (window) (strut)
  "set the strut property. The given strut is expected to be a list or a
   vector of length 4. The order of the element inside the strut is:
   left, right, top, bottom."
  `(change-property ,window :_NET_WM_STRUT ,strut :CARDINAL 32))

;; _NET_WM_STRUT_PARTIAL

(defun net-wm-strut-partial (window)
  "return the strut partial property as a multiple value. The order is:
    left, right, top, bottom, 
    left_start_y, left_end_y, right_start_y, right_end_y,
    top_start_x, top_end_x, bottom_start_x, bottom_end_x"
  (let ((v (get-property window :_NET_WM_STRUT_PARTIAL :result-type 'vector)))
    (declare (type (or null (simple-vector 12)) v))
    (when v
      (values (aref v 0) (aref v 1) (aref v 2) (aref v 3)
	      (aref v 4) (aref v 5) (aref v 6) (aref v 7)
	      (aref v 8) (aref v 9) (aref v 10) (aref v 11)))))

(defsetf net-wm-strut-partial (window) (strut)
  "set the strut partial property. The given strut is expected to be a 
   list or a vector of length 12. The order of the element inside the strut
   is: left, right, top, bottom, 
       left_start_y, left_end_y, right_start_y, right_end_y,
       top_start_x, top_end_x, bottom_start_x, bottom_end_x"
  `(change-property ,window :_NET_WM_STRUT_PARTIAL ,strut :CARDINAL 32))
  
;; _NET_WM_ICON_GEOMETRY

(defun net-wm-icon-geometry (window)
  (get-geometry-hint window :_NET_WM_ICON_GEOMETRY))

(defsetf net-wm-icon-geometry (window) (strut)
  `(set-geometry-hint ,window ,strut :_NET_WM_ICON_GEOMETRY))

;; _NET_WM_ICON

(defun net-wm-icon (window)
  "Returns a vector of possible icons for the client. This is an vector
  of (unsigned-byte 32) ARGB with high byte being A, low byte being B.
  The first two cardinals are width, height. Data is in rows, left to right
  and top to bottom."
  (get-property window :_NET_WM_ICON :result-type 'vector))

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
  (get-atoms-property window :_NET_WM_ALLOWED_ACTIONS t))

(defsetf net-wm-allowed-actions (window &key (mode :replace)) (actions)
  `(set-atoms-property ,window ,actions :_NET_WM_ALLOWED_ACTIONS :mode ,mode))

;; _NET_WM_USER_TIME

(defun net-wm-user-time (window)
  (first (get-property window :_NET_WM_USER_TIME)))

(defsetf net-wm-user-time (window) (time)
  `(change-property ,window :_NET_WM_USER_TIME (list ,time) :cardinal 32))

;; _NET_FRAME_EXTENTS

(defun net-frame-extents (window)
  (values-list (get-property window :_NET_FRAME_EXTENTS)))

(defsetf net-frame-extents (window) (left right top bottom)
  `(change-property ,window :_NET_FRAME_EXTENTS
       (list ,left ,right ,top ,bottom) :cardinal 32))
