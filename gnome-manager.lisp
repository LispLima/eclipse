;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: gnome-manager.lisp,v 1.2 2002/06/24 07:33:44 james Exp $
;;;
;;; This is the CLX support for the managing with gnome.
;;;
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
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
;;; This package implements :
;;;  - the gnome window manager complience specification.
;;;    http://developer.gnome.org/doc/standards/wm/
;;;  - the extended window manager hints.
;;;    http://www.freedesktop.org/standards/wm-spec/

(common-lisp:in-package :common-lisp-user)

(defpackage gnome
  (:use common-lisp)
  (:import-from :xlib get-property change-property)
  (:size 50)
  (:export
   win-client-list           win-workspace-count
   win-workspace-names       win-workspace
   win-area                  win-area-count
   win-protocols             win-supporting-wm-check
   win-layer                 win-state
   win-hints                 win-desktop-button-proxy
   win-app-state             win-expanded-size

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

   geometry-hint             make-geometry-hint
   geometry-hint-x           geometry-hint-y
   geometry-hint-width       geometry-hint-height

   encode-names              decode-names
   encode-mask               decode-mask

   utf8->strings
   string->utf8

   intern-gnome-atom)
  (:documentation
   "This package implements :
 - the Gnome Window Manager Complience specification.
 - the Extended Window Manager Hints (from Freedesktop.org).
When you use it I recommend to call (intern-gnome-atom display),
before anything else, to be sure that all the atoms you will use
exist in the server. - draft 1.2 10-17-2001"))

(in-package :GNOME)

(declaim (optimize (speed 3)
		   (safety 1)
		   (debug 1)
		   (compilation-speed 0)))

(deftype card-32 () '(unsigned-byte 32))
(deftype card-16 () '(unsigned-byte 16))
(deftype card-8 () '(unsigned-byte 8))
(deftype int-16 () '(signed-byte 16))

(defconstant +gnome-atoms+
  (list "_WIN_SUPPORTING_WM_CHECK"  "_WIN_PROTOCOLS"
	"_WIN_LAYER"                "_WIN_STATE"
	"_WIN_HINTS"                "_WIN_APP_STATE"
	"_WIN_EXPANDED_SIZE"        "_WIN_ICONS"
	"_WIN_WORKSPACE"            "_WIN_WORKSPACE_COUNT"
	"_WIN_WORKSPACE_NAMES"      "_WIN_CLIENT_LIST"
	"_WIN_DESKTOP_BUTTON_PROXY" "_WIN_AREA"
	"_WIN_AREA_COUNT"

	"_NET_SUPPORTED"              "_NET_CLIENT_LIST"
	"_NET_CLIENT_LIST_STACKING"   "_NET_NUMBER_OF_DESKTOPS"
	"_NET_CURRENT_DESKTOP"        "_NET_DESKTOP_GEOMETRY"
	"_NET_DESKTOP_VIEWPORT"       "_NET_DESKTOP_NAMES"
	"_NET_ACTIVE_WINDOW"          "_NET_WORKAREA"
	"_NET_SUPPORTING_WM_CHECK"    "_NET_VIRTUAL_ROOTS"
	"_NET_CLOSE_WINDOW"	      "_NET_WM_MOVERESIZE"

	"_NET_WM_NAME"                "_NET_WM_VISIBLE_NAME"
	"_NET_WM_ICON_NAME"           "_NET_WM_VISIBLE_ICON_NAME"
	"_NET_WM_DESKTOP"             "_NET_WM_WINDOW_TYPE"
	"_NET_WM_STATE"               "_NET_WM_STRUT"
	"_NET_WM_ICON_GEOMETRY"       "_NET_WM_ICON"
	"_NET_WM_PID"                 "_NET_WM_HANDLED_ICONS"
	"_NET_WM_PING"                "_NET_WM_MOVE_ACTIONS"

	"_NET_WM_WINDOW_TYPE_DESKTOP" "_NET_WM_STATE_MODAL"
	"_NET_WM_WINDOW_TYPE_DOCK"    "_NET_WM_STATE_STICKY"
	"_NET_WM_WINDOW_TYPE_TOOLBAR" "_NET_WM_STATE_MAXIMIZED_VERT"
	"_NET_WM_WINDOW_TYPE_MENU"    "_NET_WM_STATE_MAXIMIZED_HORZ"
	"_NET_WM_WINDOW_TYPE_UTILITY" "_NET_WM_STATE_SHADED"
	"_NET_WM_WINDOW_TYPE_SPLASH"  "_NET_WM_STATE_SKIP_TASKBAR"
	"_NET_WM_WINDOW_TYPE_DIALOG"  "_NET_WM_STATE_SKIP_PAGER"
	"_NET_WM_WINDOW_TYPE_NORMAL"  "_NET_WM_STATE_HIDDEN"
	                              "_NET_WM_STATE_FULLSCREEN"
  ))

;; General initialisation
(defun intern-gnome-atom (display)
  (declare (type xlib:display display))
  (mapcar #'(lambda (atom-name) (xlib:intern-atom display atom-name))
	  +gnome-atoms+)
  (values))

;; Some generals functions.

(defmacro aref8 (array index)
  `(the (unsigned-byte 8) (aref ,array ,index)))

(defun utf8->strings (data)
  " Transform a vector of (unsigned-byte 8) - suppose to be the
  representation of null terminated strings encoded in utf8 format -
  into a list of simple strings"
  (declare (type simple-vector data))
  (loop with aux = nil
	with length of-type card-16 = (1- (array-dimension data 0))
	for i of-type (unsigned-byte 24) from 0 to length
	for c of-type card-8 = 0 do
	(unless (zerop (aref8 data i))
	  (if (logbitp 7 (aref8 data i))
	      (if (= #x40 (logand (aref8 data i) #x7c))
		  (when (logbitp 7 (aref8 data (1+ i)))
		    (setf c (logior (ash (logand (aref8 data i) #x3) 6) 
				    (logand (aref8 data (incf i)) #x3F))))
		  (loop do (incf i)
			while (and (logbitp 7 (aref8 data i))
				   (logbitp 6 (aref8 data i)))
			finally (decf i) (setf c 35))) ; #\#
	      (setf c (aref8 data i))))
	unless (= c 0) do (push c aux) end
	if (and aux (or (= c 0) (= i length)))
	collect (map 'string #'xlib:card8->char (reverse aux))
	and do (setf aux nil)))

(defun string->utf8 (string &key (null-terminated t))
  " Return a null terminated list, or not, containing the utf8 encoding
 of the given string."
  (declare (type string string))
  (loop with aux = (map 'vector #'xlib:char->card8 string)
	for car of-type (unsigned-byte 8) across (the simple-vector aux)
	if (< car #x80) collect car into v
	else collect (logior #xC0 (ash car -6)) into v
	     and collect (logior #x80 (logand #xBF car)) into v end
	finally (return (if null-terminated (concatenate 'list v '(0)) v))))

(defstruct geometry-hint
  (x      0 :type int-16)
  (y      0 :type int-16)
  (width  0 :type card-16)
  (height 0 :type card-16))

(defun get-geometry-hint (window atom)
  (let ((prop (get-property window atom)))
    (make-geometry-hint
        :x (first prop)
	:y (second prop)
	:width (third prop)
	:height (fourth prop))))

(defun set-geometry-hint (window hint atom)
  (change-property window
		   atom
		   (list (geometry-hint-x hint)
			 (geometry-hint-y hint)
			 (geometry-hint-width hint)
			 (geometry-hint-height hint))
		   :CARDINAL 32))

(defun encode-mask (key-vector key-list key-type)
  (declare (type (simple-array keyword (*)) key-vector)
           (type (or card-16 list) key-list))
  (typecase key-list
    (card-16 key-list)
    (list (loop with mask of-type card-16 = 0
		for key in key-list
		for bit = (position key key-vector :test #'eq)
		if bit do (setf mask (logior mask (the card-16 (ash 1 bit))))
		else do (xlib::x-type-error key key-type)
		finally (return mask)))))

(defun decode-mask (key-vector mask)
  (declare (type (simple-array keyword (*)) key-vector)
           (type (or card-16 null) mask))
  (when mask
    (loop for bit of-type card-16 from 0 below (length key-vector)
	  when (logbitp bit mask) collect (aref key-vector bit))))

(defun encode-names (strings)
  (loop for string in strings
	for car = (map '(vector card-8) #'xlib:char->card8 (string string))
	collect (concatenate '(vector card-8) car #(0)) into vector
	finally (return (apply #'concatenate '(vector card-8) vector))))

(defun decode-names (chars)
  (declare (type simple-vector chars))
  (loop with name = nil
	with length of-type card-16 = (1- (array-dimension chars 0))
	for char of-type card-8 across (the simple-vector chars)
	for i of-type card-16 from 0
	unless (= 0 char) do (push char name) end
	when (and name (or (= 0 char) (= i length)))
	collect (prog1 (map 'string #'xlib:card8->char (reverse name))
		  (setf name nil))))

(defun encode-string-property (atom rest)
  (case atom
    (:string (encode-names rest))
    (:utf8_string (apply #'concatenate 'list (mapcar #'string->utf8 rest)))))

(defun get-text-property (window property-atom)
  (multiple-value-bind (data type format)
      (get-property window property-atom :result-type 'vector)
    (when (and (= format 8) data) ;; is that true ??
      (case type
	(:string (decode-names data))
	(:utf8_string (utf8->strings data))))))

(defun set-workspace-names (window names type mode atom)
  (let ((workspace-names (get-text-property window atom)))
    (unless (eq mode :replace)
      (when (eq mode :remove)
	(rotatef names workspace-names)
	(setf mode :replace))
      (setf names (nset-difference names workspace-names :test #'string=))))
  (when names
    (change-property window
		     atom
		     (encode-string-property type names)
		     type 8
		     :mode mode)))

(defun set-atoms-property (window atoms atom-property &key (mode :replace))
  (change-property
       window
       atom-property
       (loop for name in atoms
	     collect (xlib:find-atom (xlib:drawable-display window) name))
       :ATOM
       32 :mode mode))

(declaim (inline get-atoms-property))
(defun get-atoms-property (window atom-property atom-list-p)
  (get-property
      window
      atom-property
      :transform
      (when atom-list-p
	(lambda (id) (xlib:atom-name (xlib:drawable-display window) id)))))

(declaim (inline get-window-property))
(defun get-window-property (window atom window-list-p)
  (get-property
         window
	 atom
	 :transform
	 (when window-list-p
	   (lambda (id)
	     (xlib::lookup-window (xlib:drawable-display window) id)))))

(defmacro make-window-list-seter (type atom &optional (data-type :WINDOW))
  (let ((primary (with-standard-io-syntax (format nil "~A" type)))
	(setter (with-standard-io-syntax (format nil "SET-~A" type))))
    `(defun ,(intern setter) (window win mode)
       (when win
	 (change-property
	     window
	     ,atom
	     (cond ((eq mode :remove)
		    (setf mode :replace)
		    (remove win (,(intern primary) window :window-list t)))
		   ((listp win) win)
		   (t (list win)))
	     ,data-type
	     32
	     :mode mode :transform #'xlib:window-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The gnome window manager complience specification.

;;;; All the following properties are suppose to be root properties.
;;;; Except for _WIN_WORKSPACE.

;; _WIN_CLIENT_LIST

;; Each entry is a window-id of a managed client.

(defun win-client-list (window &key window-list)
  (get-window-property window :_WIN_CLIENT_LIST window-list))

(make-window-list-seter win-client-list :_WIN_CLIENT_LIST :CARDINAL)

(defsetf win-client-list (window &key (mode :replace)) (win)
  " To set this property give or a single window or a list of window.
    you can add or remove one window from the property or
    simply replace the actual value by a new list."
  `(set-win-client-list ,window ,win ,mode))

;; _WIN_WORKSPACE_COUNT

(defun win-workspace-count (window)
  (first (get-property window :_WIN_WORKSPACE_COUNT)))

(defsetf win-workspace-count (window) (n)
  `(change-property ,window :_WIN_WORKSPACE_COUNT (list ,n) :CARDINAL 32))

;; _WIN_WORKSPACE_NAMES

(defun win-workspace-names (window)
  (get-text-property window :_WIN_WORKSPACE_NAMES))

(defsetf win-workspace-names (window &key (mode :replace)) (names)
  `(set-workspace-names ,window ,names :STRING ,mode :_WIN_WORKSPACE_NAMES))

;; _WIN_AREA
;; point is a list : current-active-area-x , current-active-area-y
;; to advertise which desktop area is currently active

(defun win-area (window)
  (get-property window :_WIN_AREA))

(defsetf win-area (window) (point)
 `(change-property ,window :_WIN_AREA ,point :CARDINAL 32))

;; _WIN_AREA_COUNT
;; nb-screens is a list :
;;    (number-of-screen-horizontally , number-of-screen-vertically)
;; to advertise the size of your areas (ie N x M screens in size)

(defun win-area-count (window)
  (get-property window :_WIN_AREA_COUNT))

(defsetf win-area-count (window) (nb-screens)
  `(change-property ,window :_WIN_AREA_COUNT ,nb-screens :CARDINAL 32))

;; _WIN_PROTOCOLS

(defun win-protocols (window &key atom-list)
  (get-atoms-property window :_WIN_PROTOCOLS atom-list))

(defsetf win-protocols (window &key (mode :replace)) (protocols)
  `(set-atoms-property ,window ,protocols :_WIN_PROTOCOLS :mode ,mode))

;; _WIN_SUPPORTING_WM_CHECK

(defun win-supporting-wm-check (window &key window-list)
  (get-window-property window :_WIN_SUPPORTING_WM_CHECK window-list))

(defsetf win-supporting-wm-check (window) (win)
  `(change-property ,window
		    :_WIN_SUPPORTING_WM_CHECK
		    (list (xlib:window-id ,win))
		    :CARDINAL 32))

;; _WIN_DESKTOP_BUTTON_PROXY

(defun win-desktop-button-proxy (window &key window-list)
  (get-window-property window :_WIN_DESKTOP_BUTTON_PROXY window-list))

(defsetf win-desktop-button-proxy (window) (win)
  `(change-property ,window
		    :_WIN_DESKTOP_BUTTON_PROXY
		    (list (xlib:window-id ,win))
		    :CARDINAL 32))
	
;; _WIN_WORKSPACE
;; WARNING : It is either a root property and a client property

(defun win-workspace (window)
  (first (get-property window :_WIN_WORKSPACE)))

(defsetf win-workspace (window) (i)
  `(change-property ,window :_WIN_WORKSPACE (list ,i) :CARDINAL 32))

;;;; All the folowing properties are client property.

;; _WIN_STATE

;; Note concerning _WIN_STATE or _WIN_HINTS properties :
;; These are a simple bitmasks - if the bit is set, that state is
;; desired by the application.
;; To define your them:
;; use list of appropriate keywords, or give directly a mask value.
;; (setf (win-state window) '(:WIN_STATE_STICKY :WIN_STATE_MINIMIZED ... ))

(defconstant +win-state-vector+
  '#(:win_state_sticky          ;; everyone knows sticky
     :win_state_minimized       ;; reserved - definition is unclear
     :win_state_maximized_vert  ;; window in maximized v state
     :win_state_maximized_horiz ;; window in maximized h state
     :win_state_hidden          ;; not on taskbar but window visible
     :win_state_shaded          ;; shaded (MacOs/Afterstep style)
     :win_state_hid_workspace   ;; not on current desktop
     :win_state_hid_transient   ;; owner of transient is hidden
     :win_state_fixed_position  ;; window is fixed in position even
     :win_state_arrange_ignore  ;; ignore for auto arranging
     ))

(deftype win-state ()
  '(member :win_state_sticky :win_state_maximized_vert :win_state_fixed_position
	   :win_state_maximized_horiz :win_state_hidden :win_state_minimized
	   :win_state_hid_workspace :win_state_hid_transient :win_state_shaded
	   :win_state_arrange_ignore))

(defun win-state (window &key (result-type 'list))
  (if (eq result-type 'list)
      (decode-mask +win-state-vector+ (first (get-property window :_WIN_STATE)))
      (first (get-property window :_WIN_STATE))))

(defsetf win-state (window) (states)
  `(change-property ,window
		    :_WIN_STATE
		    (list (encode-mask +win-state-vector+ ,states 'win-state))
		    :CARDINAL 32))

;; _WIN_HINTS

(defconstant +win-hints-vector+
  '#(:win_hints_skip_focus      ;; 'alt-tab' skips this win
     :win_hints_skip_winlist    ;; do not show in window list
     :win_hints_skip_taskbar    ;; do not show on taskbar
     :win_hints_group_transient ;; Reserved - definition is unclear
     :win_hints_focus_on_click  ;; app only accepts focus if clicked
     ))

(deftype win-hints ()
  '(member :win_hints_skip_focus :win_hints_skip_winlist :win_hints_skip_taskbar
	   :win_hints_group_transient :win_hints_focus_on_click))

(defun win-hints (window &key (result-type 'list))
  (if (eq result-type 'list)
      (decode-mask +win-hints-vector+ (first (get-property window :_WIN_HINTS)))
      (first (get-property window :_WIN_HINTS))))

(defsetf win-hints (window) (hints)
  `(change-property ,window
		    :_WIN_HINTS
		    (list (encode-mask +win-hints-vector+ ,hints 'win-hints))
		    :CARDINAL 32))

;; _WIN_LAYER

;; The application can choose one of these layers to exist in. It can
;; also specify a layer other than the ones listed above if it wishes
;; to exist between 2 layers. The layer remains constant and the window
;; will always be arranged in stacking order between windows in the
;; layers above and below its own layer. If the Window Manager changes
;; the layer of an application it should change this property.

(defconstant +win-layer-vector+
  '#(:win_layer_desktop
     :win_layer_below
     :win_layer_normal
     :win_layer_ontop
     :win_layer_dock
     :win_layer_above_dock
     :win_layer_menu))

(defun win-layer (window &key (result-type 'keyword))
  (let ((layer (first (get-property window :_WIN_LAYER))))
    (declare (type card-32 layer))
    (when layer
      (if (eq result-type 'keyword)
	  (aref +win-layer-vector+ (/ layer 2))
	  layer))))

(defsetf win-layer (window) (layer)
  `(change-property ,window
		    :_WIN_LAYER
		    (list (* 2 (position ,layer +win-layer-vector+ :test #'eq)))
		    :CARDINAL 32))

;; _WIN_APP_STATE

;; Application state - also "color reactiveness" - the app can keep changing
;; this property when it changes its state and the WM or monitoring program
;; will pick this up and display something appropriate.
;; !! ONLY the client sets this property !!

(defconstant +win-app-state-vector+
  '#(:win_app_state_none
     :win_app_state_active1          :win_app_state_active2
     :win_app_state_error1           :win_app_state_error2
     :win_app_state_fatal_error1     :win_app_state_fatal_error2
     :win_app_state_idle1            :win_app_state_idle2
     :win_app_state_waiting1         :win_app_state_waiting2
     :win_app_state_working1         :win_app_state_working2
     :win_app_state_need_user_input1 :win_app_state_need_user_input2
     :win_app_state_struggling1      :win_app_state_struggling2
     :win_app_state_disk_traffic1    :win_app_state_disk_traffic2
     :win_app_state_network_traffic1 :win_app_state_network_traffic2
     :win_app_state_overloaded1  :win_app_state_overloaded2
     :win_app_state_percent000_1 :win_app_state_percent000_2
     :win_app_state_percent010_1 :win_app_state_percent010_2
     :win_app_state_percent020_1 :win_app_state_percent020_2
     :win_app_state_percent030_1 :win_app_state_percent030_2
     :win_app_state_percent040_1 :win_app_state_percent040_2
     :win_app_state_percent050_1 :win_app_state_percent050_2
     :win_app_state_percent060_1 :win_app_state_percent060_2
     :win_app_state_percent070_1 :win_app_state_percent070_2
     :win_app_state_percent080_1 :win_app_state_percent080_2
     :win_app_state_percent090_1 :win_app_state_percent090_2
     :win_app_state_percent100_1 :win_app_state_percent100_2))

(defun win-app-state (window &key (result-type 'keyword))
  (let ((state (first (get-property window :_WIN_APP_STATE))))
    (if (eq result-type 'keyword) (aref +win-app-state-vector+ state) state)))

(defsetf win-app-state (window) (state)
  `(change-property
       ,window
       :_WIN_APP_STATE
       (list (position ,state +win-app-state-vector+ :test #'eq))
       :CARDINAL 32))


;; _WIN_EXPANDED_SIZE

;; Expanded space occupied - this is the area on screen the app's window
;; will occupy when "expanded" - i.e. if you have a button on an app that
;; "hides" it by reducing its size, this is the geometry of the expanded
;; window - so the window manager should consider it for client windows
;; auto-positioning assuming the app can at any point use this area.
;; !! ONLY the client sets this property !!

(defun win-expanded-size (window)
  (get-geometry-hint window :_WIN_EXPANDED_SIZE))

(defsetf win-expanded-size (window) (expanded-size)
  `(set-geometry-hint ,window ,expanded-size :_WIN_EXPANDED_SIZE))

;; _WIN_ICONS
;; no documentation found for it

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

;;;; Other root window messages. (those are messages not properties)
;; _NET_CLOSE_WINDOW

;; _NET_WM_MOVERESIZE

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

;; _NET_WM_PID

(defun net-wm-pid (window)
  (first (get-property window :_NET_WM_PID)))

(defsetf net-wm-pid (window) (pid)
  `(change-property ,window :_NET_WM_PID (list ,pid) :CARDINAL 32))

;; _NET_WM_HANDLED_ICONS

;; FIXME: This is probably wrong

(defun net-wm-handled-icons (window)
  (multiple-value-bind (data type)
      (get-property window :_NET_WM_HANDLED_ICONS)
    (declare (ignore data))
    type))

(defsetf net-wm-handled-icons (window) (data)
  (declare (ignore data))
  `(change-property ,window :_NET_WM_HANDLED_ICONS (list 0) :CARDINAL 8))
