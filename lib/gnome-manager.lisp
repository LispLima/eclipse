;;; -*- Mode: Lisp; Package: GNOME -*-
;;; $Id: gnome-manager.lisp,v 1.3 2004/03/01 14:54:02 ihatchondo Exp $
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

(common-lisp:in-package :common-lisp-user)

(defpackage gnome
  (:use common-lisp)
  (:use manager-commons)
  (:size 50)
  (:import-from :xlib #:get-property #:change-property)
  (:import-from :manager-commons #:card-32 #:card-16 #:card-8 #:int-16)
  (:export
   #:win-client-list           #:win-workspace-count
   #:win-workspace-names       #:win-workspace
   #:win-area                  #:win-area-count
   #:win-protocols             #:win-supporting-wm-check
   #:win-layer                 #:win-state
   #:win-hints                 #:win-desktop-button-proxy
   #:win-app-state             #:win-expanded-size

   #:intern-gnome-atom)
  (:documentation
   "This package implements:
   The Gnome Window Manager Complience specification.
   When you use it I recommend to call (intern-gnome-atom display), before
   anything else, to be sure that all the atoms you will use exist in the
   server."))

(in-package :GNOME)

(declaim (optimize (speed 3)
		   (safety 1)
		   (debug 1)
		   (compilation-speed 0)))

(defconstant +gnome-atoms+
  (list "_WIN_SUPPORTING_WM_CHECK"  "_WIN_PROTOCOLS"
	"_WIN_LAYER"                "_WIN_STATE"
	"_WIN_HINTS"                "_WIN_APP_STATE"
	"_WIN_EXPANDED_SIZE"        "_WIN_ICONS"
	"_WIN_WORKSPACE"            "_WIN_WORKSPACE_COUNT"
	"_WIN_WORKSPACE_NAMES"      "_WIN_CLIENT_LIST"
	"_WIN_DESKTOP_BUTTON_PROXY" "_WIN_AREA"
	"_WIN_AREA_COUNT"
  ))

;; General initialisation
(defun intern-gnome-atom (display)
  (declare (type xlib:display display))
  (mapcar #'(lambda (atom-name) (xlib:intern-atom display atom-name))
	  +gnome-atoms+)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The gnome window manager complience specification.

;;;; All the following properties are suppose to be root properties.
;;;; Except for _WIN_WORKSPACE.

;; _WIN_CLIENT_LIST

;; Each entry is a window-id of a managed client.

(define-window-list-property-accessor (win-client-list)
  :property-atom :_WIN_CLIENT_LIST
  :data-type :CARDINAL
  :reader-documentation
  "Returns the _win_client_list property. 
    - window: a window
    - window-list: if true the returned list is a list of window. Otherwise 
      the returned list is the list of window-id."
  :writer-documentation
  "To set this property give or a single window or a list of window.
   You can add or remove one window from the property or simply
   replace the actual value by a new list.")

;; _WIN_WORKSPACE_COUNT

(defun win-workspace-count (window)
  (first (get-property window :_WIN_WORKSPACE_COUNT)))

(defsetf win-workspace-count (window) (n)
  `(change-property ,window :_WIN_WORKSPACE_COUNT (list ,n) :CARDINAL 32))

;; _WIN_WORKSPACE_NAMES

(defun win-workspace-names (window)
  (get-text-property window :_WIN_WORKSPACE_NAMES))

(defsetf win-workspace-names (window &key (mode :replace)) (names)
  `(set-multiple-text-property
       ,window ,names :STRING ,mode :_WIN_WORKSPACE_NAMES))

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
