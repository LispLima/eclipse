;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: package.lisp,v 1.3 2003/06/11 18:29:23 hatchond Exp $
;;;
;;; This file is part of Eclipse.
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(common-lisp:in-package :common-lisp-user)

(defpackage "ECLIPSE-INTERNALS"
  (:nicknames eclipse ewmi)
  (:use clx-extensions common-lisp)
  (:size 500)
  (:documentation "")
  (:export 
   "APPLICATION"			  ;class
   "BASE-WIDGET"			  ;class
   "BOTTOM"				  ;class
   "BOTTOM-LEFT"			  ;class
   "BOTTOM-RIGHT"			  ;class
   "BOX-BUTTON"				  ;class
   "BUTTON"				  ;class
   "CLOSE-BUTTON"			  ;class
   "DECORATION"				  ;class
   "DEFAULT-STYLE"			  ;class
   "ECLIPSE-SCREENS"			  ;class
   "EDGE"				  ;class
   "FRAME-STYLE"			  ;class
   "GEOMETRY"				  ;class
   "ICON"				  ;class
   "ICONIFY-BUTTON"			  ;class
   "KEYSTROKE"				  ;class
   "LEFT"				  ;class
   "MAXIMIZE-BUTTON"			  ;class
   "MENU-BUTTON"			  ;class
   "MENU-ITEM"				  ;class
   "MENU-LEAF-ITEM"			  ;class
   "MENU-WIDGET"			  ;class
   "MOUSE-STROKE"			  ;class
   "POP-UP-MENU"			  ;class
   "PUSH-BUTTON"			  ;class
   "RIGHT"				  ;class
   "STROKE"				  ;class
   "SUB-MENU"				  ;class
   "THEME"				  ;class
   "TITLE-BAR"				  ;class
   "TOP"				  ;class
   "TOP-LEFT"				  ;class
   "TOP-RIGHT"				  ;class
   "TRANSIENT-STYLE"			  ;class

   "APPLICATION-CLASS"			  ;function
   "APPLICATION-CLASS-NAME"		  ;function
   "APPLICATION-CLASS-TYPE"		  ;function
   "APPLICATION-FIND"			  ;function
   "APPLICATION-LIST"			  ;function
   "APPLICATION-NAME"			  ;function
   "APPLICATION-P"			  ;function
   "BASE-WIDGET-P"			  ;function
   "BUTTON-P"				  ;function
   "CHECK-SIZE"				  ;function
   "COPY-GEOMETRY"			  ;function
   "CREATE-APPLICATION"			  ;function
   "CREATE-BUTTON"			  ;function
   "CREATE-ICON"			  ;function
   "CREATE-MESSAGE-BOX"			  ;function
   "CURRENT-VSCREEN"                      ;function
   "DECORATION-P"			  ;function
   "DECORATION-THEME"			  ;setf function
   "DECORE-APPLICATION"			  ;function
   "DEFAULT-HANDLER"			  ;function
   "DEFAULT-STYLE-P"			  ;function
   "DEFINE-KEY-COMBO"			  ;function
   "DEFINE-MENU-3"			  ;function
   "DEFINE-MOUSE-COMBO"			  ;function
   "DELETE-ROOT-PROPERTIES"		  ;function
   "DISMISS-MOVE-RESIZE"		  ;function
   "DISPLAY-COORDINATES"		  ;function
   "DISPLAY-GEOMETRY"			  ;function
   "DISPLAY-INFOS"			  ;function
   "DRAW-FOCUSED-DECORATION"		  ;function
   "DRAW-UNFOCUSED-DECORATION"		  ;function
   "DRAW-WINDOW-GRID"			  ;function
   "ECLIPSE"				  ;function
   "ECLIPSE-PATH"			  ;function
   "EDGE-POSITION"			  ;function
   "ENSURE-THEME-DIRECTORY-EXISTS"	  ;function
   "FINALIZE-MOVE"			  ;function
   "FIND-CORNER"			  ;function
   "FIND-DECORATION-FRAME-STYLE"	  ;function
   "FIND-INPUT-MODEL"			  ;function
   "FINISH-RESIZE"			  ;function
   "FONT-NAME"				  ;setf function
   "FREE-THEME"				  ;function
   "FULLSCREENABLE-P"			  ;function
   "FULLSCREEN-MODE"			  ;setf function
   "GEOMETRY-COORDINATES"		  ;function
   "GEOMETRY-H"				  ;function
   "GEOMETRY-P"				  ;function
   "GEOMETRY-SIZES"			  ;function
   "GEOMETRY-W"				  ;function
   "GEOMETRY-X"				  ;function
   "GEOMETRY-Y"				  ;function
   "GET-MAX-ITEM-WIDTH"			  ;function
   "GET-SCREEN-CONTENT"                   ;function
   "GET-VISIBLE-WINDOWS"		  ;function
   "GIVE-FOCUS-TO-NEXT-WIDGET-IN-DESKTOP" ;function
   "GNOME-DESKTOP-NUM"			  ;function
   "HIGHLIGHT"				  ;function
   "HOME-SUBDIRECTORY"			  ;function
   "ICON-BOX-UPDATE"			  ;function
   "ICON-P"				  ;function
   "ICON-SORT-CREATION-ORDER"		  ;function
   "ICON-SORT-NAME"			  ;function
   "ICON-SORT-TYPE"			  ;function
   "INIT-EDGES-CURSORS"			  ;function
   "INIT-GNOME-COMPLIANCE"		  ;function
   "INITIALIZE-CLONE"			  ;function
   "INITIALIZE-FRAME"			  ;function
   "INITIALIZE"				  ;function
   "INITIALIZE-GEOMETRY-INFO-BOX"	  ;function
   "INITIALIZE-MANAGER"			  ;function
   "INIT-LOG-FILE"			  ;function
   "INPUT-FOCUS"                          ;function
   "KEYCODE-REGISTERED-P"		  ;function
   "KEYSTROKE-P"			  ;function
   "KILL-CLIENT-WINDOW"			  ;function
   "LOAD-CONFIG-FILE"			  ;function
   "LOAD-PNM->PIXMAP"			  ;function
   "LOAD-THEME"				  ;function
   "LOOKUP-KEYSTROKE"			  ;function
   "LOOKUP-MOUSE-STROKE"		  ;function
   "LOOKUP-THEME"                         ;function
   "LOOKUP-WIDGET"			  ;function
   "MAKE-BACKGROUND-PIXMAP"		  ;function
   "MAKE-BUTTONS-BAR"			  ;function
   "MAKE-CORNER"			  ;function
   "MAKE-DECORATION"			  ;function
   "MAKE-DESK-ENTRIES"			  ;function
   "MAKE-EDGES"				  ;function
   "MAKE-GEOMETRY"			  ;function
   "MAKE-KEYSTROKE"			  ;function
   "MAKE-MENU-BUTTON"			  ;function
   "MAKE-MENU-BUTTON-MENU"		  ;function
   "MAKE-MENU-LEAF-ITEM"		  ;function
   "MAKE-MOUSE-STROKE"			  ;function
   "MAKE-POP-UP"			  ;function
   "MAKE-RAISE-WINDOW"			  ;function
   "MAKE-RUNNING-MENU"			  ;function
   "MAKE-SUB-MENU"			  ;function
   "MAKE-SUBSTRUCTURE"			  ;function
   "MAKE-TITLE-BAR"			  ;function
   "MAKE-VIEWPORT-PROPERTY"		  ;function
   "MAXIMIZE-WINDOW"			  ;function
   "MOTIF-WM-DECORATION"		  ;function
   "MOUSE-STROKE-FOR-MOVE-AND-RESIZE"	  ;function
   "MOUSE-STROKE-P"			  ;function
   "MOVE-CURSOR-DOWN"			  ;function
   "MOVE-CURSOR-LEFT"			  ;function
   "MOVE-CURSOR-RIGHT"			  ;function
   "MOVE-CURSOR-UP"			  ;function
   "MOVE-WIDGET"			  ;function
   "NUMBER-OF-VIRTUAL-SCREENS"		  ;function, setf
   "PERFORM-CLICK"			  ;function
   "PIXMAP-HEIGHT"			  ;function
   "PIXMAP-WIDTH"			  ;function
   "PROCEDE-DECORATION"			  ;function
   "QUERY-APPLICATION-TREE"		  ;function
   "%QUIT%"				  ;function
   "RAISE-WINDOW"			  ;function
   "REALIZE-MENU-ITEMS"			  ;function
   "REALIZE-POP-UP"			  ;function
   "RECOMPUTE-WM-NORMAL-HINTS"		  ;function
   "REGISTER-ALL-KEYSTROKES"		  ;function
   "REGISTER-ALL-MOUSE-STROKES"		  ;function
   "RUN-APPLICATION"			  ;function
   "SEND-WM-PROTOCOLS-CLIENT-MESSAGE"	  ;function
   "STICK-P"				  ;function
   "TIMED-MESSAGE-BOX"			  ;function
   "TITLE-BAR-HORIZONTAL-P"		  ;function
   "TRANSIENT-STYLE-P"			  ;function
   "UNDECORE-APPLICATION"		  ;function
   "UNDRAW-GEOMETRY-INFO-BOX"		  ;function
   "UNREGISTER-ALL-KEYSTROKES"		  ;function
   "UNREGISTER-ALL-MOUSE-STROKES"	  ;function
   "UPDATE-CLONE"			  ;function
   "UPDATE-EDGES-GEOMETRY"		  ;function
   "UPDATE-LISTS"			  ;function
   "UPDATE-TITLE-BAR-SIZES"		  ;function
   "WHERE-IS-POINTER"			  ;function
   "WINDOW-DESKTOP-NUM"			  ;setf function
   "WINDOW-NOT-DECORABLE-P"               ;function
   "WINDOW-PRIORITY"                      ;setf function
   "WINDOW-TRANSIENT-P"			  ;function
   "WM-ICON-NAME"			  ;function
   "WM-NAME"				  ;function
   "WM-STATE"				  ;function
   "WORKSPACE-NAMES"			  ;function, setf

   "ADD-DESKTOP-APPLICATION"		  ;generic function
   "APPLICATION-ICON"			  ;generic function
   "APPLICATION-ICONIC-P"		  ;generic function
   "APPLICATION-INPUT-MODEL"		  ;generic function
   "APPLICATION-MASTER"			  ;generic function
   "APPLICATION-WANTS-FOCUS-P"		  ;generic function
   "ARM-BRANCH"				  ;generic function
   "ARM"				  ;generic function
   "BOTTOM-LEFT-H"			  ;generic function
   "BOTTOM-LEFT-W"			  ;generic function
   "BOTTOM-RIGHT-H"			  ;generic function
   "BOTTOM-RIGHT-W"			  ;generic function
   "BUTTON-ACTIVE-P"			  ;generic function
   "BUTTON-ARMED"			  ;generic function
   "BUTTON-ITEM-TO-DRAW"		  ;generic function
   "BUTTON-MASTER"			  ;generic function
   "CHANGE-VSCREEN"			  ;generic function
   "CIRCULATE-WINDOW"			  ;generic function
   "CLOSE-WIDGET"			  ;generic function
   "DECORATION-ACTIVE-P"		  ;generic function
   "DECORATION-APPLICATION-GRAVITY"	  ;generic function
   "DECORATION-BASE-HEIGHT"		  ;generic function
   "DECORATION-BASE-WIDTH"		  ;generic function
   "DECORATION-CHILDREN"		  ;generic function
   "DECORATION-FRAME-STYLE"		  ;generic function
   "DECORATION-INC-SIZES"		  ;generic function
   "DECORATION-MAX-HEIGHT"		  ;generic function
   "DECORATION-MAX-WIDTH"		  ;generic function
   "DECORATION-MIN-HEIGHT"		  ;generic function
   "DECORATION-MIN-WIDTH"		  ;generic function
   "DECORATION-PRECEDENT-TIME"		  ;generic function
   "DECORATION-WM-HINTS"		  ;generic function
   "DECORATION-WM-SIZE-HINTS"		  ;generic function
   "DEFAULT-MODIFIERS-P"		  ;generic function
   "DESTROY-SUBSTRUCTURE"		  ;generic function
   "DISARM"				  ;generic function
   "DISPATCH-REPAINT"			  ;generic function
   "DRAW-ON-FOCUS-IN"			  ;generic function
   "DRAW-ON-FOCUS-OUT"			  ;generic function
   "EVENT-PROCESS"			  ;generic function
   "FOCUSED-P"				  ;generic function
   "FOCUS-WIDGET"			  ;generic function
   "FRAME-BUTTON-SIZES"			  ;generic function
   "FRAME-ITEM-EXIST-P"			  ;generic function
   "FRAME-ITEM-HEIGHT"			  ;generic function
   "FRAME-ITEM-PIXMAPS"			  ;generic function
   "FRAME-ITEM-SIZES"			  ;generic function
   "FRAME-ITEM-WIDTH"			  ;generic function
   "FREE-FRAME-STYLE"			  ;generic function
   "GET-CHILD"				  ;generic function
   "GET-PIXMAP"				  ;generic function
   "GET-ROOT-DESKTOP"			  ;generic function
   "ICON-APPLICATION"			  ;generic function
   "ICON-CREATION-TIME"			  ;generic function
   "ICON-DESICONIFY-P"			  ;generic function
   "ICONIFY"				  ;generic function
   "INITIALIZE-MOVE"			  ;generic function
   "KEYSTROKE-KEYSYMS"			  ;generic function
   "MENU-3-PROCESS"			  ;generic function
   "MENU-LEAF-CALLBACK"			  ;generic function
   "MENU-ROOT-APPLICATION-WINDOW"	  ;generic function
   "MENU-ROOT"				  ;generic function
   "MOUSE-STROKE-BUTTON"		  ;generic function
   "PUT-ON-TOP"                           ;generic function
   "PUT-ON-BOTTOM"                        ;generic function
   "REPAINT"				  ;generic function
   "REMOVE-DESKTOP-APPLICATION"		  ;generic function
   "REMOVE-WIDGET"			  ;generic function
   "RESIZE-FROM"			  ;generic function
   "RESIZE"				  ;generic function
   "ROOT-DECORATION-THEME"		  ;generic function
   "ROOT-DEFAULT-CURSOR"		  ;generic function
   "ROOT-DESKTOP"			  ;generic function
   "ROOT-MANAGER"			  ;generic function
   "ROOT-MOVE-STATUS"			  ;generic function
   "ROOT-RESIZE-STATUS"			  ;generic function
   "SET-FOCUS"				  ;generic function
   "SHADE"                                ;generic function
   "SHADED-P"                             ;generic function
   "STROKE-ACTION"			  ;generic function
   "STROKE-EQUAL"			  ;generic function
   "STROKE-KEYS"			  ;generic function
   "STROKE-MODIFIERS"			  ;generic function
   "STROKE-NAME"			  ;generic function
   "STYLE-BOTTOM-MARGIN"		  ;generic function
   "STYLE-FRAME-ITEM-PIXMAPS"		  ;generic function
   "STYLE-HMARGIN"			  ;generic function
   "STYLE-LEFT-MARGIN"			  ;generic function
   "STYLE-NB-BUTTONS"			  ;generic function
   "STYLE-PARTS-TO-REDRAW-ON-FOCUS"	  ;generic function
   "STYLE-PIXMAP-TABLE"			  ;generic function
   "STYLE-RIGHT-MARGIN"			  ;generic function
   "STYLE-TITLE-BAR-DIRECTION"		  ;generic function
   "STYLE-TITLE-BAR-POSITION"		  ;generic function
   "STYLE-TOP-MARGIN"			  ;generic function
   "STYLE-VMARGIN"			  ;generic function
   "THEME-DEFAULT-STYLE"		  ;generic function
   "THEME-NAME"				  ;generic function
   "THEME-TRANSIENT-STYLE"		  ;generic function
   "TOP-LEFT-H"				  ;generic function
   "TOP-LEFT-W"				  ;generic function
   "TOP-RIGHT-H"			  ;generic function
   "TOP-RIGHT-W"			  ;generic function
   "UNICONIFY"				  ;generic function
   "UNMAP-ICON-WINDOW"			  ;generic function
   "WIDGET-GCONTEXT"			  ;generic function
   "WIDGET-WINDOW"			  ;generic function

   "ACTION"				  ;macro
   "ATOM-NAME->ID"			  ;macro
   "CURRENT-DESK"			  ;macro
   "DEFINE-THEME"			  ;macro
   "DEFTYPEDPARAMETER"			  ;macro
   "ID->ATOM-NAME"			  ;macro
   "WITH-ROOT-CURSOR"			  ;macro

   "+ANY-DESKTOP+"			  ;constant
   "+DECORATION-EVENT-MASK+"		  ;constant
   "+EDGE-EVENT-MASK+"			  ;constant
   "+FOCUSABLE-MASK+"			  ;constant
   "+GNOME-PROTOCOLS+"			  ;constant
   "+NETWM-PROTOCOL+"			  ;constant
   "+POINTER-EVENT-MASK+"		  ;constant
   "+PUSH-BUTTON-MASK+"			  ;constant
   "+STD-BUTTON-MASK+"			  ;constant
   "+UNFOCUSABLE-MASK+"			  ;constant

   "*WHITE*"                              ;variable
   "*BLACK*"                              ;variable
   "*ROOT*"				  ;variable
   "*ROOT-WINDOW*"			  ;variable
   "*STDERR*"				  ;variable
   "*CURSOR-2*"				  ;variable
   "*DISPLAY*"				  ;variable
   "*ECLIPSE-DIRECTORY*"		  ;variable

   ;; user custom.
   "*CHANGE-DESKTOP-MESSAGE-ACTIVE-P*"	  ;variable
   "*CLOSE-DISPLAY-P*"			  ;variable
   "*DOUBLE-CLICK-SPEED*"                 ;variable
   "*FOCUS-TYPE*"			  ;variable
   "*FOCUS-NEW-MAPPED-WINDOW*"		  ;variable
   "*FOCUS-WHEN-WINDOW-CYCLE*"		  ;variable
   "*ICON-BOX-SORT-FUNCTION*"		  ;variable
   "*ICON-HINTS*"			  ;variable
   "*MENU-1-ITEMS*"			  ;variable
   "*MOVE-MODE*"			  ;variable
   "*RESIZE-MODE*"			  ;variable
   "*VERBOSE-MOVE*"			  ;variable
   "*VERBOSE-RESIZE*"			  ;variable
   "*WARP-POINTER-WHEN-CYCLE*"		  ;variable
   ))
