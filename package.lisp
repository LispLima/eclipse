;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: package.lisp,v 1.17 2004/02/17 22:30:50 ihatchondo Exp $
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
   #:application 			  ;class
   #:base-widget 			  ;class
   #:bottom 				  ;class
   #:bottom-left 			  ;class
   #:bottom-right 			  ;class
   #:box-button				  ;class
   #:button 				  ;class
   #:close-button 			  ;class
   #:decoration				  ;class
   #:default-style 			  ;class
   #:eclipse-screens 			  ;class
   #:edge 				  ;class
   #:frame-style 			  ;class
   #:geometry 				  ;class
   #:icon 				  ;class
   #:iconify-button 			  ;class
   #:keystroke 				  ;class
   #:left 				  ;class
   #:maximize-button 			  ;class
   #:menu-button 			  ;class
   #:menu-item 				  ;class
   #:menu-leaf-item 			  ;class
   #:menu-widget 			  ;class
   #:mouse-stroke 			  ;class
   #:pop-up-menu 			  ;class
   #:push-button 			  ;class
   #:right 				  ;class
   #:stroke 				  ;class
   #:sub-menu 				  ;class
   #:theme 				  ;class
   #:title-bar 				  ;class
   #:top 				  ;class
   #:top-left 				  ;class
   #:top-right 				  ;class
   #:transient-style 			  ;class

   #:activate-move-resize                 ;function
   #:application-class 			  ;function
   #:application-class-name 		  ;function
   #:application-class-type 		  ;function
   #:application-find 			  ;function
   #:application-list 			  ;function
   #:application-name 			  ;function
   #:application-p 			  ;function
   #:base-widget-p 			  ;function
   #:button-p 				  ;function
   #:check-size 			  ;function
   #:circulate-window-up-and-down 	  ;function
   #:copy-geometry 			  ;function
   #:create-application 		  ;function
   #:create-button 			  ;function
   #:create-icon 			  ;function
   #:create-message-box 		  ;function
   #:current-vscreen                      ;function
   #:decode-netwm-icon-pixmap             ;function
   #:decoration-p 			  ;function
   #:decoration-theme 			  ;setf function
   #:decore-application 		  ;function
   #:default-handler 			  ;function
   #:default-style-p 			  ;function
   #:define-key-combo 			  ;function
   #:define-menu-3 			  ;function
   #:define-mouse-combo 		  ;function
   #:delete-properties 		          ;function
   #:dismiss-move-resize 		  ;function
   #:display-coordinates 		  ;function
   #:display-geometry 			  ;function
   #:display-infos 			  ;function
   #:draw-focused-decoration 		  ;function
   #:draw-unfocused-decoration 		  ;function
   #:draw-window-grid 			  ;function
   #:eclipse 				  ;function
   #:eclipse-path 			  ;function
   #:edge-position 			  ;function
   #:ensure-theme-directory-exists 	  ;function
   #:find-corner 			  ;function
   #:find-decoration-frame-style 	  ;function
   #:find-input-model 			  ;function
   #:finish-move 			  ;function
   #:finish-resize 			  ;function
   #:font-name 				  ;setf function
   #:free-theme 			  ;function
   #:fullscreenable-p 			  ;function
   #:fullscreen-mode 			  ;setf function
   #:geometry-coordinates 		  ;function
   #:geometry-h 			  ;function
   #:geometry-p 			  ;function
   #:geometry-sizes 			  ;function
   #:geometry-w 			  ;function
   #:geometry-x 			  ;function
   #:geometry-y 			  ;function
   #:get-max-item-width 		  ;function
   #:screen-content                       ;function
   #:get-visible-windows 		  ;function
   #:give-focus-to-next-widget-in-desktop ;function
   #:highlight 				  ;function
   #:home-subdirectory 			  ;function
   #:icon-box-update 			  ;function
   #:icon-p 				  ;function
   #:icon-sort-creation-order 		  ;function
   #:icon-sort-name 			  ;function
   #:icon-sort-type 			  ;function
   #:init-edges-cursors 		  ;function
   #:init-gnome-compliance 		  ;function
   #:initialize-clone 			  ;function
   #:initialize-frame 			  ;function
   #:initialize 			  ;function
   #:initialize-geometry-info-box 	  ;function
   #:initialize-manager 		  ;function
   #:initialize-move                      ;function
   #:initialize-resize                    ;function
   #:init-log-file 			  ;function
   #:input-focus                          ;function
   #:keycode-registered-p 		  ;function
   #:keystroke-p 			  ;function
   #:kill-client-window 		  ;function
   #:load-config-file 			  ;function
   #:load-pnm->pixmap 			  ;function
   #:load-theme 			  ;function
   #:lookup-keystroke 			  ;function
   #:lookup-mouse-stroke 		  ;function
   #:lookup-theme                         ;function
   #:lookup-widget 			  ;function
   #:make-background-pixmap 		  ;function
   #:make-buttons-bar 			  ;function
   #:make-decoration 			  ;function
   #:make-frame 			  ;function
   #:make-geometry 			  ;function
   #:make-keystroke 			  ;function
   #:make-menu-button 			  ;function
   #:make-menu-button-menu 		  ;function
   #:make-menu-leaf-item 		  ;function
   #:make-mouse-stroke 			  ;function
   #:make-pop-up 			  ;function
   #:make-raise-window 			  ;function
   #:make-running-menu 			  ;function
   #:make-sub-menu 			  ;function
   #:make-substructure 			  ;function
   #:make-title-bar 			  ;function
   #:make-viewport-property 		  ;function
   #:maximize-modifier                    ;setf function
   #:maximize-window 			  ;function
   #:motif-wm-decoration 		  ;function
   #:mouse-stroke-for-move-and-resize 	  ;function
   #:mouse-stroke-p 			  ;function
   #:move-cursor-down 			  ;function
   #:move-cursor-left 			  ;function
   #:move-cursor-right 			  ;function
   #:move-cursor-up 			  ;function
   #:move-widget 			  ;function
   #:number-of-virtual-screens 		  ;function, setf
   #:perform-click 			  ;function
   #:pixmap-height 			  ;function
   #:pixmap-width 			  ;function
   #:procede-decoration 		  ;function
   #:query-application-tree 		  ;function
   #:%quit% 				  ;function
   #:realize-menu-items 		  ;function
   #:realize-pop-up 			  ;function
   #:recompute-wm-normal-hints 		  ;function
   #:region-intersect-region-p            ;function
   #:region-intersect-window-in-screen    ;function
   #:register-all-keystrokes 		  ;function
   #:register-all-mouse-strokes 	  ;function
   #:run-application 			  ;function
   #:send-wm-protocols-client-message 	  ;function
   #:stick-p 				  ;function
   #:timed-message-box 			  ;function
   #:title-bar-horizontal-p 		  ;function
   #:transient-style-p 			  ;function
   #:undecore-application 		  ;function
   #:undraw-geometry-info-box 		  ;function
   #:unregister-all-keystrokes 		  ;function
   #:unregister-all-mouse-strokes 	  ;function
   #:update-clone 			  ;function
   #:update-edges-geometry 		  ;function
   #:update-lists 			  ;function
   #:update-title-bar-sizes 		  ;function
   #:where-is-pointer 			  ;function
   #:widget->frame-item-key               ;function
   #:window-desktop-num 		  ;function, setf
   #:window-not-decorable-p               ;function
   #:window-priority                      ;setf function
   #:window-transient-p 		  ;function
   #:wm-icon-name 			  ;function
   #:wm-name 				  ;function
   #:wm-state 				  ;function
   #:workspace-names 			  ;function, setf

   #:add-desktop-application 		  ;generic function
   #:application-icon 			  ;generic function
   #:application-iconic-p 		  ;generic function
   #:application-input-model 		  ;generic function
   #:application-master 		  ;generic function
   #:application-wants-focus-p 		  ;generic function
   #:arm-branch 			  ;generic function
   #:arm 				  ;generic function
   #:bottom-left-h 			  ;generic function
   #:bottom-left-w 			  ;generic function
   #:bottom-right-h 			  ;generic function
   #:bottom-right-w 			  ;generic function
   #:button-active-p 			  ;generic function
   #:button-armed 			  ;generic function
   #:button-item-to-draw 		  ;generic function
   #:button-master 			  ;generic function
   #:change-vscreen 			  ;generic function
   #:circulate-window 			  ;generic function
   #:close-widget 			  ;generic function
   #:decoration-active-p 		  ;generic function
   #:decoration-application-gravity 	  ;generic function
   #:decoration-children 		  ;generic function
   #:decoration-frame-style 		  ;generic function
   #:decoration-wm-hints 		  ;generic function
   #:decoration-wm-size-hints 		  ;generic function
   #:default-modifiers-p 		  ;generic function
   #:destroy-substructure 		  ;generic function
   #:disarm 				  ;generic function
   #:dispatch-repaint 			  ;generic function
   #:draw-on-focus-in 			  ;generic function
   #:draw-on-focus-out 			  ;generic function
   #:event-process 			  ;generic function
   #:focused-p 				  ;generic function
   #:focus-widget 			  ;generic function
   #:frame-button-sizes 		  ;generic function
   #:frame-item-exist-p 		  ;generic function
   #:frame-item-height 			  ;generic function
   #:frame-item-pixmaps 		  ;generic function
   #:frame-item-sizes 			  ;generic function
   #:frame-item-width 			  ;generic function
   #:free-frame-style 			  ;generic function
   #:get-child 				  ;generic function
   #:get-pixmap 			  ;generic function
   #:icon-application 			  ;generic function
   #:icon-creation-time 		  ;generic function
   #:icon-desiconify-p 			  ;generic function
   #:iconify 				  ;generic function
   #:initialize-move 			  ;generic function
   #:keystroke-keysyms 			  ;generic function
   #:menu-3-process 			  ;generic function
   #:menu-leaf-callback 		  ;generic function
   #:menu-root-application-window 	  ;generic function
   #:menu-root 				  ;generic function
   #:mouse-stroke-button 		  ;generic function
   #:put-on-top                           ;generic function
   #:put-on-bottom                        ;generic function
   #:repaint 				  ;generic function
   #:remove-desktop-application 	  ;generic function
   #:remove-widget 			  ;generic function
   #:resize-from 			  ;generic function
   #:resize 				  ;generic function
   #:root-decoration-theme 		  ;generic function
   #:root-default-cursor 		  ;generic function
   #:root-desktop 			  ;generic function
   #:root-move-status 			  ;generic function
   #:root-resize-status 		  ;generic function
   #:set-focus 				  ;generic function
   #:shade                                ;generic function
   #:shaded-p                             ;generic function
   #:stroke-action 			  ;generic function
   #:stroke-equal 			  ;generic function
   #:stroke-keys 			  ;generic function
   #:stroke-modifiers 			  ;generic function
   #:stroke-name 			  ;generic function
   #:style-bottom-margin 		  ;generic function
   #:style-frame-item-pixmaps 		  ;generic function
   #:style-hmargin 			  ;generic function
   #:style-left-margin 			  ;generic function
   #:style-nb-buttons 			  ;generic function
   #:style-parts-to-redraw-on-focus 	  ;generic function
   #:style-pixmap-table 		  ;generic function
   #:style-right-margin 		  ;generic function
   #:style-title-bar-direction 		  ;generic function
   #:style-title-bar-position 		  ;generic function
   #:style-top-margin 			  ;generic function
   #:style-vmargin 			  ;generic function
   #:theme-default-style 		  ;generic function
   #:theme-name 			  ;generic function
   #:theme-transient-style 		  ;generic function
   #:top-left-h 			  ;generic function
   #:top-left-w 			  ;generic function
   #:top-right-h 			  ;generic function
   #:top-right-w 			  ;generic function
   #:uniconify 				  ;generic function
   #:unmap-icon-window 			  ;generic function
   #:widget-gcontext 			  ;generic function
   #:widget-window 			  ;generic function

   #:action 				  ;macro
   #:atom-name->id 			  ;macro
   #:current-desk 			  ;macro
   #:define-theme 			  ;macro
   #:deftypedparameter 			  ;macro
   #:id->atom-name 			  ;macro
   #:with-root-cursor 			  ;macro
   #:with-gensym                          ;macro

   #:+any-desktop+ 			  ;constant
   #:+application-mask+                   ;constant
   #:+decoration-event-mask+ 		  ;constant
   #:+edge-event-mask+ 			  ;constant
   #:+gnome-protocols+ 			  ;constant
   #:+netwm-protocol+ 			  ;constant
   #:+pointer-event-mask+ 		  ;constant
   #:+push-button-mask+ 		  ;constant
   #:+std-button-mask+ 			  ;constant

   #:*white*                              ;variable
   #:*black*                              ;variable
   #:*root* 				  ;variable
   #:*root-window* 			  ;variable
   #:*stderr* 				  ;variable
   #:*cursor-2* 			  ;variable
   #:*display* 				  ;variable
   #:*eclipse-directory* 		  ;variable

   ;; user custom variables.
   #:*change-desktop-message-active-p* 	  
   #:*close-display-p* 			  
   #:*cycle-icons-p*                       
   #:*double-click-speed*                  
   #:*focus-type* 			  
   #:*focus-new-mapped-window* 		  
   #:*focus-when-window-cycle* 		  
   #:*icon-box-sort-function* 		  
   #:*icon-hints* 			  
   #:*maximize-fill*                       
   #:*maximize-modifier*                   
   #:*menu-1-items* 			  
   #:*move-mode* 			  
   #:*resize-mode* 			  
   #:*save-and-restore-pointer-position-during-workspace-switch* 
   #:*screen-edge-resistant-p*             
   #:*standard-window-edge-resistant-p*    
   #:*verbose-move* 			  
   #:*verbose-resize* 			  
   #:*verbose-window-cycling*              
   #:*warp-pointer-when-cycle*    
   ))

(defpackage  ECLIPSE-EXTENSIONS 
  (:nicknames eclipse-ext)
  (:use clx-extensions eclipse-internals common-lisp)
  (:size 50)
  (:documentation
   "This is the package definition in which Eclipse extensions should be
   defined. If you want to write an extension for eclipse export in this
   package all the external symbols that your extension provides."))
