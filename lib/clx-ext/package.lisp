;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: package.lisp,v 1.3 2003/09/11 00:06:34 hatchond Exp $
;;;
;;; This is the CLX extensions package definition.
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

(common-lisp:in-package :common-lisp-user)

(defpackage clx-extensions
  (:nicknames clx-ext)
  (:use common-lisp)
  (:size 200)
  (:export
   ;; from clx-extensions.lisp
   window-geometry
   window-position
   drawable-sizes
   with-event-mask
   with-pointer-grabbed
   get-environment-variable
   open-clx-display
   grab-button
   draw-glyphs
   translate
   text-width
   draw-centered-text
   send-configuration-notify

   ;; from cursor.lisp
   get-x-cursor

   ;; from event.lisp
   ;; events class name
   event
   keyboard-pointer-event
   pointer-event
   button-press
   button-release
   motion-notify
   keyboard-event
   key-press
   key-release
   in-out-window-event
   leave-notify
   enter-notify
   input-focus-event
   focus-in
   focus-out
   keymap-notify
   mapping-notify
   exposure
   no-exposure
   graphics-exposure
   circulate-notify
   configure-notify
   create-notify
   map-notify
   reparent-notify
   unmap-notify
   visibility-notify
   destroy-notify
   gravity-notify
   colormap-notify
   configure-request
   map-request
   resize-request
   circulate-request
   client-message
   property-notify
   selection-clear
   selection-notify
   selection-request
   
   ;; slots name
   above-sibling
   atom
   border-width
   code
   colormap
   configure-p
   count
   data
   drawable
   focus-p
   format
   hint-p
   installed-p
   keymap
   kind
   major
   minor
   mode
   new-p
   override-redirect-p
   parent
   place
   property
   request
   requestor
   root
   window
   child
   x
   y
   width
   height
   root-x
   root-y
   same-screen-p
   selection
   send-event-p
   stack-mode
   start
   state
   target
   time
   type
   value-mask

   ;; slots reader
   event-root
   event-event-window
   event-window
   event-child
   event-x
   event-y
   event-width
   event-height
   event-root-x
   event-root-y
   event-above-sibling
   event-atom
   event-border-width
   event-code
   event-colormap
   event-configure-p
   event-count
   event-data
   event-drawable
   event-focus-p
   event-format
   event-hint-p
   event-installed-p
   event-keymap
   event-kind
   event-mode
   event-new-p
   event-override-redirect-p
   event-parent
   event-place
   event-property
   event-request
   event-requestor
   event-same-screen-p
   event-selection
   event-send-event-p
   event-stack-mode
   event-start
   event-state
   event-target
   event-time
   event-type
   event-value-mask

   ; functions
   get-next-event

   ; methods
   make-event

   ;;
   )
  
  (:documentation ""))
