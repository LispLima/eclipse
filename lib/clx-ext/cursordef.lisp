;;; -*- Mode: Lisp; Package: Clx-Extensions -*-
;;; Copyright 1987, 1998  The Open Group
;;; $Id: cursordef.lisp,v 1.2 2003/12/03 14:32:38 ihatchondo Exp $
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation.
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Except as contained in this notice, the name of The Open Group shall
;;; not be used in advertising or otherwise to promote the sale, use or
;;; other dealings in this Software without prior written authorization
;;; from The Open Group.

(common-lisp:in-package :clx-extensions)

(declaim (type (simple-array keyword 78) +cursors+))

(defconstant +cursors+ 
  '#(:XC_X_CURSOR              :XC_ARROW
     :XC_BASED_ARROW_DOWN      :XC_BASED_ARROW_UP
     :XC_BOAT                  :XC_BOGOSITY
     :XC_BOTTOM_LEFT_CORNER    :XC_BOTTOM_RIGHT_CORNER
     :XC_BOTTOM_SIDE           :XC_BOTTOM_TEE
     :XC_BOX_SPIRAL            :XC_CENTER_PTR
     :XC_CIRCLE                :XC_CLOCK
     :XC_COFFEE_MUG            :XC_CROSS
     :XC_CROSS_REVERSE         :XC_CROSSHAIR
     :XC_DIAMOND_CROSS         :XC_DOT
     :XC_DOTBOX                :XC_DOUBLE_ARROW
     :XC_DRAFT_LARGE           :XC_DRAFT_SMALL
     :XC_DRAPED_BOX            :XC_EXCHANGE
     :XC_FLEUR                 :XC_GOBBLER
     :XC_GUMBY                 :XC_HAND1
     :XC_HAND2                 :XC_HEART
     :XC_ICON                  :XC_IRON_CROSS
     :XC_LEFT_PTR              :XC_LEFT_SIDE
     :XC_LEFT_TEE              :XC_LEFTBUTTON
     :XC_LL_ANGLE              :XC_LR_ANGLE
     :XC_MAN                   :XC_MIDDLEBUTTON
     :XC_MOUSE                 :XC_PENCIL
     :XC_PIRATE                :XC_PLUS
     :XC_QUESTION_ARROW        :XC_RIGHT_PTR
     :XC_RIGHT_SIDE            :XC_RIGHT_TEE
     :XC_RIGHTBUTTON           :XC_RTL_LOGO
     :XC_SAILBOAT              :XC_SB_DOWN_ARROW
     :XC_SB_H_DOUBLE_ARROW     :XC_SB_LEFT_ARROW
     :XC_SB_RIGHT_ARROW        :XC_SB_UP_ARROW
     :XC_SB_V_DOUBLE_ARROW     :XC_SHUTTLE
     :XC_SIZING                :XC_SPIDER
     :XC_SPRAYCAN              :XC_STAR
     :XC_TARGET                :XC_TCROSS
     :XC_TOP_LEFT_ARROW        :XC_TOP_LEFT_CORNER
     :XC_TOP_RIGHT_CORNER      :XC_TOP_SIDE
     :XC_TOP_TEE               :XC_TREK
     :XC_UL_ANGLE              :XC_UMBRELLA
     :XC_UR_ANGLE              :XC_WATCH
     :XC_XTERM                 :XC_NUM_GLYPHS))
