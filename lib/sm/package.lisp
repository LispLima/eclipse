;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: COMMON-LISP; -*-
;;; $Id: package.lisp,v 1.1 2004/01/12 11:10:52 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: SM Library
;;;   Created: 2004 01 15 15:28
;;;    Author: Iban Hatchondo <hatchond@labri.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Iban Hatchondo

;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; NOTE: All comments below are from the original, modified only for
;;; readability. (added for package doc).
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a 
;;; copy of this software and associated documentation files (the ``Software'')
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;; OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;; Except as contained in this notice, the name of the X Consortium shall not
;;; be used in advertising or otherwise to promote the sale, use or other
;;; dealings in this Software without prior written authorization from the X
;;; Consortium.
;;; (X Window System is a trademark of X Consortium, Inc.)

(common-lisp:in-package :common-lisp-user)

(defpackage "SM-LIB"
  (:use common-lisp ice-lib)
  (:documentation 
   "The purpose of the X Session Management Protocol (XSMP) is to provide a
    uniform mechanism for users to save and restore their sessions. A
    session is a group of clients, each of which has a particular state.
    The session is controlled by a network service called the session manager.
    The session manager issues commands to its clients on behalf of the user.
    These commands may cause clients to save their state or to terminate. It is
    expected that the client will save its state in such a way that the client
    can be restarted at a later time and resume its operation as if it had
    never been terminated.  A client's state might include information about
    the file currently being edited, the current position of the insertion
    point within the file, or the start of an uncommitted transaction. The
    means by which clients are restarted is unspecified by this protocol.

    For purposes of this protocol, a \fIclient\fP of the session manager is
    defined as a connection to the session manager. A client is typically,
    though not necessarily, a process running an application program connected
    to an X Window System display.  However, a client may be connected to more
    than one X display or not be connected to any X displays at all.

    This protocol is layered on top of the X Consortium's ICE protocol and
    relies on the ICE protocol to handle connection management and
    authentication.

    This a small implementation of the protocol specification that contains
    only the functions that are used on the client side. If you want to build a
    SM server you'll need to implement some functionalities describe in the 
    specification.

    protocol specification can be found at: 
     <ftp://ftp.xfree86.org/X.Org/pub/R6.6/xc/doc/hardcopy/SM/xsmp.PS.gz>")
  (:import-from :ice-lib 
    "MAKE-DEFAULT-VERSIONS"
    "AUTHENTICATION-PROTOCOL-INDEX"
    "PROTOCOL-MAJOR-OPCODE"
    "MAJOR-OPCODE" "OFFENDING-MINOR-OPCODE"
    "VERSION-INDEX" "VENDOR-NAME" "RELEASE-NAME"
    "SINTERN"
    )
  (:shadow
    "STRING-LENGTH" "BUFFER-READ-STRING" "BUFFER-WRITE-STRING"
    "BUFFER-READ-STRINGS" "BUFFER-WRITE-STRINGS")
  (:size 90)
  (:export 
   ;; classes.                      minor opcode
   "REGISTER-CLIENT"              ; 1
   "REGISTER-CLIENT-REPLY"        ; 2
   "SAVE-YOURSELF"               ; 3
   "SAVE-YOURSELF-REQUEST"       ; 4
   "INTERACT-REQUEST"             ; 5
   "INTERACT"                     ; 6
   "INTERACT-DONE"                ; 7
   "SAVE-YOURSELF-DONE"          ; 8
   "DIE"                          ; 9
   "SHUTDOWN-CANCELLED"           ; 10
   "CONNECTION-CLOSED"            ; 11 
   "SET-PROPERTIES"               ; 12
   "DELETE-PROPERTIES"            ; 13
   "GET-PROPERTIES"               ; 14
   "GET-PROPERTIES-REPLY"         ; 15
   "SAVE-YOURSELF-PHASE2-REQUEST" ; 16
   "SAVE-YOURSELF-PHASE2"         ; 17
   "SAVE-COMPLETE"                ; 18

   ;; structures.
   "PROPERTY"
   
   ;; condition
   "SESSION-MANAGER-UNAVAILABLE"

   ;; sm lib classes    
   "SM-CONNECTION"

   ;; types
   "INTERACT-STYLE"
   "DIALOG-TYPE"
   "SAVE-TYPE"
   "ARRAY8"
   "ARRAY8S"
   "PROPERTIES"
   
   ;; constants & vars
   "+SM-PROTO-MAJOR+"
   "+SM-PROTO-MINOR+"

   ;; macros.
   "BUFFER-READ-ARRAY8"
   "BUFFER-READ-ARRAY8S"
   "BUFFER-READ-CLIENT-ID"
   "BUFFER-READ-DIALOG-TYPE"
   "BUFFER-READ-INTERACT-STYLE"
   "BUFFER-READ-PROPERTIES"
   "BUFFER-READ-PROPERTY"
   "BUFFER-READ-SAVE-TYPE"
   "BUFFER-WRITE-ARRAY8"
   "BUFFER-WRITE-ARRAY8S"
   "BUFFER-WRITE-CLIENT-ID"
   "BUFFER-WRITE-DIALOG-TYPE"
   "BUFFER-WRITE-INTERACT-STYLE"
   "BUFFER-WRITE-PROPERTIES"
   "BUFFER-WRITE-PROPERTY"
   "BUFFER-WRITE-SAVE-TYPE"
   "SIGNAL-SM-ERROR"

   ;; functions
   "CONNECTION-CLOSED-P"
   "DELETE-PROPERTIES-P"
   "DIE-P"
   "GET-PROPERTIES-P"
   "GET-PROPERTIES-REPLY-P"
   "INTERACT-DONE-P"
   "INTERACT-P"
   "INTERACT-REQUEST-P"
   "PROPERTY-P"
   "REGISTER-CLIENT-P"
   "REGISTER-CLIENT-REPLY-P"
   "REGISTER-XSMP-PROTOCOL"
   "SAVE-COMPLETE-P"
   "SAVE-YOURSELF-DONE-P"
   "SAVE-YOURSELF-P"
   "SAVE-YOURSELF-REQUEST-P"
   "SAVE-YOURSELF-PHASE2-P"
   "SAVE-YOURSELF-PHASE2-REQUEST-P"
   "SET-PROPERTIES-P"
   "SHUTDOWN-CANCELLED-P"

   "ARRAY8-LENGTH"
   "ARRAY8S-LENGTH"
   "CLIENT-ID-LENGTH"
   "DIALOG-TYPE-LENGTH"
   "INTERACT-STYLE-LENGTH"
   "PROPERTIES-LENGTH"
   "PROPERTY-LENGTH"
   "SAVE-TYPE-LENGTH"
   
   "COPY-PROPERTY"
   "MAKE-PROPERTY"
   "PROPERTY-NAME"
   "PROPERTY-TYPE"
   "PROPERTY-VALUES"
   "MAKE-ARRAY8"

   "CLOSE-SM-CONNECTION"
   "CONNECTION-CLOSED-REASON"
   "OPEN-SM-CONNECTION"   
   "SESSION-MANAGER-UNAVAILABLE-REASON"

   ;; methods
   "DELETE-PROPERTIES-PROPERTIES"
   "GET-PROPERTIES-REPLY-PROPERTIES"
   "INTERACT-DONE-CANCEL-SHUTDOWN-P"
   "INTERACT-REQUEST-DIALOG-TYPE"
   "REGISTER-CLIENT-PREVIOUS-ID"
   "REGISTER-CLIENT-REPLY-CLIENT-ID"
   "SAVE-YOURSELF-DONE-SUCCESS-P"
   "SAVE-YOURSELF-FAST-P"
   "SAVE-YOURSELF-INTERACT-STYLE"
   "SAVE-YOURSELF-REQUEST-FAST-P"
   "SAVE-YOURSELF-REQUEST-GLOBAL-P"
   "SAVE-YOURSELF-REQUEST-INTERACT-STYLE"
   "SAVE-YOURSELF-REQUEST-SHUTDOWN-P"
   "SAVE-YOURSELF-REQUEST-TYPE"
   "SAVE-YOURSELF-SHUTDOWN-P"
   "SAVE-YOURSELF-TYPE"
   "SET-PROPERTIES-PROPERTIES"
   "SM-CLIENT-ID"
   "SM-PROTOCOL-REVISION"
   "SM-PROTOCOL-VERSION"
   "SM-RELEASE"
   "SM-VENDOR"
   ))
