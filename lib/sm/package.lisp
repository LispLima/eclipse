;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: COMMON-LISP; -*-
;;; $Id: package.lisp,v 1.6 2004/12/14 17:58:18 ihatchondo Exp $
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
    #:make-default-versions
    #:authentication-protocol-index
    #:protocol-major-opcode
    #:major-opcode #:offending-minor-opcode
    #:version-index #:vendor-name #:release-name
    #:sintern #:index+
    )
  (:shadow
    #:get-properties
    #:string-length #:strings-length #:strings
    #:buffer-read-string #:buffer-write-string
    #:buffer-read-strings #:buffer-write-strings)
  (:size 90)
  (:export 
   ;; classes.                      minor opcode
   #:register-client              ; 1
   #:register-client-reply        ; 2
   #:save-yourself                ; 3
   #:save-yourself-request        ; 4
   #:interact-request             ; 5
   #:interact                     ; 6
   #:interact-done                ; 7
   #:save-yourself-done           ; 8
   #:die                          ; 9
   #:shutdown-cancelled           ; 10
   #:connection-closed            ; 11 
   #:set-properties               ; 12
   #:delete-properties            ; 13
   #:get-properties               ; 14
   #:get-properties-reply         ; 15
   #:save-yourself-phase2-request ; 16
   #:save-yourself-phase2         ; 17
   #:save-complete                ; 18

   ;; structures.
   #:property
   
   ;; condition
   #:session-manager-unavailable

   ;; sm lib classes    
   #:sm-connection

   ;; types
   #:interact-style
   #:dialog-type
   #:save-type
   #:array8
   #:array8s
   #:properties
   
   ;; constants & vars
   #:+sm-proto-major+
   #:+sm-proto-minor+

   ;; macros.
   #:buffer-read-array8
   #:buffer-read-array8s
   #:buffer-read-client-id
   #:buffer-read-dialog-type
   #:buffer-read-interact-style
   #:buffer-read-properties
   #:buffer-read-property
   #:buffer-read-save-type
   #:buffer-write-array8
   #:buffer-write-array8s
   #:buffer-write-client-id
   #:buffer-write-dialog-type
   #:buffer-write-interact-style
   #:buffer-write-properties
   #:buffer-write-property
   #:buffer-write-save-type
   #:signal-sm-error

   ;; functions
   #:connection-closed-p
   #:delete-properties-p
   #:die-p
   #:get-properties-p
   #:get-properties-reply-p
   #:interact-done-p
   #:interact-p
   #:interact-request-p
   #:property-p
   #:register-client-p
   #:register-client-reply-p
   #:register-xsmp-protocol
   #:save-complete-p
   #:save-yourself-done-p
   #:save-yourself-p
   #:save-yourself-request-p
   #:save-yourself-phase2-p
   #:save-yourself-phase2-request-p
   #:set-properties-p
   #:shutdown-cancelled-p

   #:array8-length
   #:array8s-length
   #:client-id-length
   #:dialog-type-length
   #:interact-style-length
   #:properties-length
   #:property-length
   #:save-type-length

   #:array8->string
   #:array8s->strings
   #:string->array8
   #:strings->array8s
   
   #:copy-property
   #:make-property
   #:property-name
   #:property-type
   #:property-values
   #:make-array8

   #:close-sm-connection
   #:connection-closed-reason
   #:open-sm-connection   
   #:session-manager-unavailable-reason

   ;; methods
   #:delete-properties-properties
   #:get-properties-reply-properties
   #:interact-done-cancel-shutdown-p
   #:interact-request-dialog-type
   #:register-client-previous-id
   #:register-client-reply-client-id
   #:save-yourself-done-success-p
   #:save-yourself-fast-p
   #:save-yourself-interact-style
   #:save-yourself-request-fast-p
   #:save-yourself-request-global-p
   #:save-yourself-request-interact-style
   #:save-yourself-request-shutdown-p
   #:save-yourself-request-type
   #:save-yourself-shutdown-p
   #:save-yourself-type
   #:set-properties-properties
   #:sm-client-id
   #:sm-protocol-revision
   #:sm-protocol-version
   #:sm-release
   #:sm-vendor
   ))
