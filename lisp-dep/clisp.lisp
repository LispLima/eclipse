;;; -*- Mode: Lisp; Package: CLX-EXTENSIONS -*-
;;; $Id: $

;;; Specific clisp CLX patch.

(in-package :xlib)

(defparameter *known-authorizations* '("MIT-MAGIC-COOKIE-1"))

(defun homedir-file-pathname (name)
  (and #-(or unix mach) (search "Unix" (software-type) :test #'char-equal)
       (merge-pathnames (user-homedir-pathname) (pathname name))))

(defun authority-pathname ()
  (or (let ((xauthority (sys::getenv "XAUTHORITY")))
	(and xauthority (pathname xauthority)))
      (homedir-file-pathname ".Xauthority")))

(defun get-best-authorization (host display protocol)
  (labels ((read-short (stream &optional (eof-errorp t))
	     (let ((high-byte (read-byte stream eof-errorp)))
	       (when high-byte (dpb high-byte (byte 8 8) (read-byte stream)))))
	   (read-short-length-string (stream)
	     (let* ((length (read-short stream))
		    (string (make-string length)))
	       (dotimes (k length)
		 (setf (schar string k) (card8->char (read-byte stream))))
	       string))
	   (read-short-length-vector (stream)
	     (let* ((length (read-short stream))
		    (vector (make-array
			        length :element-type '(unsigned-byte 8))))
	       (dotimes (k length)
		 (setf (aref vector k) (read-byte stream)))
	       vector)))
    (let ((pathname (authority-pathname)))
      (when pathname
	(with-open-file (stream pathname
			 :element-type '(unsigned-byte 8)
			 :if-does-not-exist nil)
	  (when stream
	    (let* ((host-family (ecase protocol
				  ((:tcp :internet nil) 0)
				  ((:dna :DECnet) 1)
				  ((:chaos) 2)
				  ((:unix :local) 256)))
		   (host-address (if (member protocol '(:local :unix))
				     (map 'list #'char-int (machine-instance))
				     (rest (host-address host host-family))))
		   (best-name nil)
		   (best-data nil))
	      ;; Check for the localhost address, in which case we're
	      ;; really FamilyLocal.
	      (when (and (= host-family 0)
			 (equal host-address '(127 0 0 1)))
		(setq host-address (map 'list #'char-int (machine-instance)))
		(setq host-family 256))
	      (loop
	       (let ((family (read-short stream nil)))
		 (when (null family) (return))
		 (let* ((address (read-short-length-vector stream))
			(number (parse-integer
				    (read-short-length-string stream)))
			(name (read-short-length-string stream))
			(data (read-short-length-vector stream)))
		   (when (and (= family host-family)
			      (equal host-address (coerce address 'list))
			      (= number display)
			      (let ((pos1 (position
					      name *known-authorizations*
					      :test #'string=)))
				(and pos1
				     (or (null best-name)
					 (< pos1 (position best-name
						     *known-authorizations*
						     :test #'string=))))))
		     (setf best-name name)
		     (setf best-data data)))))
	      (when best-name
		(return-from get-best-authorization
		  (values best-name best-data)))))))))
  (values "" ""))
