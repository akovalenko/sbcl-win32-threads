;;;; win32 socket operations
;;;; these have all been done by hand since I can't seem
;;;; to get my head around the sb-grovel stuff

;;;; Winsock requires us to convert HANDLES to/from
;;;; file descriptors, so I've added an additional
;;;; package for the actual winsock alien defs, and then
;;;; in the sockint package, we implement wrappers that
;;;; handle the conversion.

;;; these are all of the basic structure alien defs
(in-package :sockint)

;;;; we are now going back to the normal sockint
;;;; package where we will redefine all of the above
;;;; functions, converting between HANDLES and fds

(defmacro handle->fd (handle flags)
  #+fds-are-windows-handles (declare (ignorable flags))
  #+fds-are-windows-handles handle
  #-fds-are-windows-handles `(handle->real-fd ,handle ,flags))

(defmacro fd->handle (fd)
  #+sbcl
  `(sb-win32:get-osfhandle ,fd)
  #-sbcl
  (fd->handle fd))

(defun socket (af type proto)
  (let* ((handle (wsa-socket af type proto nil 0 1))
         (fd (handle->fd handle 0)))
    fd))

(defmacro define-socket-fd-arg-routines (names)
  `(progn
     #+fds-are-windows-handles
     (declaim (inline ,@names))
     ,@(loop for routine in names
             for win32-routine = (intern
                                   (format nil "~A-~A" '#:win32 routine))
             collect
             `(defun ,routine (fd &rest options)
                (apply #',win32-routine (fd->handle fd) options)))))

(define-socket-fd-arg-routines
    (bind getsockname listen recv recvfrom send sendto close connect getpeername
          ioctl setsockopt getsockopt))

(defun accept (fd &rest options)
  (let ((handle (apply #'win32-accept (fd->handle fd) options)))
    (if (= handle -1)
        -1
        (handle->fd handle 0))))

(defun make-wsa-version (major minor)
  (dpb minor (byte 8 8) major))

(defvar *wsa-startup-call* nil)

(defun call-wsa-startup ()
  (setf *wsa-startup-call* (wsa-startup (make-wsa-version 2 2))))

;;; Startup!
(call-wsa-startup)

;;; Ensure startup for saved cores as well.
(push 'call-wsa-startup sb-ext:*init-hooks*)

;; not implemented on win32
(defconstant af-local 0)
(defconstant msg-dontwait 0)
(defconstant msg-trunc 0)
(defconstant msg-eor 0)
(defconstant msg-nosignal 0)
(defconstant msg-waitall 0)
(defconstant msg-eor 0)
(defconstant size-of-sockaddr-un 0)
(defun (setf sockaddr-un-family) (addr family) ())
(defun (setf sockaddr-un-path) (addr family) ())
(defun sockaddr-un-path (addr) ())
(defun free-sockaddr-un (addr) ())
(defun allocate-sockaddr-un () ())

