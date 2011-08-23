;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:sb-thread)

(define-alien-routine ("SetConsoleCtrlHandler" set-console-ctrl-handler) int
  (callback (function (:stdcall int) int))
  (enable int))

(defun windows-console-control-handler (event-code)
  (case event-code
    (0
     (flet ((interrupt-it ()
              (with-interrupts
                (let ((int (make-condition
                            'interactive-interrupt
                            :address "...<sorry, don't know the address>")))
                  ;; First SIGNAL, so that handlers can run.
                  (signal int)
                  (%break 'sigint int)))))
       (sb-thread:interrupt-thread (foreground-thread) #'interrupt-it)
       t))))

(defvar *console-control-handler* #'windows-console-control-handler)
(defvar *console-control-enabled* nil)
(defvar *console-control-spec* nil)

(sb-alien::define-alien-callback alien-console-control-handler (:stdcall int)
    ((event-code int))
  (if (ignore-errors (funcall *console-control-handler* event-code)) 1 0))

(defun console-control-handler ()
  "Get or set windows console control handler.

Boolean value: use default handler (NIL) or ignore event (T).  Symbol
or function: designator for a function that receives an event code and
returns generalized boolean (false to fall back to other handlers,
true to stop searching)." *console-control-spec*)

(defun (setf console-control-handler) (new-handler)
  (etypecase new-handler
    (boolean
     (aver (plusp (set-console-ctrl-handler
                   (sap-alien (int-sap 0)
                              (function (:stdcall int) int))
                   (if new-handler 1 0))))
     (setf *console-control-enabled* nil))
    ((or symbol function)
     (setf *console-control-handler* new-handler)
     (aver (plusp (set-console-ctrl-handler alien-console-control-handler 1)))))
  (setf *console-control-spec* new-handler))

(defun initialize-console-control-handler (&optional reset-to-default-p)
  (setf (console-control-handler)
        (if reset-to-default-p
            'windows-console-control-handler
            (console-control-handler))))

(initialize-console-control-handler t)
(pushnew 'initialize-console-control-handler sb-ext:*init-hooks*)
