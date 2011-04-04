;;;; Windows API bindings not needed for cold initialization.
(in-package "SB-WIN32")

;;;; CreateProcess and surrounding data structures provide a way to implement
;;;; RUN-PROGRAM for the hard case when :FDS-ARE-WINDOWS-HANDLES. [[There is a
;;;; lot of work to do on RUN-PROGRAM anyway: its mainstream Win32 version that
;;;; works with FDs is full of bugs too]].

(define-alien-type process-information
    (struct process-information
      (process-handle handle)
      (thread-handle handle)
      (process-id dword)
      (thread-id dword)))

(define-alien-type startup-info
    (struct startup-info
      (cb dword)
      (nil system-string)
      (desktop system-string)
      (title system-string)
      (x dword)
      (y dword)
      (x-size dword)
      (y-size dword)
      (x-chars dword)
      (y-chars dword)
      (fill-attribute dword)
      (flags dword)
      (show-window word)
      (nil unsigned-short)
      (nil (* t))
      (stdin handle)
      (stdout handle)
      (stderr handle)))

(defconstant +startf-use-std-handles+ #x100)

(define-alien-routine ("CreateProcessW" create-process)
    lispbool
  (application-name system-string)
  (command-line system-string)
  (process-security-attributes (* t))
  (thread-security-attributes (* t))
  (inherit-handles-p lispbool)
  (creation-flags dword)
  (environment (* t))
  (current-directory system-string)
  (startup-info (* t))
  (process-information (* t)))

(defun mswin-spawn (program argv stdin stdout stderr searchp envp waitp)
  (declare (ignorable envp))
  (with-alien ((process-information process-information)
               (startup-info startup-info))
    (setf (slot startup-info 'cb) (alien-size startup-info :bytes)
          (slot startup-info 'stdin) stdin
          (slot startup-info 'stdout) stdout
          (slot startup-info 'stderr) stderr
          (slot startup-info 'flags) +startf-use-std-handles+)
    (without-interrupts
      (and (create-process (if searchp nil program)
                           argv
                           nil nil
                           t 0 nil nil
                           (alien-sap startup-info)
                           (alien-sap process-information))
           (let ((child (slot process-information 'process-handle)))
             (close-handle (slot process-information 'thread-handle))
             (if waitp
                 (do () ((/= 1 (with-local-interrupts (wait-object-or-signal child)))
                           (multiple-value-bind (got code) (get-exit-code-process child)
                             (if got code -1))))
                 child))))))
