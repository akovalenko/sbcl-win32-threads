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
      (reserved1 system-string)
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
      (show-window unsigned-short)
      (reserved2 unsigned-short)
      (reserved3 (* t))
      (stdin handle)
      (stdout handle)
      (stderr handle)))

(defconstant +startf-use-std-handles+ #x100)

(define-alien-routine ("CreateProcessW" create-process) lispbool
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

(defun search-path (partial-name)
  "Searh executable using the system path"
  (with-alien ((pathname-buffer pathname-buffer))
    (syscall (("SearchPath" t) dword
              system-string
              system-string
              system-string
              dword
              (* t)
              (* t))
             (and (plusp result)
                  (values (decode-system-string pathname-buffer) result))
             nil partial-name nil
             max_path (cast pathname-buffer (* char)) nil)))

(define-alien-routine ("GetExitCodeProcess" get-exit-code-process) int
  (handle unsigned) (exit-code dword :out))

(define-alien-routine ("GetExitCodeThread" get-exit-code-thread) int
  (handle handle) (exit-code dword :out))

(defun mswin-spawn (program argv stdin stdout stderr searchp envp waitp)
  (declare (ignorable envp))
  (let ((std-handles (get-std-handles))
        (inheritp nil))
    (flet ((maybe-std-handle (arg)
             (let ((default (pop std-handles)))
               (case arg (-1 default) (otherwise (setf inheritp t) arg)))))
      (with-alien ((process-information process-information)
                   (startup-info startup-info))
        (setf (slot startup-info 'cb) (alien-size startup-info :bytes)
              (slot startup-info 'stdin) (maybe-std-handle stdin)
              (slot startup-info 'stdout) (maybe-std-handle stdout)
              (slot startup-info 'stderr) (maybe-std-handle stderr)
              (slot startup-info 'reserved1) nil
              (slot startup-info 'reserved2) 0
              (slot startup-info 'reserved3) nil
              (slot startup-info 'flags) (if inheritp +startf-use-std-handles+ 0))
        (without-interrupts
          (if (create-process (or (and searchp (search-path program)) program)
                              argv
                              nil nil
                              inheritp 0 nil nil
                              (alien-sap startup-info)
                              (alien-sap process-information))
              (let ((child (slot process-information 'process-handle)))
                (close-handle (slot process-information 'thread-handle))
                (if waitp
                    (do () ((/= 1 (with-local-interrupts (wait-object-or-signal child)))
                              (multiple-value-bind (got code) (get-exit-code-process child)
                                (if got code -1))))
                    child))
              -1))))))
