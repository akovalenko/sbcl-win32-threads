;;;; Support for userspace-scheduled thread-like fibers in the target machine.

;;;; Current implementation, restricted to win32, uses system
;;;; fibers. For unix-like systems with native pthreads something
;;;; similar could be done with setjmp/longjmp.
;;;;
;;;; Our C-level wrapper for WinNT system fibers adds a concept of
;;;; ownership: though system fibers as such aren't "owned" by
;;;; threads, pthreads_win32-managed fibers are. Once created, fibers
;;;; can be transferred to other threads.
;;;;
;;;; This implementation is not yet ready to be used in applications
;;;; as a userspace scheduling tool (coroutines, one-shot
;;;; continuations); it will work in principle, but there are too many
;;;; uncovered pits to be checked, amended or documented in the
;;;; future. Its main use, especially for win32, is accepting
;;;; callbacks in foreign threads, running their code in the same
;;;; thread where they were received.
;;;;
;;;; SBCL's problem with foreign thread callbacks is caused by the
;;;; following facts:
;;;;
;;;; * Each SBCL thread is set up (cleaned up) in a complicated
;;;;   manner, around the extent where it's visible to Lisp and its
;;;;   GC, and where it may be used for Lisp code evaluation.
;;;;
;;;; * Foreign thread callbacks are invoked on foreign stack. Even if
;;;;   it's detected by examining TLS storage, SBCL can't wrap its
;;;;   thread initialization code around this stack.
;;;;
;;;; * GC has to examine other threads' contexts. It relies on many
;;;;   specific features of SBCL-created threads, and expects control
;;;;   stack to be continuous (and, on some platforms, page-aligned).
;;;;
;;;; * SBCL expects only Lisp threads to create new Lisp
;;;;   threads. Special support for initial thread context setup is
;;;;   provided, only for (initially)non-Lisp application main thread.
;;;;
;;;; There are approximately three classes of solutions. We could:
;;;;
;;;; 1. Create Lisp thread context each time when a callback is
;;;; invoked by foreign thread. Problem1.1: foreign stack may be too
;;;; small, and we have no control over it. Problem1.2: doing thread
;;;; context setup multiple times for the same foreign thread is
;;;; wasteful.
;;;;
;;;; 2. Create a thread running a foreign callback server. When
;;;; foreign callback is detected, a message is sent to server;
;;;; foreign thread waits for an answer and then returns. Multiple
;;;; server threads may be used to allow several simultaneous
;;;; callbacks. Problem2.1. Callouts-in-callback don't run on the same
;;;; thread where callback is invoked. Foreign libraries with
;;;; thread-specific state might not expect it. Problem2.2. Destroying
;;;; unused thread contexts is complicated; naive methods can be
;;;; ineffective and wasteful in more than one way.
;;;;
;;;; 3. Create a thread context for foreign threads from Lisp
;;;; (possibly on demand, e.g. with client-server approach), then run
;;;; foreign calls in their native foreign threads, but on this Lisp
;;;; stack context. Solves problems 1.1, 1.2, 2.1; if we create a
;;;; context once for each unknown foreign thread, problem 2.2 is
;;;; solvable if we are able to detect when foreign thread exists
;;;; (fortunately the latter is trivial for win32). Fibers are useful
;;;; here: they _are_ little more than bare stack contexts without
;;;; threads behind them. System win32 fibers are easier to use than
;;;; setjmp/longjmp, because the system creates and manages fiber
;;;; stack address space itself.
;;;;
;;;; When a 3rd kind of solution is used, and the context is created
;;;; once per each foreign thread, it means, additionally, that
;;;; Lisp-level thread introspection provides valid results for
;;;; foreign threads too.

(in-package "SB-THREAD")

(load-shared-object "kernel32.dll")

;;; Typedef for foreign thread: unsigned word. Should be maintained to
;;; accept value range returned by (current-thread-os-thread),
;;; (thread-os-thread) etc.
(define-alien-type os-thread (unsigned #.sb-vm:n-word-bits))

;;; Adjust current thread settings in order to create fibers instead
;;; of threads. This module assumes that it's used locally only
;;; (i.e. enable, create fiber, disable in cleanup).  This approach is
;;; questionable, but here we go without adorning several layers of
;;; SBCL threading code with trivial thread/fiber check.
;;;
;;; This setting is per thread (and per fiber), it defaults to 0 and
;;; is not inherited.
(define-alien-routine ("pthread_np_set_fiber_factory_mode" set-fiber-factory-mode) int
  (enable int))

;;; Switch to fiber represented by os-thread value. There is also
;;; pthread_np_run_in_fiber, for running C callbacks in a fiber, using
;;; it as a "server" of sorts. Switching into fiber from Lisp is
;;; usually a way to terminate it.
(define-alien-routine ("pthread_np_switch_to_fiber" switch-to-fiber) int
  (target os-thread))

;;; Get current system thread id (for helpful thread naming).  Thread
;;; id is not unique if we speak of finished threads too, but it's
;;; still usable: many debugging utilities show process thread list
;;; using thread id as identifier. Among running threads it is unique.
(define-alien-routine ("GetCurrentThreadId" get-current-thread-id) int)

;;; Lisp representation of fiber factory thread.  Fiber factory is a
;;; fiber that is entered by newly noticed foreign threads and return
;;; a fresh companion Lisp fiber to run Lisp callbacks.
(defvar *lisp-fiber-factory-thread* nil)

;;; Lisp representation of fiber cleanup (monitor) thread.
(defvar *lisp-fiber-cleanup-thread* nil)

;;; A variable that is set (in a thread-local static SBCL-specific
;;; way) to T in all foreign thread companion fibers, allowing
;;; deinitialization code to filter this kind of threads
(defvar *current-thread-is-a-companion-fiber* nil)

;;; Create fiber (see MAKE-THREAD). Turned out that fiber running an
;;; empty inner function is still usable for many purposes, so
;;; function argument is now a keyword one, unlike in make-thread.
(defun make-fiber (&key function name ephemeral)
  (set-fiber-factory-mode 1)
  ;; SBCL uses semaphore to synchronize thread creator with thread
  ;; setup. Fibers aren't scheduled automatically now, with the only
  ;; exception of switching to main thread context on fiber exit. As
  ;; in WinNT API, we have to specify each transition explicitly.
  (let ((self (current-thread-os-thread)))
    (flet ((run-fiber ()
             ;; pthread_create in fiber mode switches into new fiber
             ;; immediately.  At this point, make-thread semaphore is
             ;; signalled (thread creating would hang otherwise).  Now
             ;; we switch back into originating thread, so the caller
             ;; may decide when and whether to start Lisp code in fiber.
             (switch-to-fiber self)
             ;; C-side callback API makes empty fiber functions useful.
             (when function (funcall function))))
      (unwind-protect (make-thread #'run-fiber :name name
                                   :ephemeral ephemeral)
        (set-fiber-factory-mode 0)))))

;;; Make fiber for entering callbacks in a foreign thread.  Fibers
;;; that are used for callbacks don't have to run any non-trivial
;;; function. MAKE-FOREIGN-THREAD-FIBER doesn't override the empty
;;; function default, but takes care on thread name and a flag that
;;; distinguishes such threads among others.
(defun make-foreign-thread-fiber ()
  (let ((fiber (make-fiber :name (format nil "foreign-thread :tid ~A"
                                         (get-current-thread-id))))
        ;; The binding below creates a TLS index for
        ;; *current-thread-is-a-companion-fiber*, if it has not been
        ;; established yet. Otherwise, both setting and getting
        ;; thread-local static value of a symbol will fail.
        (*current-thread-is-a-companion-fiber* nil))
    ;; After deinit, this flag is used to select outstanding foreign
    ;; thread fibers from all threads, to force their completion or to
    ;; wait for it (the latter applies to active calls).
    (setf (symbol-value-in-thread
           '*current-thread-is-a-companion-fiber* fiber) t)
    fiber))

;;; Alien callback, wrapping make-foreign-thread-fiber in a way usable
;;; with pthread_np_run_in_fiber
(sb-alien::define-alien-callback make-lisp-fiber (:cdecl void)
    ((fiber-pointer (* os-thread)))
  (setf (deref fiber-pointer)
        (thread-os-thread (make-foreign-thread-fiber))))

;;; Register, within SBCL runtime, a fiber that makes other fibers for
;;; foreign thread callbacks, and an alien function returning callback
;;; itself. This two-step horror is actually a convenient way to
;;; interface C.
(define-alien-routine fiber-announce-factory void
  (os-thread os-thread) (callback (* t)))

;;; Undo fiber factory announce, disable foreign callback support on C
;;; level.
(define-alien-routine fiber-deinit-runtime void)

(defun foreign-thread-init ()
  #+sb-doc
  "Initialize runtime support for foreign thread callbacks"
  (fiber-announce-factory
   (thread-os-thread
    (setf
     *lisp-fiber-factory-thread*
     (make-fiber :name "foreign-thread-helper"
                 :ephemeral t)))
   (alien-sap make-lisp-fiber)))

(defun foreign-thread-p (thread)
  (symbol-value-in-thread '*current-thread-is-a-companion-fiber* thread nil))

(defun foreign-thread-deinit ()
  #+sb-doc
  "Deinitialize runtime support for foreign thread callbacks"
  (alien-funcall (extern-alien fiber-deinit-runtime (function void)))
  (when *lisp-fiber-cleanup-thread*
    (join-thread *lisp-fiber-cleanup-thread* :default t))
  (setf *lisp-fiber-cleanup-thread* nil
        *lisp-fiber-factory-thread* nil)
  (let ((remaining-foreign-threads
         (remove-if-not #'foreign-thread-p (list-all-threads))))
    (dolist (thread (mapc #'switch-to-fiber remaining-foreign-threads))
      (join-thread thread :default nil))))
