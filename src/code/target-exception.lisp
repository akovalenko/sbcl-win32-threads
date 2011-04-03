;;;; code for handling Win32 exceptions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!WIN32")

;;;
;;; An awful lot of this stuff is stubbed out for now. We basically
;;; only handle inbound exceptions (the local equivalent to unblockable
;;; signals), and we're only picking off the sigsegv and sigfpe traps.
;;;
;;; This file is based on target-signal.lisp, but most of that went
;;; away. Some of it might want to be put back or emulated.
;;;

;;; SIGINT is handled like BREAK, except that ANSI BREAK ignores
;;; *DEBUGGER-HOOK*, but we want SIGINT's BREAK to respect it, so that
;;; SIGINT in --disable-debugger mode will cleanly terminate the system
;;; (by respecting the *DEBUGGER-HOOK* established in that mode).
;;;
;;; We'd like to have this work, but that would require some method of
;;; delivering a "blockable signal". Windows doesn't really have the
;;; concept, so we need to play with the threading functions to emulate
;;; it (especially since the local equivalent of SIGINT comes in on a
;;; separate thread). This is on the list for fixing later on, and will
;;; be required before we implement threads (because of stop-for-gc).
;;;
;;; This specific bit of functionality may well be implemented entirely
;;; in the runtime.
#||
(defun sigint-%break (format-string &rest format-arguments)
  (flet ((break-it ()
           (apply #'%break 'sigint format-string format-arguments)))
    (sb!thread:interrupt-thread (sb!thread::foreground-thread) #'break-it)))
||#

(define-alien-type ()
    (struct exception-record
            (exception-code dword)
            (exception-flags dword)
            (exception-record system-area-pointer)
            (exception-address system-area-pointer)
            (number-parameters dword)
            (exception-information system-area-pointer)))

;;; Map Windows Exception code to condition names: symbols or strings
(defvar *exception-code-map*
  (macrolet ((cons-name (symbol)
               `(cons ,symbol ,(remove #\+ (substitute #\_ #\- (string symbol))))))
    (list
     ;; Floating point exceptions
     (cons +exception-flt-divide-by-zero+    'division-by-zero)
     (cons +exception-flt-invalid-operation+ 'floating-point-invalid-operation)
     (cons +exception-flt-underflow+         'floating-point-underflow)
     (cons +exception-flt-overflow+          'floating-point-overflow)
     (cons +exception-flt-inexact-result+    'floating-point-inexact)
     (cons +exception-flt-denormal-operand+
           (lambda (&key &allow-other-keys)
             (error 'sb!kernel::floating-point-exception
                    :traps (getf (get-floating-point-modes) :traps))))
     (cons +exception-access-violation+
           (lambda (&key context memory-fault-address
                    &allow-other-keys)
             (error 'memory-fault-error
                    :context (sap-int context)
                    :address (sap-int memory-fault-address))))
     (cons +exception-flt-stack-check+       'floating-point-invalid-operation)
     ;; Stack overflow
     (cons +exception-stack-overflow+        'sb!kernel::control-stack-exhausted)
     ;; Various
     (cons-name +exception-single-step+)
     (cons-name +exception-array-bounds-exceeded+)
     (cons-name +exception-breakpoint+)
     (cons-name +exception-datatype-misalignment+)
     (cons-name +exception-illegal-instruction+)
     (cons-name +exception-in-page-error+)
     (cons-name +exception-int-divide-by-zero+)
     (cons-name +exception-int-overflow+)
     (cons-name +exception-invalid-disposition+)
     (cons-name +exception-noncontinuable-exception+)
     (cons-name +exception-priv-instruction+))))

;;; Actual exception handler. We hit something the runtime doesn't
;;; want to or know how to deal with (that is, not a sigtrap or gc wp
;;; violation), so it calls us here.
(defun sb!kernel:handle-win32-exception (context-sap exception-record-sap)
  (let* ((record (deref (sap-alien exception-record-sap (* (struct exception-record)))))
         (code (slot record 'exception-code))
         (number-parameters (slot record 'number-parameters))
         (condition-name (cdr (assoc code *exception-code-map*)))
         (memory-fault-access-sap
          (and (eql code +exception-access-violation+)
               (> number-parameters 1)
               (sap-ref-sap
                (alien-sap (addr (slot record 'exception-information)))
                (alien-size system-area-pointer :bytes))))
         (pc (sap-int (slot record 'exception-address)))
         (sb!debug:*stack-top-hint*
          (nth-value 1 (sb!kernel:find-interrupted-name-and-frame))))
    (when (and memory-fault-access-sap
               (sap= memory-fault-access-sap
                     (extern-alien "undefined_alien_address"
                                   system-area-pointer)))
      (setf condition-name 'sb!kernel::undefined-alien-variable-error))
    (typecase condition-name
      (function
         (funcall condition-name
                  :context context-sap
                  :exception-record record
                  :memory-fault-address memory-fault-access-sap
                  :pc pc))
      (t (error 'sb!kernel::unhandled-exception :pc pc
                :code code :name condition-name)))))

;;;; etc.

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
;;; SBCL/Win32 comment:
;;;   I don't know if we still need this or not. Better safe for now.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))

(in-package "SB!UNIX")

#!+sb-thread
(progn
  (defun receive-pending-interrupt ()
    (receive-pending-interrupt))

  (defmacro with-interrupt-bindings (&body body)
    `(let*
         ;; KLUDGE: Whatever is on the PCL stacks before the interrupt
         ;; handler runs doesn't really matter, since we're not on the
         ;; same call stack, really -- and if we don't bind these (esp.
         ;; the cache one) we can get a bogus metacircle if an interrupt
         ;; handler calls a GF that was being computed when the interrupt
         ;; hit.
         ((sb!pcl::*cache-miss-values-stack* nil)
          (sb!pcl::*dfun-miss-gfs-on-stack* nil))
       ,@body))

;;; Evaluate CLEANUP-FORMS iff PROTECTED-FORM does a non-local exit.
  (defmacro nlx-protect (protected-form &rest cleanup-froms)
    (with-unique-names (completep)
      `(let ((,completep nil))
         (without-interrupts
           (unwind-protect
                (progn
                  (allow-with-interrupts
                    ,protected-form)
                  (setq ,completep t))
             (unless ,completep
               ,@cleanup-froms))))))

  (declaim (inline %unblock-deferrable-signals %unblock-gc-signals))
  (sb!alien:define-alien-routine ("unblock_deferrable_signals"
                                  %unblock-deferrable-signals)
      sb!alien:void
    (where sb!alien:unsigned)
    (old sb!alien:unsigned))
  (sb!alien:define-alien-routine ("unblock_gc_signals" %unblock-gc-signals)
      sb!alien:void
    (where sb!alien:unsigned)
    (old sb!alien:unsigned))

  (defun block-deferrable-signals ()
    (%block-deferrable-signals 0 0))

  (defun unblock-deferrable-signals ()
    (%unblock-deferrable-signals 0 0))

  (defun unblock-gc-signals ()
    (%unblock-gc-signals 0 0))

  (declaim (inline %block-deferrables-and-return-mask %apply-sigmask))
  (sb!alien:define-alien-routine ("block_deferrables_and_return_mask"
                                  %block-deferrables-and-return-mask)
      sb!alien:unsigned)
  (sb!alien:define-alien-routine ("apply_sigmask"
                                  %apply-sigmask)
      sb!alien:void
    (mask sb!alien:unsigned))

  (defmacro without-interrupts/with-deferrables-blocked (&body body)
    (let ((mask-var (gensym)))
      `(without-interrupts
         (let ((,mask-var (%block-deferrables-and-return-mask)))
           (unwind-protect
                (progn ,@body)
             (%apply-sigmask ,mask-var))))))

  (defun invoke-interruption (function)
    (without-interrupts
      ;; Reset signal mask: the C-side handler has blocked all
      ;; deferrable signals before funcalling into lisp. They are to be
      ;; unblocked the first time interrupts are enabled. With this
      ;; mechanism there are no extra frames on the stack from a
      ;; previous signal handler when the next signal is delivered
      ;; provided there is no WITH-INTERRUPTS.
      (let ((sb!unix::*unblock-deferrables-on-enabling-interrupts-p* t))
        (with-interrupt-bindings
          (let ((sb!debug:*stack-top-hint*
                 (nth-value 1 (sb!kernel:find-interrupted-name-and-frame))))
            (allow-with-interrupts
              (nlx-protect
               (funcall function)
               ;; We've been running with deferrables
               ;; blocked in Lisp called by a C signal
               ;; handler. If we return normally the sigmask
               ;; in the interrupted context is restored.
               ;; However, if we do an nlx the operating
               ;; system will not restore it for us.
               (when sb!unix::*unblock-deferrables-on-enabling-interrupts-p*
                 ;; This means that storms of interrupts
                 ;; doing an nlx can still run out of stack.
                 (unblock-deferrable-signals)))))))))

  (defmacro in-interruption ((&key) &body body)
    #!+sb-doc
    "Convenience macro on top of INVOKE-INTERRUPTION."
    `(dx-flet ((interruption () ,@body))
       (invoke-interruption #'interruption)))

  (defun sb!kernel:signal-cold-init-or-reinit ()
    #!+sb-doc
    "Enable all the default signals that Lisp knows how to deal with."
    (unblock-gc-signals)
    (unblock-deferrable-signals)
    (values)))
