;;;; miscellaneous tests of thread stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

; WHITE-BOX TESTS

(in-package "SB-THREAD")
(use-package :test-util)
(use-package "ASSERTOID")

(setf sb-unix::*on-dangerous-wait* :error)

(sb-alien:define-alien-routine "check_deferrables_blocked_or_lose"
    void
  (where sb-alien:unsigned-long))
(sb-alien:define-alien-routine "check_deferrables_unblocked_or_lose"
    void
  (where sb-alien:unsigned-long))

;(sb-unix::unblock-deferrable-signals)
(check-deferrables-unblocked-or-lose 0)

(defun wait-for-threads (threads)
  (mapc (lambda (thread) (sb-thread:join-thread thread :default nil)) threads)
  (assert (not (some #'sb-thread:thread-alive-p threads))))


(with-test (:name (:two-threads-running-gc))
  #+darwin
  (error "Hangs on Darwin.")
  (let (a-done b-done)
    (make-thread (lambda ()
                   (dotimes (i 1000)
                     (sb-ext:gc) (princ "\\") (force-output))
                   (setf a-done t)))
    (make-thread (lambda ()
                   (dotimes (i 250)
                     (sb-ext:gc :full t)
                     (princ "/") (force-output))
                   (setf b-done t)))
    (loop
      (when (and a-done b-done) (return))
      (sleep 1))))

(terpri)



;;; compare-and-swap

(defmacro defincf (name accessor &rest args)
  `(defun ,name (x)
     (let* ((old (,accessor x ,@args))
         (new (1+ old)))
    (loop until (eq old (sb-ext:compare-and-swap (,accessor x ,@args) old new))
       do (setf old (,accessor x ,@args)
                new (1+ old)))
    new)))

(defstruct cas-struct (slot 0))

(defincf incf-car car)
(defincf incf-cdr cdr)
(defincf incf-slot cas-struct-slot)
(defincf incf-symbol-value symbol-value)
(defincf incf-svref/1 svref 1)
(defincf incf-svref/0 svref 0)

(defmacro def-test-cas (name init incf op)
  `(with-test (:name ,name)
     (flet ((,name (n)
              (declare (fixnum n))
              (let* ((x ,init)
                     (run nil)
                     (threads
                      (loop repeat 10
                            collect (sb-thread:make-thread
                                     (lambda ()
                                       (loop until run
                                             do (sb-thread:thread-yield))
                                       (loop repeat n do (,incf x)))))))
                (setf run t)
                (dolist (th threads)
                  (sb-thread:join-thread th))
                (assert (= (,op x) (* 10 n))))))
       (,name 200000))))

(def-test-cas test-cas-car (cons 0 nil) incf-car car)
(def-test-cas test-cas-cdr (cons nil 0) incf-cdr cdr)
(def-test-cas test-cas-slot (make-cas-struct) incf-slot cas-struct-slot)
(def-test-cas test-cas-value (let ((x '.x.))
                               (set x 0)
                               x)
  incf-symbol-value symbol-value)
(def-test-cas test-cas-svref/0 (vector 0 nil) incf-svref/0 (lambda (x)
                                                             (svref x 0)))
(def-test-cas test-cas-svref/1 (vector nil 0) incf-svref/1 (lambda (x)
                                                             (svref x 1)))
(format t "~&compare-and-swap tests done~%")

(defun test-interrupt (function-to-interrupt &optional quit-p)
  (let ((child  (make-thread function-to-interrupt)))
    ;;(format t "gdb ./src/runtime/sbcl ~A~%attach ~A~%" child child)
    (sleep 2)
    (format t "interrupting child ~A~%" child)
    (interrupt-thread child
                      (lambda ()
                        (format t "child pid ~A~%" *current-thread*)
                        (when quit-p (sb-ext:quit))))
    (sleep 1)
    child))

;; separate tests for (a) interrupting Lisp code, (b) C code, (c) a syscall,
;; (d) waiting on a lock, (e) some code which we hope is likely to be
;; in pseudo-atomic

(defun alloc-stuff () (copy-list '(1 2 3 4 5)))

(with-test (:name (:interrupt-thread :interrupt-consing-child))
  #+darwin
  (error "Hangs on Darwin.")
  (let ((thread (sb-thread:make-thread (lambda () (loop (alloc-stuff))))))
    (let ((killers
           (loop repeat 10 collect
                 (sb-thread:make-thread
                  (lambda ()
                    (loop repeat 250 do
                          (sleep (random 0.1d0))
                          (princ ".")
                          (force-output)
                          (sb-thread:interrupt-thread thread (lambda ()))))))))
      (wait-for-threads killers)
      (sb-thread:terminate-thread thread)
      (wait-for-threads (list thread))))
  (sb-ext:gc :full t))

(format t "~&multi interrupt test done~%")

#+(or x86 x86-64) ;; x86oid-only, see internal commentary.
(with-test (:name (:interrupt-thread :interrupt-consing-child :again))
  #+darwin
  (error "Hangs on Darwin.")
  (let ((c (make-thread (lambda () (loop (alloc-stuff))))))
    ;; NB this only works on x86: other ports don't have a symbol for
    ;; pseudo-atomic atomicity
    (dotimes (i 250)
      (sleep (random 0.1d0))
      (interrupt-thread c
                        (lambda ()
                          (princ ".") (force-output)
                          (assert (thread-alive-p *current-thread*))
                          (assert
                           (not (logbitp 0 SB-KERNEL:*PSEUDO-ATOMIC-BITS*))))))
    (terminate-thread c)
    (wait-for-threads (list c))))

(format t "~&interrupt test done~%")

(defstruct counter (n 0 :type sb-vm:word))
(defvar *interrupt-counter* (make-counter))

(declaim (notinline check-interrupt-count))
(defun check-interrupt-count (i)
  (declare (optimize (debug 1) (speed 1)))
  ;; This used to lose if eflags were not restored after an interrupt.
  (unless (typep i 'fixnum)
    (error "!!!!!!!!!!!")))

(with-test (:name (:interrupt-thread :interrupt-ATOMIC-INCF))
  (let ((c (make-thread
            (lambda ()
              (handler-bind ((error #'(lambda (cond)
                                        (princ cond)
                                        (sb-debug:backtrace
                                         most-positive-fixnum))))
                (loop (check-interrupt-count
                       (counter-n *interrupt-counter*))))))))
    (let ((func (lambda ()
                  (princ ".")
                  (force-output)
                  (sb-ext:atomic-incf (counter-n *interrupt-counter*)))))
      (setf (counter-n *interrupt-counter*) 0)
      (dotimes (i 100)
        (sleep (random 0.1d0))
        (interrupt-thread c func))
      (loop until (= (counter-n *interrupt-counter*) 100) do (sleep 0.1))
      (terminate-thread c)
      (wait-for-threads (list c)))))

(format t "~&interrupt count test done~%")

(defvar *runningp* nil)

(with-test (:name (:interrupt-thread :no-nesting))
  (let ((thread (sb-thread:make-thread
                 (lambda ()
                   (catch 'xxx
                     (loop))))))
    (declare (special runningp))
    (sleep 0.2)
    (sb-thread:interrupt-thread thread
                                (lambda ()
                                    (let ((*runningp* t))
                                      (sleep 1))))
    (sleep 0.2)
    (sb-thread:interrupt-thread thread
                                (lambda ()
                                  (throw 'xxx *runningp*)))
    (assert (not (sb-thread:join-thread thread)))))

(with-test (:name (:interrupt-thread :nesting))
  (let ((thread (sb-thread:make-thread
                 (lambda ()
                   (catch 'xxx
                     (loop))))))
    (declare (special runningp))
    (sleep 0.2)
    (sb-thread:interrupt-thread thread
                                (lambda ()
                                  (let ((*runningp* t))
                                    (sb-sys:with-interrupts
                                      (sleep 1)))))
    (sleep 0.2)
    (sb-thread:interrupt-thread thread
                                (lambda ()
                                  (throw 'xxx *runningp*)))
    (assert (sb-thread:join-thread thread))))

(defun waste (&optional (n 100000))
  (loop repeat n do (make-string 16384)))

(with-test (:name (:one-thread-runs-gc-while-other-conses))
  (loop for i below 100 do
        (princ "!")
        (force-output)
        (sb-thread:make-thread
         #'(lambda ()
             (waste)))
        (waste)
        (sb-ext:gc)))

(terpri)

(defparameter *aaa* nil)
(with-test (:name (:one-thread-runs-gc-while-other-conses :again))
  (loop for i below 100 do
        (princ "!")
        (force-output)
        (sb-thread:make-thread
         #'(lambda ()
             (let ((*aaa* (waste)))
               (waste))))
        (let ((*aaa* (waste)))
          (waste))
        (sb-ext:gc)))

(format t "~&gc test done~%")

;; this used to deadlock on session-lock
(with-test (:name (:no-session-deadlock))
  (sb-thread:make-thread (lambda () (sb-ext:gc))))

(with-test (:name (:terminate-thread-restart))
  (loop repeat 100 do
        (let ((thread (sb-thread:make-thread (lambda () (sleep 0.1)))))
          (sb-thread:interrupt-thread
           thread
           (lambda ()
             (assert (find-restart 'sb-thread:terminate-thread)))))))

(sb-ext:gc :full t)

(format t "~&thread startup sigmask test done~%")

;; expose thread creation races by exiting quickly
(with-test (:name (:no-thread-creation-race :light))
  (sb-thread:make-thread (lambda ())))

(with-test (:name (:no-thread-creation-race :heavy))
  (loop repeat 200 do
        (wait-for-threads
         (loop for i below 100 collect
               (sb-thread:make-thread (lambda ()))))))

(format t "~&creation test done~%")

;;;; Binding stack safety

(defparameter *x* nil)
(defparameter *n-gcs-requested* 0)
(defparameter *n-gcs-done* 0)

(let ((counter 0))
  (defun make-something-big ()
    (let ((x (make-string 32000)))
      (incf counter)
      (let ((counter counter))
        (sb-ext:finalize x (lambda () (format t " ~S" counter)
                                   (force-output)))))))

(defmacro wait-for-gc ()
  `(progn
     (incf *n-gcs-requested*)
     (loop while (< *n-gcs-done* *n-gcs-requested*))))

(defun send-gc ()
  (loop until (< *n-gcs-done* *n-gcs-requested*))
  (format t "G")
  (force-output)
  (sb-ext:gc)
  (incf *n-gcs-done*))

(defun exercise-binding ()
  (loop
   (let ((*x* (make-something-big)))
     (let ((*x* 42))
       ;; at this point the binding stack looks like this:
       ;; NO-TLS-VALUE-MARKER, *x*, SOMETHING, *x*
       t))
   (wait-for-gc)
   ;; sig_stop_for_gc_handler binds FREE_INTERRUPT_CONTEXT_INDEX. By
   ;; now SOMETHING is gc'ed and the binding stack looks like this: 0,
   ;; 0, SOMETHING, 0 (because the symbol slots are zeroed on
   ;; unbinding but values are not).
   (let ((*x* nil))
     ;; bump bsp as if a BIND had just started
     (incf sb-vm::*binding-stack-pointer* 2)
     (wait-for-gc)
     (decf sb-vm::*binding-stack-pointer* 2))))

(with-test (:name (:binding-stack-gc-safety)
            :fails-on :win32)
  #+win32 (error "Not appliable on platform with safepoints")
  (let (threads)
    (unwind-protect
         (progn
           (push (sb-thread:make-thread #'exercise-binding) threads)
           (push (sb-thread:make-thread (lambda ()
                                          (loop
                                           (sleep 0.1)
                                           (send-gc))))
                 threads)
           (sleep 4))
      (mapc #'sb-thread:terminate-thread threads))))

(format t "~&binding test done~%")

;;; HASH TABLES

(defvar *errors* nil)

(defun oops (e)
  (setf *errors* e)
  (format t "~&oops: ~A in ~S~%" e *current-thread*)
  (sb-debug:backtrace)
  (catch 'done))

(with-test (:name (:unsynchronized-hash-table))
  ;; We expect a (probable) error here: parellel readers and writers
  ;; on a hash-table are not expected to work -- but we also don't
  ;; expect this to corrupt the image.
  #-(and) #+win32 (error "Corrupts the image?")
  (let* ((hash (make-hash-table))
         (*errors* nil)
         (threads (list (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (random 100) hash) 'h)))))
                         :name "writer")
                        (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (random 100) hash)))))
                         :name "reader")
                        (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                         :name "collector"))))
    (unwind-protect
         (sleep 10)
      (mapc #'sb-thread:terminate-thread threads))))

(format t "~&unsynchronized hash table test done~%")

(with-test (:name (:synchronized-hash-table))
  (let* ((hash (make-hash-table :synchronized t))
         (*errors* nil)
         (threads (list (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (random 100) hash) 'h)))))
                         :name "writer")
                        (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (random 100) hash)))))
                         :name "reader")
                        (sb-thread:make-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                         :name "collector"))))
    (unwind-protect
         (sleep 10)
      (mapc #'sb-thread:terminate-thread threads))
    (assert (not *errors*))))

(format t "~&synchronized hash table test done~%")

(with-test (:name (:hash-table-parallel-readers))
  (let ((hash (make-hash-table))
        (*errors* nil))
    (loop repeat 50
          do (setf (gethash (random 100) hash) 'xxx))
    (let ((threads (list (sb-thread:make-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 1")
                         (sb-thread:make-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 2")
                         (sb-thread:make-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 3")
                         (sb-thread:make-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                          :name "collector"))))
      (unwind-protect
           (sleep 10)
        (mapc #'sb-thread:terminate-thread threads))
      (assert (not *errors*)))))

(format t "~&multiple reader hash table test done~%")

(with-test (:name (:hash-table-single-accessor-parallel-gc))
  #+darwin
  (error "Prone to hang on Darwin due to interrupt issues.")
  (let ((hash (make-hash-table))
        (*errors* nil))
    (let ((threads (list (sb-thread:make-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (let ((n (random 100)))
                                  (if (gethash n hash)
                                      (remhash n hash)
                                      (setf (gethash n hash) 'h))))))
                          :name "accessor")
                         (sb-thread:make-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (sleep (random 1.0))
                                (sb-ext:gc :full t))))
                          :name "collector"))))
      (unwind-protect
           (sleep 10)
        (mapc #'sb-thread:terminate-thread threads))
      (assert (not *errors*)))))

(format t "~&single accessor hash table test~%")

#|  ;; a cll post from eric marsden
| (defun crash ()
|   (setq *debugger-hook*
|         (lambda (condition old-debugger-hook)
|           (debug:backtrace 10)
|           (unix:unix-exit 2)))
|   #+live-dangerously
|   (mp::start-sigalrm-yield)
|   (flet ((roomy () (loop (with-output-to-string (*standard-output*) (room)))))
|     (mp:make-process #'roomy)
|     (mp:make-process #'roomy)))
|#

;;; Make sure that a deadline handler is not invoked twice in a row in
;;; CONDITION-WAIT. See LP #512914 for a detailed explanation.
;;;
#-sb-lutex    ; See KLUDGE above: no deadlines for condition-wait+lutexes.
(with-test (:name (:condition-wait :deadlines :LP-512914))
  (let ((n 2) ; was empirically enough to trigger the bug
        (mutex (sb-thread:make-mutex))
        (waitq (sb-thread:make-waitqueue))
        (threads nil)
        (deadline-handler-run-twice? nil))
    (dotimes (i n)
      (let ((child
             (sb-thread:make-thread
              #'(lambda ()
                  (handler-bind
                      ((sb-sys:deadline-timeout
                        (let ((already? nil))
                          #'(lambda (c)
                              (when already?
                                (setq deadline-handler-run-twice? t))
                              (setq already? t)
                              (sleep 0.2)
                              (sb-thread:condition-broadcast waitq)
                              (sb-sys:defer-deadline 10.0 c)))))
                    (sb-sys:with-deadline (:seconds 0.1)
                      (sb-thread:with-mutex (mutex)
                        (sb-thread:condition-wait waitq mutex))))))))
        (push child threads)))
    (mapc #'sb-thread:join-thread threads)
    (assert (not deadline-handler-run-twice?))))

(with-test (:name (:condition-wait :signal-deadline-with-interrupts-enabled))
  #+darwin
  (error "Bad Darwin")
  (let ((mutex (sb-thread:make-mutex))
        (waitq (sb-thread:make-waitqueue))
        (A-holds? :unknown)
        (B-holds? :unknown)
        (A-interrupts-enabled? :unknown)
        (B-interrupts-enabled? :unknown)
        (A)
        (B))
    ;; W.L.O.G., we assume that A is executed first...
    (setq A (sb-thread:make-thread
             #'(lambda ()
                 (handler-bind
                     ((sb-sys:deadline-timeout
                       #'(lambda (c)
                           ;; We came here through the call to DECODE-TIMEOUT
                           ;; in CONDITION-WAIT; hence both here are supposed
                           ;; to evaluate to T.
                           (setq A-holds? (sb-thread:holding-mutex-p mutex))
                           (setq A-interrupts-enabled?
                                 sb-sys:*interrupts-enabled*)
                           (sleep 0.2)
                           (sb-thread:condition-broadcast waitq)
                           (sb-sys:defer-deadline 10.0 c))))
                   (sb-sys:with-deadline (:seconds 0.1)
                     (sb-thread:with-mutex (mutex)
                       (sb-thread:condition-wait waitq mutex)))))))
    (setq B (sb-thread:make-thread
             #'(lambda ()
                 (thread-yield)
                 (handler-bind
                     ((sb-sys:deadline-timeout
                       #'(lambda (c)
                           ;; We came here through the call to GET-MUTEX
                           ;; in CONDITION-WAIT (contended case of
                           ;; reaquiring the mutex) - so the former will
                           ;; be NIL, but interrupts should still be enabled.
                           (setq B-holds? (sb-thread:holding-mutex-p mutex))
                           (setq B-interrupts-enabled?
                                 sb-sys:*interrupts-enabled*)
                           (sleep 0.2)
                           (sb-thread:condition-broadcast waitq)
                           (sb-sys:defer-deadline 10.0 c))))
                   (sb-sys:with-deadline (:seconds 0.1)
                     (sb-thread:with-mutex (mutex)
                       (sb-thread:condition-wait waitq mutex)))))))
    (sb-thread:join-thread A)
    (sb-thread:join-thread B)
    (let ((A-result (list A-holds? A-interrupts-enabled?))
          (B-result (list B-holds? B-interrupts-enabled?)))
      ;; We also check some subtle behaviour w.r.t. whether a deadline
      ;; handler in CONDITION-WAIT got the mutex, or not. This is most
      ;; probably very internal behaviour (so user should not depend
      ;; on it) -- I added the testing here just to manifest current
      ;; behaviour.
      (cond ((equal A-result '(t t)) (assert (equal B-result '(nil t))))
            ((equal B-result '(t t)) (assert (equal A-result '(nil t))))
            (t (error "Failure: fall through."))))))

(with-test (:name (:mutex :finalization))
  (let ((a nil))
    (dotimes (i 500000)
      (setf a (make-mutex)))))

(format t "mutex finalization test done~%")

;;; Check that INFO is thread-safe, at least when we're just doing reads.

(let* ((symbols (loop repeat 10000 collect (gensym)))
       (functions (loop for (symbol . rest) on symbols
                        for next = (car rest)
                        for fun = (let ((next next))
                                    (lambda (n)
                                      (if next
                                          (funcall next (1- n))
                                          n)))
                        do (setf (symbol-function symbol) fun)
                        collect fun)))
  (defun infodb-test ()
    (funcall (car functions) 9999)))

(with-test (:name (:infodb :read))
  (let* ((ok t)
         (threads (loop for i from 0 to 10
                        collect (sb-thread:make-thread
                                 (lambda ()
                                   (dotimes (j 100)
                                     (write-char #\-)
                                     (finish-output)
                                     (let ((n (infodb-test)))
                                       (unless (zerop n)
                                         (setf ok nil)
                                         (format t "N != 0 (~A)~%" n)
                                         (sb-ext:quit)))))))))
    (wait-for-threads threads)
    (assert ok)))

(format t "infodb test done~%")

(with-test (:name (:backtrace))
  #+darwin
  (error "Prone to crash on Darwin, cause unknown.")
  ;; Printing backtraces from several threads at once used to hang the
  ;; whole SBCL process (discovered by accident due to a timer.impure
  ;; test misbehaving). The cause was that packages weren't even
  ;; thread-safe for only doing FIND-SYMBOL, and while printing
  ;; backtraces a loot of symbol lookups need to be done due to
  ;; *PRINT-ESCAPE*.
  (let* ((threads (loop repeat 10
                        collect (sb-thread:make-thread
                                 (lambda ()
                                   (dotimes (i 1000)
                                     (with-output-to-string (*debug-io*)
                                       (sb-debug::backtrace 10))))))))
    (wait-for-threads threads)))

(format t "backtrace test done~%")

(check-deferrables-unblocked-or-lose 0)

(format t "~&starting gc deadlock test: WARNING: THIS TEST WILL HANG ON FAILURE!~%")

(with-test (:name (:gc-deadlock))
  #+darwin
  (error "Prone to hang on Darwin due to interrupt issues.")
  ;; Prior to 0.9.16.46 thread exit potentially deadlocked the
  ;; GC due to *all-threads-lock* and session lock. On earlier
  ;; versions and at least on one specific box this test is good enough
  ;; to catch that typically well before the 1500th iteration.
  (loop
     with i = 0
     with n = 6000
     while (< i n)
     do
       (incf i)
       (when (zerop (mod i 100))
         (write-char #\.)
         (force-output))
       (handler-case
           (if (oddp i)
               (sb-thread:make-thread
                (lambda ()
                  (sleep (random 0.001)))
                :name (format nil "SLEEP-~D" i))
               (sb-thread:make-thread
                (lambda ()
                  ;; KLUDGE: what we are doing here is explicit,
                  ;; but the same can happen because of a regular
                  ;; MAKE-THREAD or LIST-ALL-THREADS, and various
                  ;; session functions.
                  (sb-thread::with-all-threads-lock
                    (sb-thread::with-session-lock (sb-thread::*session*)
                      (sb-ext:gc))))
                :name (format nil "GC-~D" i)))
         (error (e)
           (format t "~%error creating thread ~D: ~A -- backing off for retry~%" i e)
           (sleep 0.1)
           (incf i)))))

(format t "~&gc deadlock test done~%")

(let ((count (make-array 8 :initial-element 0)))
  (defun closure-one ()
    (declare (optimize safety))
    (values (incf (aref count 0)) (incf (aref count 1))
            (incf (aref count 2)) (incf (aref count 3))
            (incf (aref count 4)) (incf (aref count 5))
            (incf (aref count 6)) (incf (aref count 7))))
  (defun no-optimizing-away-closure-one ()
    (setf count (make-array 8 :initial-element 0))))

(defstruct box
  (count 0))

(let ((one (make-box))
      (two (make-box))
      (three (make-box)))
  (defun closure-two ()
    (declare (optimize safety))
    (values (incf (box-count one)) (incf (box-count two)) (incf (box-count three))))
  (defun no-optimizing-away-closure-two ()
    (setf one (make-box)
          two (make-box)
          three (make-box))))

(check-deferrables-unblocked-or-lose 0)

(with-test (:name (:funcallable-instances))
  ;; the funcallable-instance implementation used not to be threadsafe
  ;; against setting the funcallable-instance function to a closure
  ;; (because the code and lexenv were set separately).
  (let ((fun (sb-kernel:%make-funcallable-instance 0))
        (condition nil))
    (setf (sb-kernel:funcallable-instance-fun fun) #'closure-one)
    (flet ((changer ()
             (loop (setf (sb-kernel:funcallable-instance-fun fun) #'closure-one)
                   (setf (sb-kernel:funcallable-instance-fun fun) #'closure-two)))
           (test ()
             (handler-case (loop (funcall fun))
               (serious-condition (c) (setf condition c)))))
      (let ((changer (make-thread #'changer))
            (test (make-thread #'test)))
        (handler-case
            (progn
              ;; The two closures above are fairly carefully crafted
              ;; so that if given the wrong lexenv they will tend to
              ;; do some serious damage, but it is of course difficult
              ;; to predict where the various bits and pieces will be
              ;; allocated.  Five seconds failed fairly reliably on
              ;; both my x86 and x86-64 systems.  -- CSR, 2006-09-27.
              (sb-ext:with-timeout 5
                (wait-for-threads (list test)))
              (error "~@<test thread got condition:~2I~_~A~@:>" condition))
          (sb-ext:timeout ()
            (terminate-thread changer)
            (terminate-thread test)
            (wait-for-threads (list changer test))))))))

(format t "~&funcallable-instance test done~%")

(defun random-type (n)
  `(integer ,(random n) ,(+ n (random n))))

(defun subtypep-hash-cache-test ()
  (dotimes (i 10000)
    (let ((type1 (random-type 500))
          (type2 (random-type 500)))
      (let ((a (subtypep type1 type2)))
        (dotimes (i 100)
          (assert (eq (subtypep type1 type2) a))))))
  (format t "ok~%")
  (force-output))

(with-test (:name (:hash-cache :subtypep))
  (dotimes (i 10)
    (sb-thread:make-thread #'subtypep-hash-cache-test)))
(format t "hash-cache tests done~%")

;;;; BLACK BOX TESTS

(in-package :cl-user)
(use-package :test-util)
(use-package "ASSERTOID")

(format t "parallel defclass test -- WARNING, WILL HANG ON FAILURE!~%")
(with-test (:name :parallel-defclass)
  (defclass test-1 () ((a :initform :orig-a)))
  (defclass test-2 () ((b :initform :orig-b)))
  (defclass test-3 (test-1 test-2) ((c :initform :orig-c)))
  (let* ((run t)
         (d1 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-1 () ((a :initform :new-a)))
                                            (write-char #\1)
                                            (force-output)))
                                    :name "d1"))
         (d2 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-2 () ((b :initform :new-b)))
                                               (write-char #\2)
                                               (force-output)))
                                    :name "d2"))
         (d3 (sb-thread:make-thread (lambda ()
                                      (loop while run
                                            do (defclass test-3 (test-1 test-2) ((c :initform :new-c)))
                                               (write-char #\3)
                                               (force-output)))
                                    :name "d3"))
         (i (sb-thread:make-thread (lambda ()
                                     (loop while run
                                           do (let ((i (make-instance 'test-3)))
                                                (assert (member (slot-value i 'a) '(:orig-a :new-a)))
                                                (assert (member (slot-value i 'b) '(:orig-b :new-b)))
                                                (assert (member (slot-value i 'c) '(:orig-c :new-c))))
                                              (write-char #\i)
                                              (force-output)))
                                   :name "i")))
    (format t "~%sleeping!~%")
    (sleep 2.0)
    (format t "~%stopping!~%")
    (setf run nil)
    (mapc (lambda (th)
            (sb-thread:join-thread th)
            (format t "~%joined ~S~%" (sb-thread:thread-name th)))
          (list d1 d2 d3 i))))
(format t "parallel defclass test done~%")
