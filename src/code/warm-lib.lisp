;;; We're in transition to fully automated tracking of foreign symbols
;;; needed by Lisp code (see make-genesis-1a). Symbol references in
;;; the "warm" code can't be tracked this way, if we aren't going to
;;; rebuild runtime once more after the cold-warm transition (and now
;;; we don't).
;;;
;;; Fortunately, dynamic linking is already working at this stage. On
;;; win32, however, dynamic foreign symbols for link-time dependencies
;;; are not available before explicit LoadLibrary on them. As far as I
;;; know, dlopen() on Unix resolves such symbols with
;;; *runtime-dlhandle*, so it's not a problem to use them in warm-only
;;; code.

(in-package "SB-IMPL")
#+win32
(progn
  (load-shared-object "kernel32.dll")
  (load-shared-object "msvcrt.dll")
  (load-shared-object "advapi32.dll")
  (load-shared-object "ws2_32.dll")
  (load-shared-object "shell32.dll"))
