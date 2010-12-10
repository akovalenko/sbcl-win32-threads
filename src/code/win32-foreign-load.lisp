;;;; Loading shared object files, Win32 specifics

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(define-alien-type hinstance long)

(define-alien-routine ("LoadLibraryA" loadlibrary) hinstance
  (file c-string))

(define-alien-routine ("FreeLibrary" freelibrary) int
  (handle hinstance))

(define-alien-routine ("GetProcAddress" getprocaddress) system-area-pointer
  (handle hinstance)
  (symbol c-string))

(define-alien-routine ("GetLastError" getlasterror) unsigned-int)

(define-alien-routine ("SetStdHandle" set-std-handle)
   void
 (id int)
 (handle int))

(sb!alien:define-alien-routine ("GetStdHandle" get-std-handle)
   sb!alien:int
 (id sb!alien:int))

(defvar *reset-stdio-on-dlopen* t)

(defconstant +stdio-handle+ -10)

(defun loadlibrary-withouth-stdio (namestring)
  (if *reset-stdio-on-dlopen*
      (let ((stdio (get-std-handle +stdio-handle+)))
        (unwind-protect
          (progn
            (set-std-handle +stdio-handle+ -1)
            (loadlibrary namestring))
          (set-std-handle +stdio-handle+ stdio)))
      (loadlibrary namestring)))

(defun dlopen-or-lose (obj)
  (let* ((namestring (shared-object-namestring obj))
         (handle (loadlibrary-withouth-stdio namestring)))
    (aver namestring)
    (when (zerop handle)
      (setf (shared-object-handle obj) nil)
      (error "Error opening shared object ~S:~%  ~A."
             namestring (getlasterror)))
    (setf (shared-object-handle obj) handle)
    handle))

(defun dlclose-or-lose (&optional (obj nil objp))
  (when (and objp (shared-object-handle obj))
    (unless (freelibrary (shared-object-handle obj))
      (cerror "Ignore the error and continue as if closing succeeded."
              "FreeLibrary() caused an error while trying to close ~
               shared object ~S: ~S"
              (shared-object-namestring obj)
              (getlasterror)))
    (setf (shared-object-handle obj) nil)))

(defun find-dynamic-foreign-symbol-address (symbol)
  ;; On real ELF & dlsym platforms the EXTERN-ALIEN-NAME is a no-op,
  ;; but on platforms where dlsym is simulated we use the mangled name.
  ;; Win32 is a special case. It needs EXTERN-ALIEN-NAME to mangle the
  ;; name for static linkage, but also needs unmangled symbols for
  ;; GetProcAddress(). So we coerce to base-string instead.
  ;; Oh, and we assume that all runtime symbols are static-linked.
  ;; No *runtime-dlhandle* for us.
  ;; Also, GetProcAddress doesn't call SetLastError(0) on success,
  ;; and GetLastError() doesn't either. For now, we assume that
  ;; GetProcAddress() won't return NULL on success.
  (let* ((extern (coerce symbol 'base-string))
         (result nil))
    (dolist (obj *shared-objects*)
      (let ((handle (shared-object-handle obj)))
        (when handle
          (setf result (sap-int (getprocaddress handle extern)))
          (when (not (zerop result))
            (return result)))))))


