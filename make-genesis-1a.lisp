(setf *print-level* 5 *print-length* 5)
(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
(defparameter *target-object-file-names*
  (with-open-file (s "output/object-filenames-for-genesis.lisp-expr"
                     :direction :input)
    (read s)))
(host-load-stem "src/compiler/generic/genesis" nil)
(sb!vm:genesis :object-file-names *target-object-file-names*
               :c-header-dir-name "output/genesis-1a"
               :symbol-table-file-name "src/runtime/sbcl-own.nm")

               ;; The map file is not needed by the system, but can be
               ;; very handy when debugging cold init problems.

(defparameter *mangled-names* (make-hash-table :test 'equal))

#!+win32
(let ((sb!fasl::*cold-foreign-symbol-table* (make-hash-table :test 'equal)))
  (sb!fasl::load-cold-foreign-symbol-table "src/runtime/mangled-stuff.nm")
  (loop for name being each hash-key of sb!fasl::*cold-foreign-symbol-table*
        for at-pos = (position #\@ name :from-end t)
        when at-pos
          do (let ((demangled-name (subseq name 0 at-pos)))
               (setf (gethash (subseq demangled-name 1) *mangled-names*)
                     (subseq name 1)))))

(with-open-file (undefs "src/runtime/gen1a-undefs" :direction :output
                        :if-does-not-exist :create :if-exists :supersede)
  (let ((n 0))
    (dolist (symbol sb!fasl::*cold-foreign-undefined-symbols*)
      (format undefs "FOREIGN_REFERENCE(~A)~%"
              (gethash symbol *mangled-names* symbol)))))

#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
