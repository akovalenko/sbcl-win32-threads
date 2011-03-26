(load "run-tests.lisp")
;(trace sb-kernel::handle-win32-exception)
;(run-tests::pure-runner '("gc.impure.lisp") 'run-tests::load-test)
;(run-tests::pure-runner '("stress-gc") 'run-tests::load-test)

;(run-tests::pure-runner '("threads.impure.lisp") 'run-tests::load-test)
(run-tests::pure-runner '("threads.pure.lisp") 'run-tests::load-test)
(run-tests::pure-runner '("gcrace.impure.lisp") 'run-tests::load-test)
(quit)

