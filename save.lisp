(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "objc-runtime.asd")))
(eval-when (:compile-toplevel :load-toplevel :execute) (ql:quickload :objc-runtime))
(eval-when (:compile-toplevel :load-toplevel :execute) (load (compile-file "demo-app.lisp")))

#+sbcl
(sb-ext:save-lisp-and-die "demo-app" :toplevel 'demo-app::main :executable t)
#+ccl
(ccl:save-application "demo-app" :toplevel-function 'demo-app::main :prepend-kernel t)
