(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-pathname-defaults* (truename "~/git_repos/objc-lisp-bridge/"))
  (load (compile-file "objc-runtime.asd")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:objc-runtime :yason :plump :cl-ppcre :data-lens)))

#+reading-list
(progn
  (load "reading-list-reader.lisp")

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-ext:save-lisp-and-die "reading-list2org"
                              :toplevel (intern "MAIN"
                                                "READING-LIST-READER")
                              :executable t)))

#+safari2org
(progn
  (load "scripting-bridge.lisp")
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-ext:save-lisp-and-die "safari2org"
                              :toplevel (intern "SAFARI-MAIN"
                                                "OBJC.SCRIPTING-BRIDGE")
                              :executable t)))
#+safari2org-beta
(progn
  (load "scripting-bridge.lisp")
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-ext:save-lisp-and-die "safari2org-beta"
                              :toplevel (intern "SAFARI-2-MAIN"
                                                "OBJC.SCRIPTING-BRIDGE")
                              :executable t)))
