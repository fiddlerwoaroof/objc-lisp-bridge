(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-pathname-defaults* (truename "~/git_repos/objc-lisp-bridge/"))
  (load (compile-file "objc-runtime.asd")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:objc-runtime :yason :plump :cl-ppcre)))

(load "reading-list-reader.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:save-lisp-and-die "reading-list2org"
                            :toplevel (intern "MAIN"
                                              "READING-LIST-READER")
                            :executable t))
