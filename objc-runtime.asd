;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :objc-runtime 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:fwoar.lisputils
               #:cffi
               #:trivial-main-thread
               #:trivial-features
               #:cffi-libffi)
  :defsystem-depends-on (#:cffi-grovel)
  :components ((:file "package")
               (:cffi-grovel-file "objc-runtime-types" :depends-on ("package"))
               (:file "readtable" :depends-on ("package"))
               (:file "gcd" :depends-on ("objc-runtime"))
               (:file "objc-runtime" :depends-on ("package" "readtable" "objc-runtime-types"))))
