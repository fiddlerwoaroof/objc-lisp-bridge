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
               #:cffi)
  :defsystem-depends-on (#:cffi-grovel)
  :components ((:file "package")
               (:cffi-grovel-file "objc-runtime-types" :depends-on ("package"))
               (:file "objc-runtime" :depends-on ("package" "objc-runtime-types"))))
