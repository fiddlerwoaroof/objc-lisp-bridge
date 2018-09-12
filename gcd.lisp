(defpackage :objc-runtime.gcd
  (:use :cl :cffi)
  (:export ))
(in-package :objc-runtime.gcd)

(serapeum:eval-always
  (pushnew #p"/usr/lib/system/"
           cffi:*foreign-library-directories*
           :test 'equal))

(serapeum:eval-always
  (define-foreign-library dispatch
    (:darwin "libdispatch.dylib")))

(defcfun
    (get-global-queue "dispatch_get_global_queue" :library dispatch)
    :pointer
  (id :long)
  (flags :unsigned-long))

(defun get-main-queue ()
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(defcfun (dispatch-async "dispatch_async_f" :library dispatch)
    :pointer
  (queue :pointer)
  (context :pointer)
  (block :pointer))

(defmacro def-gcd-callback (name (context-sym) &body body)
  `(progn
     (defcallback ,name :void ((,context-sym :pointer))
       (declare (ignorable ,context-sym))
       ,@body)
     (define-symbol-macro ,name (callback ,name))))
