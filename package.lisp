(defpackage :objc-runtime/package
  (:use :cl )
  (:export ))
(in-package :objc-runtime/package)

(defpackage :objc-runtime
  (:use :cl :cffi)
  (:export
   #:defmacro
   #:*objc-readtable*
   #:with-selectors
   #:objc-msg-send
   #:with-objc-classes
   #:make-objc-instance
   #:objc-readtable
   #:ns-size-width
   #:ns-size-height
   #:ns-rect-origin
   #:ns-rect-size
   #:ns-point-x
   #:ns-point-y
   #:ns-point
   #:ns-size
   #:ns-rect
   #:get-method-names
   #:ensure-wrapped-objc-class
   #:add-pointer-ivar
   #:objc-msg-send-int
   #:objc-msg-send-string
   #:make-nsstring
   #:ensure-class
   #:ensure-selector))

(defpackage :objc-runtime.data-extractors
  (:use :cl )
  (:export
   #:extract-from-objc
   #:define-extractor
   #:clear-extractors
   #:add-extractor
   #:get-plist))

(uiop:define-package :objc.manipulators
    (:use :cl :data-lens)
  (:export :sel :ext :<> :add-index :<count)
  (:reexport :data-lens))

(defpackage :objc-runtime.bundle-utils
  (:use :cl)
  (:export #:bundle-resource-root
           #:application-support-directory
           #:setup-bundle-logical-pathnames
           #:ensure-application-support
           #:print-for-config
           #:read-from-config))
