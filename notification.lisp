(defpackage :objc.notification
  (:use :cl )
  (:export
   #:main-loop-ticker
   #:dnc
   #:observe-notifications
   #:*mailbox*))
(in-package :objc.notification)
(serapeum:eval-always
  (named-readtables:in-readtable :objc-readtable))

(defun main-loop-ticker ()
  (let ((main-run-loop [#@NSRunLoop @(mainRunLoop)]))
    (loop do
      (sleep 0.1)
      (trivial-main-thread:with-body-in-main-thread (:blocking t)
        (objc-runtime::tick-ns-runloop main-run-loop
                                       0.1)))))

(defun dnc ()
  [#@NSDistributedNotificationCenter
   @(defaultCenter)])

(defvar *mailbox*)
(cffi:defcallback handle-notification :void
    ((_ :pointer) (__ :pointer) (notification :pointer))
  (declare (ignore _ __))
  (sb-concurrency:send-message
   *mailbox*
   (objc-runtime.data-extractors:extract-from-objc
    [notification @(userInfo)])))

(defvar *notification-handler*)
(defun setup-delegate ()
  (if (boundp '*notification-handler*)
      *notification-handler*
      (let ((delegate-class
              (objc-runtime::objc-allocate-class-pair
               #@NSObject
               (format nil "FWNotificationHandler~a"
                       (gensym))
               0)))
        (objc-runtime::class-add-method
         delegate-class
         @(handle-notification:)
         (cffi:callback handle-notification)
         "v@:@")
        (setf *mailbox*
              (sb-concurrency:make-mailbox)

              *notification-handler*
              [[delegate-class @(alloc)] @(init)]))))

(define-condition notifications-not-initialized (error)
  ())

(defun observe-notifications (dnc notification-name)
  (tagbody start
     (restart-case
         (if (boundp '*notification-handler*)
             [dnc @(addObserver:selector:name:object:)
                  :pointer *notification-handler*
                  :pointer @(handle-notification:)
                  :pointer (objc-runtime::make-nsstring
                            notification-name)
                  :pointer (cffi:null-pointer)]
             (error 'notifications-not-initialized))
       (setup-and-retry ()
         (setup-delegate)
         (go start)))))
