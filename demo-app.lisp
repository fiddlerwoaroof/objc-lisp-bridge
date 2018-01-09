(defpackage :demo-app
  (:use :cl :objc-runtime)
  (:export
   #:get-method-names))
(in-package :demo-app)
(named-readtables:in-readtable :objc-readtable)

(cffi:defcallback exception-handler :void ((exception :pointer))
  (with-selectors (reason)
    (format t "~&Exxception: ~a~%" [exception reason])
    (values)))

(cffi:defcfun (init-window "initWindow")
    :pointer
  (window :pointer)
  (rect :pointer)
  (a :char)
  (b :char)
  (c :boolean))

(cffi:defcfun (init-with-frame "initWithFrame")
    :pointer
  (thing :pointer)
  (rect :pointer))

(cffi:defcfun (print-rect "printRect")
    :void
  (rect (:struct objc-runtime::ns-rect)))

(cffi:defcfun (set-uncaught-exception-handler "set_uncaught_exception_handler"
                                              :library objc-runtime::expose-stuff)
    :void
  (cb :pointer))

(defun value-for-key (thing key)
  (with-selectors ((vfk "valueForKey:"))
    (let ((key (objc-runtime::make-nsstring key)))
      [thing vfk :string key])))

(defun call-with-rect (x y w h cb)
  (check-type x real)
  (check-type y real)
  (check-type w real)
  (check-type h real)
  (cffi:with-foreign-object (rect '(:struct objc-runtime::ns-rect))
    (cffi:with-foreign-slots (((:pointer ns-rect-origin) (:pointer ns-rect-size))
                              rect (:struct objc-runtime::ns-rect))
      (cffi:with-foreign-slots ((ns-point-x ns-point-y) ns-rect-origin (:struct ns-point))
        (setf ns-point-x (coerce x 'double-float)
              ns-point-y (coerce y 'double-float)))
      (cffi:with-foreign-slots ((ns-size-width ns-size-height)
                                ns-rect-size (:struct ns-size))
        (setf ns-size-width (coerce w 'double-float)
              ns-size-height (coerce h 'double-float))))
    (funcall cb rect)))

(defun call-with-point (x y cb)
  (check-type x real)
  (check-type y real)
  (cffi:with-foreign-object (point '(:struct ns-point))
    (cffi:with-foreign-slots ((ns-point-x ns-point-y) point (:struct ns-point))
      (setf ns-point-x (coerce x 'double-float)
            ns-point-y (coerce y 'double-float)))
    (funcall cb point)))

(defmacro with-rect ((rect (x y) (w h)) &body body)
  `(call-with-rect ,x ,y ,w ,h
                   (lambda (,rect)
                     ,@body)))

(defmacro with-point ((point (x y)) &body body)
  `(call-with-point ,x ,y
                   (lambda (,point)
                     ,@body)))

(defun make-rect (x y w h)
  (check-type x real)
  (check-type y real)
  (check-type w real)
  (check-type h real)
  (cffi:convert-to-foreign `(ns-rect-origin
                             (objc-runtime:ns-point-x
                              ,(coerce x 'double-float)
                              objc-runtime:ns-point-y
                              ,(coerce y 'double-float))
                             ns-rect-size
                             (objc-runtime:ns-size-width
                              ,(coerce w 'double-float)
                              objc-runtime:ns-size-height
                              ,(coerce h 'double-float)))
                           '(:struct objc-runtime:ns-rect)))

(defun show-alert (message)
  (let ((alert [[#@NSAlert @(alloc)] @(init)]))
    [alert @(setMessageText:) :pointer @"message"]
    [alert @(setInformativeText:) :pointer @"Informative text."]
    [alert @(addButtonWithTitle:) :pointer @"OK"]
    [alert @(addButtonWithTitle:) :pointer @"Cancel"]
    [alert @(runModal)]))

(cffi:defcallback button-action :void ((a :pointer) (b :pointer) (sender :pointer))
  (declare (ignore a b sender))
  (show-alert "Hello There!"))

(defun make-button-delegate (button)
  (let ((my-class (objc-runtime::objc-allocate-class-pair #@NSObject "ButtonDel" 0)))
    (with-selectors ((do-magic "doMagic:") (set-target "setTarget:") (set-action "setAction:")
                     alloc init)
      (objc-runtime::class-add-method my-class do-magic (cffi:callback button-action)
                                      "v@:@")
      (fw.lu:prog1-bind (result [[my-class alloc] init])
        [button set-target :pointer result]
        [button set-action :pointer do-magic]))))

(defun main ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    [#@NSAutoReleasePool @(new) ]
    [#@NSApplication @(sharedApplication) ]
    [objc-runtime::ns-app @(setActivationPolicy:) :int 0]

    (let ((app-delegate (objc-runtime::objc-allocate-class-pair
                         #@NSObject "AppDelegate" 0)))
      (objc-runtime::add-pointer-ivar app-delegate "window")
      (objc-runtime::add-pointer-ivar app-delegate "delegate"))

    (let* ((bundle [#@NSBundle @(mainBundle)])
           (nib [[#@NSNib @(alloc)] @(initWithNibNamed:bundle:)
                                    :pointer @"MainMenu"
                                    :pointer bundle]))
      (cffi:with-foreign-object (p :pointer)
        [nib @(instantiateWithOwner:topLevelObjects:)
             :pointer objc-runtime::ns-app
             :pointer p]))
    [objc-runtime::ns-app @(activateIgnoringOtherApps:) :boolean t]
    [objc-runtime::ns-app @(run) ]))
