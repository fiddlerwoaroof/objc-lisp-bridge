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

(defmacro selector-lambda (selector &rest args)
  `(lambda (receiver)
     [receiver ,selector ,@args]))

(cffi:defcfun (init-with-frame "initWithFrame")
    :pointer
  (thing :pointer)
  (rect :pointer))

(cffi:defcfun (print-rect "printRect")
    :void
  (rect (:struct objc-runtime:ns-rect)))

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

(defun show-alert (message &optional (informative-text "Informative Text!"))
  (let ((alert [[#@NSAlert @(alloc)] @(init)]))
    [alert @(setMessageText:) :pointer (objc-runtime::make-nsstring message)]
    [alert @(setInformativeText:) :pointer (objc-runtime::make-nsstring informative-text)]
    [alert @(addButtonWithTitle:) :pointer @"OK"]
    [alert @(addButtonWithTitle:) :pointer @"Cancel"]
    [alert @(runModal)]))

(cffi:defcallback do-things-action :void ((a :pointer) (b :pointer) (sender :pointer))
  (declare (ignore a b sender))
  (show-alert "Starting Swank"
              "Loading Quicklisp from ~/quicklisp/setup.lisp + starting swank")

  (load "~/quicklisp/setup.lisp")
  (funcall (intern "QUICKLOAD" (find-package :QL)) :swank)
  (funcall (intern "CREATE-SERVER" (find-package :swank)) :port 5060 :dont-close t)
  
  (show-alert "Started swank on 5060"))

(cffi:defcallback alert-action :void ((a :pointer) (b :pointer) (sender :pointer))
  (declare (ignore a b sender))
  (show-alert "Hello There!"))

(cffi:defcallback profit-action :void ((a :pointer) (b :pointer) (sender :pointer))
  (declare (ignore a b sender))
  (show-alert "That Was Profitable!"))

(defun alloc-init (cls)
  [[cls @(alloc)] @(init)])

(defun make-button-delegate (button cb)
  (objc-runtime.data-extractors:objc-typecase button
    (#@NSButton (let ((my-class (objc-runtime::objc-allocate-class-pair #@NSObject "ButtonDel" 0)))
                  (objc-runtime::class-add-method my-class @(doMagic) cb "v@:@")
                  (fw.lu:prog1-bind (result (alloc-init my-class))
                    [button @(setTarget:) :pointer result]
                    [button @(setAction:) :pointer @(doMagic)])))
    (t (format t "~&The button is not a button~%"))))

(defun make-app-delegate-class (outlets)
  (let ((app-delegate-class (objc-runtime::objc-allocate-class-pair
                             #@NSObject "AppDelegate" 0)))
    (objc-runtime:add-pointer-ivar app-delegate-class "window")
    (objc-runtime:add-pointer-ivar app-delegate-class "delegate")

    (loop for outlet in outlets do
         (objc-runtime:add-pointer-ivar app-delegate-class outlet))

    app-delegate-class))

(defun make-app-delegate-class-with-props (foo outlets)
  (let ((app-delegate-class (objc-runtime::objc-allocate-class-pair
                             #@NSObject foo 0)))
    (objc-runtime:add-pointer-ivar app-delegate-class "window")
    (objc-runtime:add-pointer-ivar app-delegate-class "delegate")

    (loop for outlet in outlets do
         (objc-runtime:add-pointer-ivar app-delegate-class outlet))

    app-delegate-class))


(defun load-nib (name)
  ;; find and activate the nib
  (let* ((bundle [#@NSBundle @(mainBundle)])
         (nib [[#@NSNib @(alloc)] @(initWithNibNamed:bundle:)
               :pointer (objc-runtime::make-nsstring name)
               :pointer bundle]))
    (cffi:with-foreign-object (p :pointer)
      ;; TODO: is dropping p a problem here? The docs say something relevant.
      ;;       must investigate.
      [nib @(instantiateWithOwner:topLevelObjects:)
           :pointer objc-runtime::ns-app
           :pointer p])))

;#+null
(defun main ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())

  (load "~/quicklisp/setup.lisp")
  (funcall (intern "QUICKLOAD"
                   (find-package :QL))
           :swank)

  #+nil
  (funcall (intern "CREATE-SERVER"
                   (find-package :swank))
           :port 5060
           :dont-close t)

  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    [#@NSAutoreleasePool @(new)]
    [#@NSApplication @(sharedApplication)]
    #+nil
    [objc-runtime::ns-app @(setActivationPolicy:) :int 0]

    ;; Setup the app delegate class. We register this one because it's useful
    ;; When debugging via something like lldb
    (objc-runtime::objc-register-class-pair
     (make-app-delegate-class '("actionButton"
                                "alertButton"
                                "profitButton")))

    (load-nib "MainMenu.nib")
    
    (let ((app-delegate [objc-runtime::ns-app @(delegate)]))
      (make-button-delegate (value-for-key app-delegate "actionButton")
                            (cffi:callback do-things-action))
      (make-button-delegate (value-for-key app-delegate "alertButton")
                            (cffi:callback alert-action))
      (make-button-delegate (value-for-key app-delegate "profitButton")
                            (cffi:callback profit-action)))
    
    [objc-runtime::ns-app @(activateIgnoringOtherApps:) :boolean t]
    [objc-runtime::ns-app @(run)]))

(defclass application-shim ()
  ((%main-view :initarg :main-view :accessor main-view)))

(cffi:defcfun (%set-string-value "objc_msgSend")
    :void
  (cls objc-runtime::o-class)
  (sel objc-runtime::o-selector)
  (value :pointer))
(defun set-string-value (control string)
  (prog1 control
    (%set-string-value control @(setStringValue:)
                       (objc-runtime:make-nsstring string))))

(defun label (text)
  (let ((view [[#@NSTextField @(alloc)] @(init)]))
    (prog1 view
      (set-string-value view text))))

(defun button (title)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    [#@NSButton @(buttonWithTitle:target:action:)
                :pointer (objc-runtime:make-nsstring title)
                :pointer #@NSButton
                :pointer @(alloc)]))

(defun init-in-main-thread (instance)
  (prog1 instance
    [instance @(performSelectorOnMainThread:withObject:waitUntilDone:)
              :pointer @(init)
              :pointer (cffi:null-pointer)
              :bool t]))

(defvar *application-shim*
  (make-instance 'application-shim))
(defun wait-for-events ()
  (let ((event [objc-runtime::ns-app @(nextEventMatchingMask:untilDate:inMode:dequeue:)
                                     :unsigned-long 18446744073709551615
                                     :pointer [#@NSDate @(distantFuture)]
                                     :pointer @"kCFRunLoopDefaultMode"
                                     :int 1]))
    [objc-runtime::ns-app @(sendEvent:) :pointer event]
    event))

(defun tick ()
  (wait-for-events))

(defun task-thread ()
  (bt:make-thread (lambda ()
                    (trivial-main-thread:with-body-in-main-thread (:blocking t)
                      [#@NSEvent @(startPeriodicEventsAfterDelay:withPeriod:) :double 0.0d0 :double 0.01d0])
                    (loop
                      (trivial-main-thread:with-body-in-main-thread (:blocking t)
                        (tick))))
                  :name "Cocoa Event Loop Feeder"))

;;#+nil
(defun old-main ()
  (load "~/quicklisp/setup.lisp")
  (funcall (intern "QUICKLOAD"
                   (find-package :QL))
           :swank)
  #+nil
  (funcall (intern "CREATE-SERVER"
                   (find-package :swank))
           :port 5060
           :dont-close t)

  (trivial-main-thread:with-body-in-main-thread (:blocking nil)
    #+sbcl
    (sb-int:set-floating-point-modes :traps '())

    [#@NSAutoreleasePool @(new)]
    [#@NSApplication @(sharedApplication)]

    (format t "~&app: ~s~%" objc-runtime::ns-app)
    #+nil
    [objc-runtime::ns-app @(setActivationPolicy) :int 0]

    (let* ((application-name [[#@NSProcessInfo @(processInfo)] @(processName)]))
      (let* ((menubar [[#@NSMenu @(new)] @(autorelease)])
             (app-menu-item [[#@NSMenuItem @(new)] @(autorelease)])
             (app-menu [[#@NSMenu @(new)] @(autorelease)])
             (quit-name @"Quit")
             (key @"q")
             (quit-menu-item
               [[[#@NSMenuItem @(alloc)]
                 @(initWithTitle:action:keyEquivalent:) :pointer quit-name :pointer @(terminate?) :string key]
                @(autorelease)]))
        [menubar @(addItem:) :pointer app-menu-item]
        [app-menu @(addItem:) :pointer quit-menu-item]
        [app-menu-item @(setSubmenu:) :pointer app-menu]
        [objc-runtime::ns-app @(setMainMenu:) :pointer menubar] )

      (setf (main-view *application-shim*)
            [#@NSStackView @(stackViewWithViews:)
                           :pointer [[#@NSArray @(alloc)] @(init)]])
      (with-point (p (20 20))
        (let* ((foreign-rect (make-rect 10 10 120 120))
               (the-window (init-window [#@NSWindow @(alloc)] foreign-rect 15 2 nil)))
          
          [(value-for-key the-window "contentView") @(addSubview:) :pointer (main-view *application-shim*)]
          [the-window @(cascadeTopLeftFromPoint:) :pointer p]
          [the-window @(setTitle:) :pointer application-name]
          [the-window @(makeKeyAndOrderFront:) :pointer (cffi:null-pointer)]
          [ objc-runtime::ns-app @(activateIgnoringOtherApps:) :boolean t]
          (task-thread))))))
