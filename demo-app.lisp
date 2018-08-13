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
  (let ((my-class (objc-runtime::objc-allocate-class-pair #@NSObject "ButtonDel" 0)))
    (objc-runtime::class-add-method my-class @(doMagic) cb "v@:@")
    (fw.lu:prog1-bind (result (alloc-init my-class))
      [button @(setTarget) :pointer result]
      [button @(setAction) :pointer @(doMagic)])))

(defun make-app-delegate-class (outlets)
  (let ((app-delegate-class (objc-runtime::objc-allocate-class-pair
                             #@NSObject "AppDelegate" 0)))
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

  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    [#@NSAutoReleasePool @(new)]
    [#@NSApplication @(sharedApplication)]
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

(defparameter *application-shim* (make-instance 'application-shim))

#+nil
(defun old-code ()
 (trivial-main-thread:with-body-in-main-thread (:blocking t)
   (sb-int:with-float-traps-masked
       (:underflow :overflow :inexact
                   :invalid :divide-by-zero)
     (with-selectors ((shared-application "sharedApplication")
                      (process-info "processInfo")
                      (process-name "processName")
                      (set-activation-policy "setActivationPolicy:")
                      ;; (init-with-content-rect "initWithContentRect:styleMask:backing:defer:")
                      (set-title "setTitle:")
                      (run "run")
                      (activate-ignoring-other-apps "activateIgnoringOtherApps:")
                      (make-key-and-order-front "makeKeyAndOrderFront:")
                      (cascade-top-left-from-point "cascadeTopLeftFromPoint:")
                      (add-item "addItem:")
                      (set-main-menu "setMainMenu:")
                      (init-with-title "initWithTitle:action:keyEquivalent:")
                      (set-submenu "setSubmenu:")
                      (init-with-encoding "initWithCString:length:")
                      (content-view "contentView")
                      (add-subview "addSubview:")
                      (set-target "setTarget:")
                      (set-action "setAction:")
                      terminate?
                      ;; (application-should-terminate "applicationShouldTerminate:")
                      ;; (set-delegate "setDelegate:")
                      ;; (finish-launching "finishLaunching")
                      alloc new autorelease
                      )
       [#@NSAutoReleasePool new]
       [#@NSApplication shared-application]
       [objc-runtime::ns-app set-activation-policy :int 0]



       ;; (break)
       (let* ((application-name [[#@NSProcessInfo process-info] process-name]))
         (let* ((menubar [[#@NSMenu new] autorelease])
                (app-menu-item [[#@NSMenuItem new] autorelease])
                (app-menu [[#@NSMenu new] autorelease])
                (quit-name [[#@NSString alloc] init-with-encoding :string "Quit" :uint 4])
                (key [[#@NSString alloc] init-with-encoding :string "q" :uint 1])
                (quit-menu-item
                 [[[#@NSMenuItem alloc] init-with-title :pointer quit-name :pointer terminate? :string key] autorelease]))
           [menubar add-item :pointer app-menu-item]
           [app-menu add-item :pointer quit-menu-item]
           [app-menu-item set-submenu :pointer app-menu]
           [objc-runtime::ns-app set-main-menu :pointer menubar] )

         (setf (main-view *application-shim*)
               [#@NSStackView @(stackViewWithViews:) :pointer [[#@NSArray @(alloc)] @(init)]])
         (with-point (p (20 20))
           (let* ((foreign-rect (make-rect 10 10 120 120))
                  (the-window (init-window [#@NSWindow alloc] foreign-rect 1 2 nil)))
             
             [(value-for-key the-window "contentView") add-subview :pointer (main-view *application-shim*)]
             [the-window cascade-top-left-from-point :pointer p]
             [the-window set-title :pointer application-name]
             [the-window make-key-and-order-front :pointer (cffi:null-pointer)]
             [ objc-runtime::ns-app activate-ignoring-other-apps :boolean t]
             [ objc-runtime::ns-app run])))))))
