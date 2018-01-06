(defpackage :demo-app
  (:use :cl :objc-runtime)
  (:export ))
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

(cffi:defcfun (print-rect "printRect")
    :void
  (rect (:struct objc-runtime::ns-rect)))

#+null
(cffi:defcfun (set-uncaught-exception-handler "set_uncaught_exception_handler"
                                              :library objc-runtime::expose-stuff)
    :void
  (cb :pointer))

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

(defun main ()
  (break)
  (trivial-main-thread:with-body-in-main-thread ()
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

        (with-point (p (20 20))
          (let* ((the-window [#@NSWindow alloc])
                 (foreign-rect (make-rect 10 10 120 120)))
            (format t "~&My rect: ~s~%"
                    (cffi:convert-from-foreign foreign-rect
                                               '(:struct objc-runtime::ns-rect)))
            (init-window the-window foreign-rect 1 2 nil)
            [the-window cascade-top-left-from-point :pointer p]
            [the-window set-title :pointer application-name]
            [the-window make-key-and-order-front :pointer (cffi:null-pointer)]
            [ objc-runtime::ns-app activate-ignoring-other-apps :boolean t]
            [ objc-runtime::ns-app run]))))))
