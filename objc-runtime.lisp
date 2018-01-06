(in-package :objc-runtime)
(serapeum:eval-always
 (named-readtables:in-readtable :objc-readtable))

(serapeum:eval-always
  (cffi:define-foreign-library cocoa
    (:darwin (:framework "Cocoa")))
  (define-foreign-library foundation
    (:darwin (:framework "Foundation")))
  (define-foreign-library appkit
    (:darwin (:framework "AppKit")))
  (define-foreign-library expose-stuff
    (:darwin #p"./libnsrect-expose.dylib")))


(use-foreign-library foundation)
(use-foreign-library cocoa)
(use-foreign-library appkit)
(use-foreign-library expose-stuff)

(defctype o-class :pointer)
(defctype o-selector :pointer)

(defcfun (objc-look-up-class "objc_lookUpClass" :library foundation)
    o-class
  (name :string))

(defcfun (objc-allocate-class-pair "objc_allocateClassPair" :library foundation)
    :pointer
  (superclass :pointer)
  (name :string)
  (extra-bytes :int))

(defcfun (objc-get-protocol "objc_getProtocol" :library foundation)
    :pointer
  (name :string))

(defcfun (class-add-protocol "class_addProtocol" :library foundation)
    :boolean
  (class :pointer)
  (protocol :pointer))

(defcfun (class-add-method "class_addMethod" :library foundation)
    :boolean
  (class :pointer)
  (selector :pointer)
  (cb :pointer)
  (type :string))

(defcfun (objc-class-get-name "class_getName" :library foundation)
    :string
  (cls o-class))

(defcfun (objc-class-get-superclass "class_getSuperclass" :library foundation)
    :pointer
  (cls o-class))

(defcfun (objc-get-class-list "objc_getClassList" :library foundation)
    :int
  (cls-buffer o-class)
  (buffer-count :int))

(defcfun (sel-register-name "sel_registerName" :library foundation)
    o-selector
  (name :string))

(defcfun (objc-msg-send "objc_msgSend")
    :pointer
  (cls o-class)
  (sel o-selector)
  &rest)

(defcfun (class-copy-method-list "class_copyMethodList" :library foundation)
    :pointer
  (cls o-class)
  (numMethods (:pointer :int)))

(defcfun (method-get-name "method_getName")
    :pointer
  (method :pointer))

(defcfun (sel-get-name "sel_getName")
    :string
  (sel o-selector))

(defcfun (class-get-instance-variable "class_getInstanceVariable" :library foundation)
    :pointer
  (cls o-class)
  (name :string))

(defcfun (class-get-instance-variable "class_addMethod" :library foundation)
    :pointer
  (cls o-class)
  (sel :pointer)
  (imp :pointer)
  (type :string))

(defcfun (object-get-class "object_getClass" :library foundation)
    :pointer
  (object :pointer))

(defcfun (object-get-ivar "object_getIvar" :library foundation)
    :pointer
  (object :pointer)
  (ivar :pointer))

(defcfun (class-get-property "class_getProperty" :library foundation)
    :pointer
  (cls o-class)
  (name :string))

(defcfun (property-copy-attribute-value "property_copyAttributeValue" :library foundation)
    :string
  (prop :pointer)
  (name :string))


(defcfun (property-get-attributes "property_getAttributes" :library foundation)
    :string
  (prop :pointer))

(defgeneric get-methods (class)
  (:method ((class string))
    (get-methods (objc-look-up-class class)))

  #+ccl
  (:method ((class ccl:macptr))
    (with-foreign-object (num-methods :int)
      (let ((methods (class-copy-method-list class num-methods)))
        (let ((result (list)))
          (dotimes (n (mem-aref num-methods :int) (nreverse result))
            (push (mem-aref methods :pointer n)
                  result))))))

  #+sbcl
  (:method ((class sb-sys:system-area-pointer))
    (with-foreign-object (num-methods :int)
      (let ((methods (class-copy-method-list class num-methods)))
        (let ((result (list)))
          (dotimes (n (mem-aref num-methods :int) (nreverse result))
            (push (mem-aref methods :pointer n)
                  result)))))))

(defun get-method-names (thing)
  (mapcar (alexandria:compose #'sel-get-name
                              #'method-get-name)
          (get-methods thing)))

(defgeneric graph->dot (graph stream)
  (:method :around (graph stream)
	   (format stream "~&digraph {~%~4trankdir=LR;~%")
	   (call-next-method)
	   (format stream "~&}"))
  (:method ((graph hash-table) stream)
    (loop for class being the hash-keys of graph using (hash-value superclass)
       do (format stream "~&~4t\"~a\" -> \"~a\"~%" class superclass))))

(defparameter *selector-cache* (make-hash-table :test 'equal))

(serapeum:eval-always
  (defun normalize-selector-name (sel-name)
    (substitute #\: #\? sel-name)))

(defun ensure-selector (name)
  (alexandria:ensure-gethash name
                             *selector-cache*
                             (sel-register-name name)))

(defmacro with-selectors ((&rest selector-specs) &body body)
  `(let (,@(mapcar (fw.lu:destructuring-lambda ((sym foreign-selector))
                     `(,sym (ensure-selector ,foreign-selector)))
                   (mapcar (fw.lu:glambda (spec)
                             (:method ((spec symbol))
                               (list spec (normalize-selector-name
                                           (string-downcase spec))))
                             (:method ((spec cons))
                               (list (car spec) (cadr spec))))
                           selector-specs)))
     ,@body))

(defmacro with-objc-classes ((&rest class-defs) &body body)
  `(let (,@(mapcar (fw.lu:destructuring-lambda ((lisp-name foreign-name))
                     `(,lisp-name (objc-look-up-class ,foreign-name)))
                   class-defs))
     ,@body))


(defgeneric make-objc-instance (class &rest args)
  (:method ((class string) &rest args)
    (apply #'make-objc-instance (objc-look-up-class class) args))
  #+ccl
  (:method ((class ccl:macptr) &rest args)
    (declare (ignore args))
    (with-selectors (alloc init)
      [[class alloc] init]))
  #+sbcl
  (:method ((class sb-sys:system-area-pointer) &rest args)
    (declare (ignore args))
    (with-selectors (alloc init)
      [[class alloc] init])))


(cffi:defcvar (ns-app "NSApp" :library appkit) :pointer)

#|
(uiop:nest (with-selectors (alloc init drain))
           (with-objc-classes ((nsobject "NSAutoreleasepool")))
           (eval-objc (objc-msg-send
                       (objc-msg-send
                        (objc-msg-send nsobject alloc)
                        init)
                       drain)))
(with-selectors (alloc init))

|#

