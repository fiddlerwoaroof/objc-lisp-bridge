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
    (:darwin #p"libnsrect-expose.dylib")))


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

(defcfun (objc-register-class-pair "objc_registerClassPair" :library foundation)
    :void
  (superclass :pointer))

(defcfun (objc-get-protocol "objc_getProtocol" :library foundation)
    :pointer
  (name :string))

(defcfun (class-add-protocol "class_addProtocol" :library foundation)
    :boolean
  (class :pointer)
  (protocol :pointer))

(serapeum:eval-always
  (defctype sizet
      :ulong
    #+32-bit-target :uint))

(defcfun (class-add-ivar "class_addIvar" :library foundation)
    :boolean
  (class :pointer)
  (name :string)
  (size :ulong)
  (alignment :uint8)
  (types :string))

(defun add-pointer-ivar (class name)
  (class-add-ivar class name
                  (foreign-type-size :pointer)
                  (floor (log (foreign-type-size :pointer)
                              2))
                  "@"))


#+nil
(defun make-app-delegate-class (outlets)
  (let ((app-delegate-class (objc-runtime::objc-allocate-class-pair
                             #@NSObject "AppDelegate" 0)))
    (objc-runtime:add-pointer-ivar app-delegate-class "window")
    (objc-runtime:add-pointer-ivar app-delegate-class "delegate")

    (loop for outlet in outlets do
         (objc-runtime:add-pointer-ivar app-delegate-class outlet))

    app-delegate-class))

(defun %setup-objc-class (name base ivars)
  (let ((class-pair (objc-allocate-class-pair base name 0)))
    (loop for ivar in ivars
         )))

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

(defcfun (objc-msg-send-int "objc_msgSend")
    :int
  (cls o-class)
  (sel o-selector)
  &rest)

(defcfun (objc-msg-send-string "objc_msgSend")
    :string
  (cls o-class)
  (sel o-selector)
  &rest)

(defcfun (objc-msg-send "objc_msgSend")
    :pointer
  (cls o-class)
  (sel o-selector)
  &rest)

(defmacro safe-objc-msg-send (result-type thing selector &rest args)
  (alexandria:once-only (thing selector)
    `(if [,thing @(respondsToSelector:) :pointer ,selector]b
         ,(ccase result-type
            (string `[,thing ,selector ,@args]s)
            (nsstring `[,thing ,selector ,@args]@)
            (num `[,thing ,selector ,@args]#)
            (bool `[,thing ,selector ,@args]b)
            (id `[,thing ,selector ,@args]))
         (error "invalid selector"))))

;;; This is a macro, because objc-msg-send is a macro.... which makes "apply" impossible
;;; \o/
(defmacro objc-msg-send-nsstring (thing selector &rest args)
  `[[,thing ,selector ,@args] @(UTF8String)]s)

(defmacro objc-msg-send-bool (thing selector &rest args)
  `(= 1 [,thing ,selector ,@args]#))

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

(defcfun (class-add-method "class_addMethod" :library foundation)
    :boolean
  (class :pointer)
  (selector :pointer)
  (cb :pointer)
  (type :string))


(defcfun (object-get-class "object_getClass" :library foundation)
    :pointer
  (object :pointer))

(defcfun (object-get-ivar "object_getIvar" :library foundation)
    :pointer
  (object :pointer)
  (ivar :pointer))

(defcfun (object-get-instance-variable "object_getInstanceVariable" :library foundation)
    :pointer
  (object :pointer)
  (name :string)
  (out :pointer))

(defcfun (class-get-property "class_getProperty" :library foundation)
    :pointer
  (cls o-class)
  (name :string))

(defcstruct objc-property-attribute-t
  (name :string)
  (value :string))

(defcfun (class-add-property "class_addProperty" :library foundation)
    :pointer
  (cls o-class)
  (name :string)
  (attributes (:pointer (:struct objc-property-attribute-t)))
  (attribute-count :unsigned-int))

(defcfun (property-copy-attribute-value "property_copyAttributeValue" :library foundation)
    :string
  (prop :pointer)
  (name :string))


(defcfun (property-get-attributes "property_getAttributes" :library foundation)
    :string
  (prop :pointer))

(defun get-classes ()
  (let ((num-classes (objc-get-class-list (null-pointer) 0))
        (result (list)))
    (with-foreign-object (classes :pointer num-classes)
      (dotimes (n (objc-get-class-list classes num-classes) (nreverse result))
        (push (mem-aref classes :pointer n)
              result)))))

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

(defmethod get-methods (f)
  (list))


(defun make-nsstring (str)
  [[#@NSString @(alloc)] @(initWithCString:encoding:) :string str :uint 1])

(defun extract-nsstring (ns-str)
  [ns-str @(UTF8String)]s)

(defun get-method-name (method)
  (sel-get-name (method-get-name method)))

(defun get-method-names (thing)
  (mapcar (alexandria:compose #'sel-get-name
                              #'method-get-name)
          (get-methods thing)))

(defgeneric graph->dot (graph stream)
  (:method :around (graph stream)
     (declare (ignore graph))
	   (format stream "~&digraph {~%~4trankdir=LR;~%")
	   (call-next-method)
	   (format stream "~&}"))
  (:method ((graph hash-table) stream)
    (loop for class being the hash-keys of graph using (hash-value superclass)
       do (format stream "~&~4t\"~a\" -> \"~a\"~%" class superclass))))

(defparameter *selector-cache* (make-hash-table :test 'equal))
(defparameter *class-cache* (make-hash-table :test 'equal))

(serapeum:eval-always
  (defun normalize-selector-name (sel-name)
    (substitute #\: #\? sel-name)))

(defun ensure-class (name)
  (let ((objc-class (objc-look-up-class name)))
    (when (and objc-class (not (null-pointer-p objc-class)))
      (alexandria:ensure-gethash name *class-cache* objc-class))))

(defun ensure-selector (name)
  (alexandria:ensure-gethash name
                             *selector-cache*
                             (sel-register-name name)))

(defmacro with-objc-classes ((&rest class-defs) &body body)
  `(let (,@(mapcar (fw.lu:destructuring-lambda ((lisp-name foreign-name))
                     `(,lisp-name (objc-look-up-class ,foreign-name)))
                   class-defs))
     ,@body))



(cffi:defcvar (ns-app "NSApp" :library appkit) :pointer)

(defclass objc-class ()
  ((%objc-class-name :initarg :name :reader name)
   (%class-pointer :initarg :pointer :reader class-pointer)
   (%cache :initform (make-hash-table :test 'equal) :allocation :class :reader objc-class-cache)))

(defclass objc-selector ()
  ((%objc-selector-name :initarg :name :reader name)
   (%selector-pointer :initarg :pointer :reader selector-pointer)
   (%args :initarg :args :reader args)
   (%result-type :initarg :result-type :reader result-type)
   (%cache :initform (make-hash-table :test 'equal) :allocation :class :reader objc-selector-cache))
  (:metaclass closer-mop:funcallable-standard-class))

(defun make-message-lambda-form (args rettype)
  (alexandria:with-gensyms ((target :target))
    (fw.lu:with (arg-syms (mapcar (serapeum:op _ (gensym "arg")) args))
      `(lambda (selector)
         (lambda (,target ,@arg-syms)
           (cffi:foreign-funcall
            "objc_msgSend"
            :pointer ,target
            :pointer selector
            ,@(mapcan #'list args arg-syms)
            ,rettype))))))

(defmethod initialize-instance :after ((sel objc-selector) &key &allow-other-keys)
  (with-accessors ((pointer selector-pointer)
                   (args args)
                   (rettype result-type))
      sel
    (closer-mop:set-funcallable-instance-function
     sel
     (funcall (compile nil (make-message-lambda-form args rettype))
              pointer))))

(defgeneric reset-class-cache (class)
  (:method ((class symbol))
    (reset-class-cache (find-class class)))
  (:method ((class class))
    (setf (slot-value (closer-mop:class-prototype class) '%cache)
          (make-hash-table :test 'equal))))


(define-condition no-such-objc-class (serious-condition)
  ((%wanted-name :initarg :wanted-name :reader wanted-name))
  (:report (lambda (c s)
             (format s "No such Objective-C class: ~a" (wanted-name c)))))

(defun %ensure-wrapped-objc-class (name)
  (let* ((class-cache (objc-class-cache (closer-mop:class-prototype (find-class 'objc-class))))
         (cached (gethash name class-cache)))
    (if cached
        cached 
        (let ((objc-class (objc-look-up-class name)))
          (if (null-pointer-p objc-class)
              (error 'no-such-objc-class :wanted-name name)
              (setf (gethash name class-cache)
                    (make-instance 'objc-class
                                   :name name
                                   :pointer objc-class)))))))

;; TODO: should this error if there is no corresponding selector?
;;         Or should we let that fall through to message sending?
(defun %ensure-wrapped-objc-selector (name target-class result-type args)
  (assert (= (count #\: name)
             (length args))
          (name args)
          "Invalid number of arg types for selector ~s" name)

  (let* ((class-cache (objc-selector-cache (closer-mop:class-prototype (find-class 'objc-selector))))
         (cached (gethash (list name target-class)
                          class-cache)))
    (if cached
        cached 
        (let ((objc-selector (ensure-selector name)))
          (setf (gethash (list name target-class) class-cache)
                (make-instance 'objc-selector
                               :name name
                               :pointer objc-selector
                               :result-type result-type
                               :args args))))))

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

(defun ensure-wrapped-objc-class (name)
  (tagbody
   retry (restart-case (return-from ensure-wrapped-objc-class
                         (%ensure-wrapped-objc-class name))
           (use-value (new)
             :interactive (lambda ()
                            (format t "New Objective-C class name: ")
                            (multiple-value-list (read)))
             :report "Retry with new class name"
             (setf name new)
             (go retry)))))

(defmacro with-selectors ((&rest selector-specs) &body body)
  `(let (,@(mapcar (fw.lu:destructuring-lambda ((sym foreign-selector))
                     `(,sym (ensure-selector ,foreign-selector)))
                   (mapcar (fwoar.anonymous-gf:glambda (spec)
                             (:method ((spec symbol))
                               (list spec (normalize-selector-name
                                           (string-downcase spec))))
                             (:method ((spec cons))
                               (list (car spec) (cadr spec))))
                           selector-specs)))
     ,@body))


(defmacro with-typed-selectors ((&rest defs) &body body)
  (let ((expanded-defs (loop for ((name objc-name) args ret-type) in defs
                          collect
                            `((,name (&rest r) (apply ,name r))
                              (,name (%ensure-wrapped-objc-selector ,objc-name ',ret-type ',args))))))
    `(let (,@(mapcar #'second expanded-defs))
       (flet (,@(mapcar #'first expanded-defs))
         ,@body))))

