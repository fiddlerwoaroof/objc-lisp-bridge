;; objc-data-extractor.lisp


;; [[file:~/git_repos/objc-lisp-bridge/README.org::*objc-data-extractor.lisp][objc-data-extractor.lisp:1]]
(defpackage :objc-runtime.data-extractors
  (:use :cl )
  (:export
   #:extract-from-objc
   #:define-extractor
   #:clear-extractors
   #:add-extractor
   #:get-plist
   #:objc-typecase))

(in-package :objc-runtime.data-extractors)
(named-readtables:in-readtable :objc-readtable)

(defun get-plist (file)
  [#@NSDictionary @(dictionaryWithContentsOfFile:)
                  :pointer (objc-runtime::make-nsstring file)])

(defun objc-subclass-p (sub super)
  (unless (or (cffi:null-pointer-p sub)
              (cffi:null-pointer-p super))
    (or (eql sub super)
        (= [sub @(isSubclassOfClass:) :pointer [super @(class)]]#
           1))))

(defun order-objc-classes (classes &rest r &key key)
  (declare (ignore key))
  (apply 'stable-sort
         (copy-seq classes)
         'objc-subclass-p
         r))

(defun objc-isa (obj class)
  (unless (or (cffi:null-pointer-p obj)
              (cffi:null-pointer-p class))
    (= [obj @(isKindOfClass:) :pointer class]#
       1)))

(defun objc-pick-by-type (obj pairs)
  (assoc obj
         (order-objc-classes pairs :key 'car)
         :test 'objc-isa))

(serapeum:eval-always
  (defun make-cases (cases obj)
    (mapcar (serapeum:op
              `(if (objc-isa ,obj ,(car _1))
                   (progn ,@(cdr _1))))
                   cases)))

(defmacro objc-typecase (form &body ((case-type &body case-handler) &rest cases))
  (alexandria:once-only (form)
    (let* ((initial-cases `((,case-type ,@case-handler) ,@(butlast cases)))
           (cases (fw.lu:rollup-list (make-cases initial-cases form)
                                     (if (eql t (caar (last cases)))
                                         `((progn ,@(cdar (last cases))))
                                         (make-cases (last cases) form)))))
      cases)))

(defun map-nsarray (fn arr)
  (unless (and (cffi:pointerp arr)
               (objc-isa arr #@NSArray))
    (error "must provide a NSArray pointer"))
  (loop for x below [arr @(count)]#
     collect (funcall fn [arr @(objectAtIndex:) :int x])))

(defun nsarray-contents (arr)
  (unless (and (cffi:pointerp arr)
               (objc-isa arr #@NSArray))
    (error "must provide a NSArray pointer"))
  (dotimes (n [arr @(count)]#)
    (let ((obj [arr @(objectAtIndex:) :int n ]))
      (objc-typecase obj
        (#@NSString (format t "~&string~%"))
        (#@NSArray (format t "~&array~%"))
        (#@NSDictionary (format t "~&dictionary~%"))
        (t (format t "~&other... ~s~%" (objc-runtime::objc-class-get-name
                                        (objc-runtime::object-get-class obj))))))))

(defmacro funcall-some (fun &rest args)
  (alexandria:once-only (fun)
    `(if ,fun
         (funcall ,fun ,@args))))

(defvar *objc-extractors* (list)
  "Functions called to extract specific data types")

(defun extract-from-objc (obj)
  (objc-typecase obj
    (#@NSDate [[[[#@NSISO8601DateFormatter @(alloc)]
                 @(init)]
                @(stringFromDate:) :pointer obj]
               @(UTF8String)]s)
    (#@NSString [obj @(UTF8String)]s)
    (#@NSNumber (parse-number:parse-number
                 (objc-runtime::extract-nsstring
                  [obj @(stringValue)])))
    (#@NSArray (map-nsarray #'extract-from-objc obj))
    (#@NSDictionary (fw.lu:alist-string-hash-table
                     (pairlis (map-nsarray #'extract-from-objc [obj @(allKeys)])
                              (map-nsarray #'extract-from-objc [obj @(allValues)]))))
    (t (or (funcall-some (cdr (objc-pick-by-type obj *objc-extractors*))
                         obj)
           obj))))

(defmacro define-extractor (class (o) &body body)
  `(serapeum:eval-always
     (add-extractor ,class
                    (lambda (,o)
                      ,@body))
     *objc-extractors*))

(defun clear-extractors ()
  (setf *objc-extractors* ()))

(serapeum:eval-always
  (defun add-extractor (class cb)
    (unless (member class *objc-extractors* :test 'cffi:pointer-eq :key #'car)
      (setf *objc-extractors*
            (merge 'list *objc-extractors* (list (cons class cb))
                   'objc-subclass-p
                   :key 'car)))
    *objc-extractors*))
;; objc-data-extractor.lisp:1 ends here
