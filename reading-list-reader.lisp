#+build
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-pathname-defaults* (truename "~/git_repos/objc-lisp-bridge/"))
  (load (compile-file "objc-runtime.asd")))

#+build
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:objc-runtime :yason :plump :cl-ppcre)))

(defpackage :reading-list-reader
  (:use :cl )
  (:export ))
(in-package :reading-list-reader)

(serapeum:eval-always
 (named-readtables:in-readtable :objc-readtable))

(defparameter *reading-list-location* "~/Library/Safari/Bookmarks.plist")

(defun get-plist (file)
  [#@NSDictionary @(dictionaryWithContentsOfFile:)
                  :pointer (objc-runtime::make-nsstring file)])


(defun objc-isa (obj class)
  (unless (or (cffi:null-pointer-p obj)
              (cffi:null-pointer-p class))
    (= [obj @(isKindOfClass:) :pointer class]#
       1)))

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
    (t (format *error-output* "~&other... ~s~%" (objc-runtime::objc-class-get-name
                                                 (objc-runtime::object-get-class obj))))))

(defun extract-nsdictionary (nsdict)
  (yason:with-output (*standard-output* :indent t)
    (maphash 'yason:encode-object-element
             (extract-from-objc nsdict))))

(defun select-child (d title)
  (remove-if-not (serapeum:op
                   (equal (gethash "Title" _)
                          title))
                 (gethash "Children" d)))

(defun slugify (s)
  (when s
    (cl-ppcre:regex-replace-all "\\s+" (string-downcase s) "_")))

(defun make-org-entry (s title url preview tag)
  (format s "~&** ~a :~{~a:~}~% ~a~2% ~{~<~% ~1,80:;~a~> ~}~%"
          title (alexandria:ensure-list tag)
          url
          (serapeum:tokens preview)))

(defun get-readinglist-info (bookmarks)
  (mapcar (serapeum:juxt
           (fw.lu:op (fw.lu:pick '("URIDictionary" "title") _))
           (fw.lu:op (fw.lu:pick '("URLString") _))
           (fw.lu:op (plump:decode-entities (coerce (fw.lu:pick '("ReadingList" "PreviewText") _)
                                                    'simple-string)
                                            t))
           (fw.lu:op (slugify (fw.lu:pick '("ReadingListNonSync" "siteName") _))))
          (gethash "Children"
                   (car
                    (select-child bookmarks "com.apple.ReadingList")))))


(defun make-org-file (s bookmarks)
  (format s "~&* Safari Reading List~%")
  (serapeum:mapply (serapeum:partial 'make-org-entry s)
                   (get-readinglist-info bookmarks)))

(defun main ()
  #+(and build sbcl)
  (progn (sb-ext:disable-debugger)
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void))))
  (make-org-file *standard-output*
                 (extract-from-objc (get-plist (uiop:unix-namestring (truename *reading-list-location*))))))

#+build
(sb-ext:save-lisp-and-die "reading-list2org" :toplevel 'main :executable t)
