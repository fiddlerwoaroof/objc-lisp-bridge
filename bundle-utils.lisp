(in-package :objc-runtime.bundle-utils)
(named-readtables:in-readtable :objc-readtable)

(defun bundle-resource-root ()
  (uiop:ensure-directory-pathname
   [[[#@NSBundle @(mainBundle)] @(resourceURL)] @(fileSystemRepresentation)]s))

(defun application-support-directory (&optional (scope :user))
  (let ((next-step (make-pathname :directory (list :relative (objc-runtime.data-extractors:extract-from-objc
                                                              [[[#@NSBundle @(mainBundle)]
                                                                @(infoDictionary)]
                                                               @(objectForKey:)
                                                               :pointer @"CFBundleIdentifier"])))))
    (car
     (mapcan (alexandria:compose 'serapeum:unsplice
                                 (lambda (p) (when p (merge-pathnames next-step p)))
                                 'probe-file)
             (mapcar (lambda (it) [it @(fileSystemRepresentation)]?s)
                     (objc-runtime.data-extractors:extract-from-objc
                      [[#@NSFileManager @(defaultManager)] @(URLsForDirectory:inDomains:)
                       :int 14 ;; NSApplicationSupportDirectory
                       :int (ccase scope
                              (:user 1)
                              (:local 2)
                              (:network 4))]))))))

(defun setup-bundle-logical-pathnames ()
  (setf (logical-pathname-translations "BUNDLE")
        `(("BUNDLE:RESOURCES;**;*.*.*" ,(bundle-resource-root))
          ("BUNDLE:SUPPORT;USER;**;*.*.*" ,(application-support-directory :user))
          ("BUNDLE:SUPPORT;LOCAL;**;*.*.*" ,(application-support-directory :local)))))

(defun ensure-application-support ()
  (setup-bundle-logical-pathnames)
  (translate-logical-pathname
   (ensure-directories-exist
    #P"BUNDLE:APPLICATION-SUPPORT;USER;")))

(named-readtables:defreadtable config
  (:case :preserve)
  (:syntax-from :standard #\) #\))
  (:macro-char #\( (lambda (s c)
                     c
                     (read-delimited-list #\) s t))
               nil)
  (:macro-char #\, (lambda (s c)
                     c
                     (values))
               nil)
  (:syntax-from :standard #\" #\")
  (:syntax-from :standard #\: #\:)
  (:syntax-from :standard #\) #\})
  (:macro-char #\{ (lambda (s c)
                     c
                     (alexandria:plist-hash-table (read-delimited-list #\} s t)
                                                  :test 'equal))
               nil)
  (:syntax-from :standard #\) #\])
  (:macro-char #\[ (lambda (s c)
                     c
                     (apply #'vector (read-delimited-list #\] s t)))
               nil))

(defparameter *config-pprint*
  (copy-pprint-dispatch))

(set-pprint-dispatch 'hash-table
                     (lambda (s hash-table)
                       (pprint-logical-block (s nil)
                         (princ "{"  s)
                         (let ((v (fset:convert 'list (fset:convert 'fset:map hash-table))))
                           (when v
                             (pprint-logical-block (s v)
                               (pprint-indent :block 0 s)
                               (loop do
                                 (destructuring-bind (key . value) (pprint-pop)
                                   (format s "~s ~s" key value)
                                   (pprint-exit-if-list-exhausted)
                                   (princ ", " s)
                                   (pprint-newline :linear s))))))
                         (princ #\} s)))
                     1 *config-pprint*)

(set-pprint-dispatch 'vector
                     (lambda (s vector)
                       (pprint-logical-block (s nil)
                         (princ "["  s)
                         (let ((v (coerce vector 'list)))
                           (when v
                             (pprint-logical-block (s v)
                               (pprint-indent :block 0 s)
                               (loop do
                                 (prin1 (pprint-pop) s)
                                 (pprint-exit-if-list-exhausted)
                                 (princ ", " s)
                                 (pprint-newline :linear s)))))
                         (princ #\] s)))
                     1 *config-pprint*)

(defun print-for-config (object s)
  (let ((*print-readably* t)
        (*print-pprint-dispatch* *config-pprint*))
    (pprint object s)))

(defun read-from-config (s)
  (let ((*readtable* (named-readtables:find-readtable 'config)))
    (read s)))
