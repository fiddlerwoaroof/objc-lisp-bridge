(in-package :objc-runtime)

#+ccl
(defgeneric send-message (object message &rest args)
  (:method ((object ccl:macptr) (message (eql 'alloc)) &rest args)
    (apply #'objc-msg-send object (ensure-selector "alloc") args)))

(defun read-until (test symbol-prefix &optional stop-before-chars)
  "Read from a string until"
  (lambda (s c b)
    (declare (ignore c b))
    (let ((class-name (coerce (loop for next-char = (peek-char nil s nil nil t)
                                    while next-char
                                    until (funcall test next-char)
                                    collect (read-char s t nil t)
                                    finally (when (and (not (member next-char
                                                                    stop-before-chars))
                                                       (funcall test next-char))
                                              (read-char s t nil t)))

                              'string)))
      `(,symbol-prefix ,class-name))))

(defmacro objc-send (obj message return &rest args)
  (let* ((return-t (case return
                     (:nsstring :pointer)
                     (t return)))
         (result `(cffi:foreign-funcall "objc_msgSend"
                                        :pointer ,obj
                                        :pointer ,message
                                        ,@args
                                        ,return-t)))
    (case return
      (:nsstring `(objc-send ,result
                             (ensure-selector "UTF8String")
                             :string))
      (t result))))

(defun read-objc-form (s char)
  (declare (ignore char))
  (let* ((info (read-delimited-list #\] s t))
         (safe-p (when (eql #\? (peek-char nil s nil #\p t))
                   (read-char s t nil t)))
         (return-t (case (peek-char nil s nil #\p t)
                     (#\# (read-char s t nil t) :int)
                     (#\& (read-char s t nil t) :pointer)
                     (#\@ (read-char s t nil t) :nsstring)
                     (#\b (read-char s t nil t) :bool)
                     (#\s (read-char s t nil t) :string)
                     (t                         :pointer))))
    (when info
      (destructuring-bind (obj message . args) info
        (if safe-p
            `(safe-objc-msg-send ,return-t ,obj ,message ,@args)
            `(objc-send ,obj ,message ,return-t ,@args))))))

(named-readtables:defreadtable :objc-readtable
  (:merge :standard)
  (:syntax-from :standard #\) #\])
  (:macro-char #\[ 'read-objc-form nil)
  (:dispatch-macro-char #\# #\@
                        (lambda (s c b)
                          c b
                          (let ((class-name (coerce (loop for c = (peek-char nil s nil nil t)
                                                          until (or (null c)
                                                                    (serapeum:whitespacep c)
                                                                    (member c
                                                                            '(#\) #\(  #\[ #\])))
                                                          collect (read-char s t nil t))
                                                    'string)))
                            `(ensure-class ,class-name))))
  (:macro-char #\@ :dispatch t)
  (:dispatch-macro-char #\@ #\( (read-until (serapeum:op (char= _ #\)))
                                            'ensure-selector))
  (:dispatch-macro-char #\@ #\" (read-until (serapeum:op (char= _ #\"))
                                            'make-nsstring)))
