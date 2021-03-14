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

(defun read-objc-form (s char)
  (declare (ignore char))
  (flet ((get-call-form (safe-p result-type base-form)
           (if safe-p
               `(safe-objc-msg-send ,result-type)
               `(,base-form))))
    (let* ((info (read-delimited-list #\] s t))
           (safe-p (when (eql #\? (peek-char nil s nil #\p t))
                     (read-char s t nil t)))
           (return-type (case (peek-char nil s nil #\p t)
                          (#\# (read-char s t nil t) :int #+(or)(get-call-form safe-p 'num 'objc-msg-send-int))
                          (#\& (read-char s t nil t) :pointer #+(or)(get-call-form safe-p 'id 'objc-msg-send))
                          (#\@ (read-char s t nil t) :pointer #+(or)(get-call-form safe-p 'nsstring 'objc-msg-send-nsstring))
                          (#\b (read-char s t nil t) :bool #+(or)(get-call-form safe-p 'bool 'objc-msg-send-bool))
                          (#\s (read-char s t nil t) :string #+(or)(get-call-form safe-p 'string 'objc-msg-send-string))
                          (t                         :pointer #+(or)(get-call-form safe-p 'id 'objc-msg-send)))))
      (when info
        (destructuring-bind (obj message . args) info
          `(foreign-funcall "objc_msgSend"
                            :pointer ,obj
                            :pointer ,message
                            ,@args
                            ,return-type))))))

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
