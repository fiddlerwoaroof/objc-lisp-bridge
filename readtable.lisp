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

(named-readtables:defreadtable :objc-readtable
  (:merge :standard)
  (:syntax-from :standard #\) #\])
  (:macro-char #\[ (lambda (s char)
                     char
                     (let ((info (read-delimited-list #\] s t)))
                       (when info
                         (destructuring-bind (obj message . args) info
                           `(objc-msg-send ,obj ,message ,@args)))))
               nil)
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
                            `(objc-look-up-class ,class-name))))
  (:macro-char #\@ :dispatch t)
  (:dispatch-macro-char #\@ #\( (read-until (serapeum:op (char= _ #\)))
                                            'ensure-selector))
  (:dispatch-macro-char #\@ #\" (read-until (serapeum:op (char= _ #\"))
                                            'make-nsstring)))
