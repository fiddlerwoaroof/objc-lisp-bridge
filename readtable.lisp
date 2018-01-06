(in-package :objc-runtime)

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
                          (let ((class-name (coerce (loop for c = (read-char s nil nil t)
                                                       until (or (null c)
                                                                 (serapeum:whitespacep c)
                                                                 (member c '(#\) #\(  #\[ #\])))
                                                       collect c)
                                                    'string)))
                            `(objc-look-up-class ,class-name)))))
