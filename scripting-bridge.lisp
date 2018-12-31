(defpackage :objc.scripting-bridge
  (:import-from :data-lens :defun-ct :shortcut)
  (:use :cl :cffi)
  (:export ))
(in-package :objc.scripting-bridge)
(named-readtables:in-readtable :objc-readtable)

(serapeum:eval-always
  (define-foreign-library scripting-bridge
    (:darwin (:framework "ScriptingBridge"))))

(use-foreign-library scripting-bridge)

(defun app (bundle-id)
  [#@SBApplication @(applicationWithBundleIdentifier:)
                   :pointer (objc-runtime:make-nsstring bundle-id)])

(defun itunes-app ()
  (app "com.apple.iTunes"))

(defun safari-app ()
  (app "com.apple.Safari"))

(defun current-track-info (itunes)
  (let* ((current-track [itunes @(currentTrack)]))
    (format t "~&Track: ~A (~v,1,0,'⋆<~>)~%Album: ~a (~v,1,0,'*<~>)~%Artist: ~a~%"
            [current-track @(name)]@
            (/ [current-track @(rating)]# 20)
            [current-track @(album)]@
            (/ [current-track @(albumRating)]# 10)
            [current-track @(artist)]@)))

(defvar *it*)
(serapeum:eval-always 
  (defgeneric sel (type sel)
    (:method :around (type sel)
             (lambda (*it*)
               (call-next-method)))

    (:method (type sel)
      [*it* sel])

    (:method ((type (eql :int)) sel)
      [*it* sel]#)

    (:method ((type (eql :string)) sel)
      [*it* sel]s)

    (:method ((type (eql :nsstring)) sel)
      [*it* sel]@)))

(defun-ct ext ()
  (lambda (it)
    (objc-runtime.data-extractors:extract-from-objc it)))

(defun-ct <> (&rest funs)
  (apply #'alexandria:compose funs))

(defun-ct tab-info ()
  (data-lens:juxt
   (<> (ext) (sel t @(name)))
   (<> (ext) (sel t @(URL)))))

(data-lens:shortcut window-info data-lens:juxt
  (sel :int @(id))
  #'identity
  (sel :nsstring @(name)))

(defun safari-tab-info (safari)
  (funcall (data-lens:over (tab-info))
           (mapcan (<> (ext) (sel t @(tabs)))
                   (objc-runtime.data-extractors:extract-from-objc
                    [safari @(windows)]))))

(defun format-tab-info (info)
  (format t "~{~:@{** ~a~% ~a~%~}~2%~}" info))


(defun safari-main ()
  (format-tab-info
   (safari-tab-info
    (safari-app))))


(defun count-invocations (hof)
  (lambda (f &rest hof-args)
    (let ((count 0))
      (declare (dynamic-extent count))
      (flet ((nested-lambda (&rest args)
               (prog1 (apply f count args)
                 (incf count))))
        (apply hof #'nested-lambda hof-args)))))

(defun-ct <count (f)
  (lambda (c &rest v)
    (list* c (apply f v))))

(defun-ct add-index (hof f)
  (lambda (&rest hof-args)
    (let ((count 0))
      (declare (dynamic-extent count))
      (flet ((nested-lambda (&rest args)
               (prog1 (apply f count args)
                 (incf count))))
        (apply hof #'nested-lambda hof-args)))))

(defun get-window-tabs (window)
  (funcall (<> 'objc-runtime.data-extractors:extract-from-objc
               (sel t @(tabs)))
           window))

(data-lens:shortcut get-safari-info <>
  (add-index 'mapcar 
             (<> (fw.lu:destructuring-lambda ((c win-id win-name . tabs))
                   (format t "~&Window: ~d ~d ~a~%~{~a~%~}~%" win-id c win-name tabs))
                 (<count
                  (<> (data-lens:transform-elt 1 (<> (add-index 'mapcar
                                                                (<> (fw.lu:destructuring-lambda ((c ti u))
                                                                      (format nil "~a ~a~%~4t~a" c ti u))
                                                                    (<count (tab-info))))
                                                     'get-window-tabs))
                      'window-info))))
  (ext)
  (sel t @(windows)))

(defun safari-2-main ()
  (get-safari-info (safari-app)))



(defun find-tab (name windows)
  (remove-if-not (serapeum:op (serapeum:string-contains-p name _))
                 windows
                 :key (<> 'string-downcase 'car)))

(defun current-tab (window)
  [window @(currentTab)])
(defun (setf current-tab) (new-value window)
  [window @(setCurrentTab:) :pointer new-value]
  new-value)

#+nil
(defun kebab-case (s)
  (loop
     for start = 0 then end
     for end = (position-if 'upper-case-p s) then (when start (position-if 'upper-case-p s :start (1+ end)))
     while start
     collect (string-downcase (subseq s start end)) into parts
     finally (return (serapeum:string-join parts #\-))))

#+nil
(defun get-method-symbol (selector-name package)
  (funcall (alexandria:compose (lambda (x) (intern x package))
                               #'string-upcase
                               (lambda (x) (substitute #\- #\: 
                                                       (string-trim ":-" x)))
                               'kebab-case)
           selector-name))

#+nil
(defun intern-method (selector-name package)
  (let ((symbol (get-method-symbol selector-name package)))
    (format t "~&~s ~s~%" symbol selector-name)
    (if (alexandria:starts-with-subseq "set" selector-name)
        (setf (fdefinition `(setf ,symbol))
              (lambda (new-val receiver &rest r)
                (declare (ignore r))
                (objc-runtime:objc-msg-send receiver (objc-runtime::ensure-selector selector-name) :pointer new-val)))
        (setf (fdefinition symbol)
              (sel (objc-runtime::ensure-selector selector-name))))))

#+nil
(defun populate-package (objc-class package)
  (mapc (lambda (method-name)
          (intern-method method-name package))
        (objc-runtime:get-method-names objc-class)))

#+nil
(defmacro define-objc-call (selector (&rest argument-specs) result-type &optional extractor)
  (declare (ignorable extractor))
  `(defun ,(get-method-symbol (cadr selector) *package*) (receiver ,@(mapcar #'car argument-specs))
     ,(case result-type
        (:string `(objc-runtime:objc-msg-send-string receiver ,selector ,@(mapcan #'reverse argument-specs)))
        ((:long :int) `(objc-runtime:objc-msg-send-int receiver ,selector ,@(mapcan #'reverse argument-specs)))
        (t `(objc-runtime:objc-msg-send receiver ,selector ,@(mapcan #'reverse argument-specs))))))

#+nil
(defmacro define-objc (() &body calls)
  `(progn ,@(loop for call in calls
               collect `(define-objc-call ,@call))))

#+nil
(define-objc-call @(init) () :pointer)
#+nil
(define-objc-call @(sharedApplication) () :pointer)

