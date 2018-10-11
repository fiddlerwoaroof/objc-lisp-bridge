(defpackage :clim-objc-browser
  (:use :clim-lisp :clim))
(in-package :clim-objc-browser)

(define-application-frame class-browser ()
  ((classes :initarg :classes :reader classes)
   (visible-classes :initform nil :accessor visible-classes)
   (current-class :initform nil :accessor current-class))
  (:panes (classes :application
                   :incremental-redisplay t
                   :display-function 'display-classes
                   :double-buffering t)
          (methods :application
                   :incremental-redisplay t
                   :display-function 'display-methods)
          (int :interactor))
  (:pointer-documentation t)
  (:layouts (default (vertically ()
                       (horizontally ()
                         classes methods)
                       int)))
  (:default-initargs
      :classes (sort (remove-if (serapeum:op
                                  (alexandria:starts-with #\_
                                                          (objc-runtime::objc-class-get-name _)))
                                (objc-runtime::get-classes))
                     #'string-lessp
                     :key 'objc-runtime::objc-class-get-name)))

(defun reset-application-frame ()
  (setf (visible-classes clim:*application-frame*) nil
        (current-class clim:*application-frame*) nil
        (slot-value clim:*application-frame* 'classes)
        (sort (remove-if (serapeum:op (alexandria:starts-with #\_
                                                              (objc-runtime::objc-class-get-name _)))
                         (objc-runtime::get-classes))
              #'string-lessp
              :key 'objc-runtime::objc-class-get-name)))

(define-presentation-type objc-class ())
(define-presentation-method present (object (type objc-class) stream view &key)
  (declare (ignore view))
  (format stream "#[OBJC Class: ~a]"
          (objc-runtime::objc-class-get-name object)))

(define-presentation-type objc-method ())
(define-presentation-method present (object (type objc-method) stream view &key)
  (declare (ignore view))
  (format stream "@(~a)"
          (objc-runtime::get-method-name object)))

(defun display-classes (frame pane)
  (updating-output (pane :unique-id (or (visible-classes frame)
                                        (classes frame))
                         :id-test 'eq)
    (loop for class in (or (visible-classes frame)
                           (classes frame))
       do
         (updating-output (pane :unique-id (cffi:pointer-address class)
                                :id-test 'eql
                                :cache-value class
                                :cache-test 'eql)
           (with-output-as-presentation (pane class 'objc-class)
             (format pane "~&   ~a~%" (objc-runtime::objc-class-get-name class)))))))

(defun display-methods (frame pane)
  (updating-output (pane :unique-id (current-class frame)
                         :id-test 'eq)
    (when (current-class frame)
      (loop for method in (sort (objc-runtime::get-methods (current-class frame))
                                'string<
                                :key 'objc-runtime::get-method-name)
         do
           (with-output-as-presentation (pane method 'objc-method)
             (format pane "   Method: ~a~%" (objc-runtime::get-method-name method)))))))

(define-class-browser-command (com-get-methods :name t :menu t) ((the-class objc-class :gesture :select))
  (setf (current-class *application-frame*) the-class))


(define-class-browser-command (com-refresh-classes :name t :menu t) ()
  (reset-application-frame))

(define-class-browser-command (com-filter-classes :name t :menu t) ((prefix string))
  (setf (visible-classes *application-frame*)
        (remove-if-not (serapeum:op
                         (alexandria:starts-with-subseq prefix _ :test #'char-equal))
                       (classes *application-frame*)
                       :key 'objc-runtime::objc-class-get-name)))

(defun main ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'class-browser)))
