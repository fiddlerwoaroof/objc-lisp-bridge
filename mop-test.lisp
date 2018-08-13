(defpackage :mop-test
  (:shadowing-import-from :closer-mop
                          :standard-method :standard-generic-function
                          :defmethod :defgeneric :standard-class)
  (:use :cl :closer-mop)
  (:export ))
(in-package :mop-test)

(defclass slot-logging-class (standard-class)
  ((%log-stream :accessor log-stream :initform (make-synonym-stream '*trace-output*))))

(defmethod validate-superclass ((class slot-logging-class) (super standard-class))
  t)

(defmethod slot-value-using-class ((class slot-logging-class) instance slotd)
  (format (log-stream class) "~&Instance ~s of class ~s read slot ~s~%"
          (class-name class)
          instance
          (slot-definition-name slotd))
  (call-next-method))

(defmethod (setf slot-value-using-class) (new-value (class slot-logging-class) instance slotd)
  (format (log-stream class) "~&Instance ~s of class ~s write slot ~s: ~s~%"
          (class-name class)
          instance
          (slot-definition-name slotd)
          new-value)
  (call-next-method))

(defclass tmp (standard-object)
  ((%a :reader a :initform :b))
  (:metaclass slot-logging-class))
