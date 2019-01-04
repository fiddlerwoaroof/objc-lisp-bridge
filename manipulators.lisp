(cl:in-package :objc.manipulators)
(named-readtables:in-readtable :objc-readtable)

(serapeum:eval-always 
  (let (*it*)
    (declare (special *it*))
    (defgeneric sel (type sel)
      (:method :around (type sel)
               (lambda (*it*)
                 (declare (special *it*))
                 (call-next-method)))

      (:method (type sel)
        [*it* sel])

      (:method ((type (eql :int)) sel)
        [*it* sel]#)

      (:method ((type (eql :string)) sel)
        [*it* sel]s)

      (:method ((type (eql :nsstring)) sel)
        [*it* sel]@))))

(defun-ct ext ()
  (lambda (it)
    (objc-runtime.data-extractors:extract-from-objc it)))

(defun-ct <> (&rest funs)
  (apply #'alexandria:compose funs))

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
