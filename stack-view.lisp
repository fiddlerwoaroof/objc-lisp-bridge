(defpackage :fwoar.stack-view
  (:use :cl )
  (:export ))
(in-package :fwoar.stack-view)
(named-readtables:in-readtable :objc-readtable)

(cffi:defcstruct ns-edge-insets 
  (top :double)
  (left :double)
  (bottom :double)
  (right :double))

(cffi:defcfun (make-edge-insets "NSEdgeInsetsMake")
    (:struct ns-edge-insets)
  (top :double)
  (left :double)
  (bottom :double)
  (right :double))

(cffi:defcfun (%set-edge-insets "objc_msgSend")
    :void
  (cls objc-runtime::o-class)
  (sel objc-runtime::o-selector)
  (value (:struct ns-edge-insets)))

(defun set-edge-insets (stack-view top right bottom left)
  (%set-edge-insets stack-view
                    @(setEdgeInsets:)
                    (list 'top (coerce top 'double-float)
                          'left (coerce left 'double-float)
                          'bottom (coerce bottom 'double-float)
                          'right (coerce right 'double-float))))

(defun add-view-to-stack (stack-view child)
  [stack-view @(addView:inGravity:) :pointer child :int 1])
