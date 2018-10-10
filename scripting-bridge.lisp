(defpackage :objc.scripting-bridge
  (:use :cl :cffi)
  (:export ))
(in-package :objc.scripting-bridge)
(named-readtables:in-readtable :objc-readtable)

(serapeum:eval-always
  (define-foreign-library scripting-bridge
    (:darwin (:framework "ScriptingBridge"))))

(use-foreign-library scripting-bridge)

(defun get-itunes-app ()
  [#@SBApplication @(applicationWithBundleIdentifier:) :pointer @"com.apple.iTunes"])

(defun get-current-track-info (itunes)
  (let* ((current-track [itunes @(currentTrack)]))
    (format t "~&Track: ~A (~v,1,0,'⋆<~>)~%Album: ~a (~v,1,0,'*<~>)~%Artist: ~a~%"
            [current-track @(name)]@
            (/ [current-track @(rating)]# 20)
            [current-track @(album)]@
            (/ [current-track @(albumRating)]# 10)
            [current-track @(artist)]@)))
