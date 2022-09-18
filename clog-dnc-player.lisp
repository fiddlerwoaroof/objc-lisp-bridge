(defpackage :objc.clog-dnc-player
  (:use :cl :clog)
  (:export ))
(in-package :objc.clog-dnc-player)
(named-readtables:in-readtable :objc-readtable)

(fw.lu:defclass+ store ()
  ((%track-name
    :accessor track-name
    :initform nil)
   (%track-artist
    :accessor track-artist
    :initform nil)
   (%track-album
    :accessor track-album
    :initform nil)
   (%player-state
    :accessor player-state
    :initform nil)))
(defvar *store*)

(defun incorporate (store info)
  (prog1 store
    (trivia:match info
      ((trivia:hash-table-entries
        "Name" name
        "Album" album
        "Artist" artist
        "Player State" player-state)
       (setf
        (track-name store) name
        (track-album store) album
        (track-artist store) artist
        (player-state store) player-state)))))

(defun reducer-task (store)
  (lambda ()
    (loop
      for message = (sb-concurrency:receive-message
                     objc.notification:*mailbox*)
      do (incorporate *store* message))))

(defun on-new-window (body)
  (let ((name         (create-section body :h2 :content "track"))
        (artist       (create-div body :content "artist"))
        (album        (create-div body :content "album"))
        (player-state (create-div body :content "player-state"))
        (play-pause   (create-button body :content "play/pause")))
    (link-slot-to-element *store* track-name name)
    (link-slot-to-element *store* track-artist artist)
    (link-slot-to-element *store* track-album album)
    (link-slot-to-element *store* player-state player-state)
    (set-on-click play-pause
                  (lambda (button)
                    (declare (ignore button))
                    [(objc.scripting-bridge::itunes-app) @(playpause)]?))))

(defvar *initialized* nil)
(defun doit ()
  (unless *initialized*
    (setf *initialized* t)
    (setf *store* (make-instance 'store))
    (bt:make-thread (reducer-task *store*) :name "Reducer")
    (objc.notification:setup-notifications)
    (objc.notification:observe-notifications (objc.notification:dnc)
                                             "com.apple.Music.playerInfo")
    (bt:make-thread 'objc.notification:main-loop-ticker)
    (set-on-new-window 'on-new-window :path "/player")))
