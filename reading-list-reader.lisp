(defpackage :reading-list-reader
  (:use :cl )
  (:export ))
(in-package :reading-list-reader)

(serapeum:eval-always
  (named-readtables:in-readtable :objc-readtable))

(defun main ()
  #+(and build sbcl)
  (progn (sb-ext:disable-debugger)
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "disable_lossage_handler"
                                 (function sb-alien:void))))
  (make-org-file *standard-output*
                 (translate-plist (get-bookmark-filename))))

(defparameter *reading-list-location* "Library/Safari/Bookmarks.plist")
(defun get-bookmark-filename ()
  (merge-pathnames *reading-list-location*
                   (truename "~/")))

(defun translate-plist (fn)
  (objc-runtime.data-extractors:extract-from-objc
   (objc-runtime.data-extractors:get-plist fn)))

(defun make-org-file (s bookmarks)
  (format s "~&* Safari Reading List~%")
  (serapeum:mapply (serapeum:partial 'make-org-entry s)
                   (get-readinglist-info bookmarks)))

(defun make-org-entry (s title url preview tag)
  (format s "~&** ~a :~{~a:~}~% ~a~2% ~{~<~% ~1,80:;~a~> ~}~%"
          title (alexandria:ensure-list tag)
          url
          (serapeum:tokens preview)))

(defun get-readinglist-info (bookmarks)
  (mapcar (serapeum:juxt
           (fw.lu:op
             (fw.lu:pick '("URIDictionary" "title")
                         _))
           (fw.lu:op
             (fw.lu:pick '("URLString")
                         _))
           (fw.lu:op
             (plump:decode-entities
              (coerce (fw.lu:pick '("ReadingList" "PreviewText")
                                  _)
                      'simple-string)
              t))
           (fw.lu:op
             (fw.lu:may
               (slugify
                (fw.lu:pick '("ReadingListNonSync" "siteName")
                            _)))))
          (gethash "Children"
                   (car
                    (select-child bookmarks
                                  "com.apple.ReadingList")))))

(defun slugify (s)
  (cl-ppcre:regex-replace-all "\\s+"
                              (string-downcase s)
                              "_"))

(defun select-child (d title)
  (flet ((get-title (h)
           (equal (gethash "Title" h)
                  title)))
    (fw.lu:let-each (:be *)
      (gethash "Children" d)
      (remove-if-not #'get-title *))))
