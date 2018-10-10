(defpackage :reading-list-reader
  (:use :cl )
  (:export ))
(in-package :reading-list-reader)

(serapeum:eval-always
  (named-readtables:in-readtable :objc-readtable))

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

(defparameter *reading-list-location* "Library/Safari/Bookmarks.plist")
(defun get-bookmark-filename ()
  (uiop:native-namestring
   (merge-pathnames *reading-list-location*
                    (truename "~/"))))

(defun translate-plist (fn)
  (objc-runtime.data-extractors:extract-from-objc
   (objc-runtime.data-extractors:get-plist fn)))

(defun make-org-file (s reading-list-info)
  (format s "~&* Safari Reading List~%")
  (serapeum:mapply (serapeum:partial 'make-org-entry s)
                   reading-list-info))

(defun make-org-entry (s date title url preview tag)
  (format s "~&** ~a (~a) :~{~a:~}~% ~a~2% ~{~<~% ~1,80:;~a~> ~}~2%"
          title
          (local-time:format-timestring nil date
                                        :format local-time:+rfc3339-format/date-only+)
          (alexandria:ensure-list tag)
          url
          (serapeum:tokens preview)))

(defun get-readinglist-info (bookmarks)
  (sort (mapcar 'extract-link-info
                (gethash "Children"
                         (car
                          (select-child bookmarks
                                        "com.apple.ReadingList"))))
        'local-time:timestamp>
        :key 'car))

(defun extract-link-info (link)
  (list (local-time:parse-rfc3339-timestring (or (fw.lu:pick '("ReadingList" "DateAdded") link)
                                                 (fw.lu:pick '("ReadingList" "DateLastViewed") link)
                                                 (fw.lu:pick '("ReadingListNonSync" "DateLastFetched") link)
                                                 (local-time:now)))
        (fw.lu:pick '("URIDictionary" "title") link)
        (fw.lu:pick '("URLString") link)
        (plump:decode-entities (coerce (fw.lu:pick '("ReadingList" "PreviewText") link) 'simple-string) t)
        (fw.lu:may (slugify (fw.lu:pick '("ReadingListNonSync" "siteName") link)))))

(defun main ()
  #+(and build sbcl)
  (progn (sb-ext:disable-debugger)
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "disable_lossage_handler"
                                 (function sb-alien:void))))
  (make-org-file *standard-output*
                 (get-readinglist-info 
                  (translate-plist 
                   (get-bookmark-filename)))))
