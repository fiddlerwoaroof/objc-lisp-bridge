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
  (mapcar 'extract-link-info
          (gethash "Children"
                   (car
                    (select-child bookmarks
                                  "com.apple.ReadingList")))))

(defun extract-link-info (link)
  (list (fw.lu:pick '("URIDictionary" "title") link)
        (fw.lu:pick '("URLString") link)
        (plump:decode-entities (coerce (fw.lu:pick '("ReadingList" "PreviewText") link) 'simple-string) t)
        (fw.lu:may (slugify (fw.lu:pick '("ReadingListNonSync" "siteName") link)))))

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
