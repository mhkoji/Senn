(defpackage :senn.im.kkc-store.hachee.user-dict
  (:use :cl)
  (:export :dict
           :entry-form
           :entry-pron
           :entry
           :dict-entries
           :read-file))
(in-package :senn.im.kkc-store.hachee.user-dict)

(defun user-dict-pathname ()
  (merge-pathnames ".senn/user-dict.txt"
                   (user-homedir-pathname)))

;;;

(defstruct entry form pron)

(defstruct dict entries)

(defun read-file-internal (path)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (when (and (string/= line "")
                   (char/= (char line 0) #\#))
          (let ((cols (cl-ppcre:split " " line)))
            (when (= (length cols) 2)
              (destructuring-bind (form pron) cols
                (setf (gethash form hash) pron)))))))
    (let ((entries nil))
      (maphash (lambda (form pron)
                 (push (make-entry :form form :pron pron)
                       entries))
               hash)
      (make-dict :entries entries))))

(defun read-file ()
  (let ((path (user-dict-pathname)))
    (when (uiop/filesystem:file-exists-p path)
      (read-file-internal path))))
