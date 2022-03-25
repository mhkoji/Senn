(defpackage :hachee.kkc.user-dict
  (:use :cl)
  (:export :entry-form
           :entry-pron
           :dict-entries
           :dict-size
           :read-file))
(in-package :hachee.kkc.user-dict)

(defstruct entry form pron)

(defstruct dict entries)

(defun dict-size (dict)
  (length (dict-entries dict)))

(defun read-file-internal (path)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (when (and (string/= line "")
                   (char/= (char line 0) #\#))
          (let ((cols (cl-ppcre:split "\\t" line)))
            (when (= (length cols) 2)
              (destructuring-bind (form pron) cols
                (setf (gethash form hash) pron)))))))
    (let ((entries nil))
      (maphash (lambda (form pron)
                 (push (make-entry :form form :pron pron)
                       entries))
               hash)
      (make-dict :entries entries))))

(defun read-file (path)
  (when (uiop/filesystem:file-exists-p path)
    (ignore-errors (read-file-internal path))))
