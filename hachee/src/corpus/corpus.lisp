(defpackage :hachee.corpus
  (:use :cl)
  (:export :do-lines
           :line-units))
(in-package :hachee.corpus)

(defvar +external-format+ :utf-8)

(defun string-to-units (string make-unit)
  (mapcar (lambda (form-pron-str)
            ;; A/a-B/b => AB/ab
            (let ((form-pron-list
                   (mapcar (lambda (form-pron-part-str)
                             (let ((split (cl-ppcre:split
                                           "/"
                                           form-pron-part-str)))
                               (list (or (first split) "")
                                     (or (second split) ""))))
                           (cl-ppcre:split "-" form-pron-str))))
              (funcall make-unit
                       (format nil "~{~A~}"
                               (mapcar #'first form-pron-list))
                       (format nil "~{~A~}"
                               (mapcar #'second form-pron-list)))))
          (cl-ppcre:split " " string)))

(defmacro do-lines ((line pathname) &body body)
  `(with-open-file (in ,pathname
                       :external-format +external-format+)
     (loop for ,line = (read-line in nil nil)
           while ,line do (progn ,@body))))

(defun line-units (line make-unit)
  (string-to-units line make-unit))
