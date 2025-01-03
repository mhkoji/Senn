(defpackage :hachee.kkc.impl.lm.build.file
  (:use :cl)
  (:export :with-sentence-reader
           :file->sentences
           :sentence-units))
(in-package :hachee.kkc.impl.lm.build.file)

(defvar +external-format+ :utf-8)

(defstruct sentence line)

(defmacro with-sentence-reader ((read-fn filename) &body body)
  `(with-open-file (in ,filename
                      :external-format +external-format+)
     (labels ((,read-fn ()
                (let ((line (read-line in nil nil)))
                  (when line
                    (make-sentence :line line)))))
       (progn ,@body))))

(defun file->sentences (pathname)
  (with-sentence-reader (read-sentence pathname)
    (loop for sentence = (read-sentence)
          while sentence collect sentence)))

(defun sentence-units (sentence)
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
              (hachee.kkc.impl.lm.unit:make-unit
               :form (format nil "~{~A~}" (mapcar #'first
                                                  form-pron-list))
               :pron (format nil "~{~A~}" (mapcar #'second
                                                  form-pron-list)))))
          (cl-ppcre:split " " (sentence-line sentence))))
