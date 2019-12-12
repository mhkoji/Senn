(defpackage :hachee.kkc.build.file
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:export :file->sentences
           :sentence-words))
(in-package :hachee.kkc.build.file)

(defvar +external-format+ :utf-8)

(defstruct sentence line)

(defmacro with-each-line ((line filename) &body body)
  (with-gensyms (in line-count)
    `(with-open-file (,in ,filename
                          :external-format +external-format+)
       (loop for ,line = (read-line ,in nil nil)
             for ,line-count from 1
             while ,line do (progn ,@body)))))

(defun file->sentences (pathname)
  (let ((sentences nil))
    (with-each-line (line pathname)
      (push (make-sentence :line line) sentences))
    (nreverse sentences)))

(defun sentence-words (sentence)
  (mapcar (lambda (form-pron-str)
            ;; A/a-/B/b => AB/ab
            (let ((form-pron-list
                   (mapcar (lambda (form-pron-part-str)
                             (let ((split (cl-ppcre:split
                                           "/"
                                           form-pron-part-str)))
                               (list (or (first split) "")
                                     (or (second split) ""))))
                           (cl-ppcre:split "-" form-pron-str))))
              (hachee.kkc.word:make-word
               :form (format nil "~{~A~}" (mapcar #'first
                                                  form-pron-list))
               :pron (format nil "~{~A~}" (mapcar #'second
                                                  form-pron-list)))))
          (cl-ppcre:split " " (sentence-line sentence))))
