(defpackage :hachee.kkc.file
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:export :sentence-units
           :file->string-sentences))
(in-package :hachee.kkc.file)

(defvar +external-format+ :utf-8)

(defstruct sentence units)

(defmacro with-each-line ((line filename) &body body)
  (with-gensyms (in line-count)
    `(with-open-file (,in ,filename
                          :external-format +external-format+)
       (loop for ,line = (read-line ,in nil nil)
             for ,line-count from 1
             while ,line do (progn ,@body)))))

(defun file->string-sentences (pathname)
  (let ((sentences nil))
    (with-each-line (line pathname)
      (let ((unit-strs (cl-ppcre:split " " line)))
        (push (make-sentence :units unit-strs) sentences)))
    (nreverse sentences)))
