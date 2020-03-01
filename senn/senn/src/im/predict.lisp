(defpackage :senn.im.predict
  (:use :cl)
  (:export :prefix-predictor
           :load-prefix-dictionary))
(in-package :senn.im.predict)

(defclass prefix-predictor ()
  ((dictionary
    :initarg :dictionary
    :reader prefix-predictor-dictionary)))

(defmethod senn.im:predict append ((predictor prefix-predictor)
                                   (string string))
  (let ((dict (prefix-predictor-dictionary predictor)))
    (let ((words (senn.prefix-dictionary:lookup dict string)))
      (mapcar #'hachee.kkc.word:word-form
              (subseq words 0 (min 10 (length words)))))))

(defun load-prefix-dictionary (user-homedir-pathname)
  (with-open-file (s (merge-pathnames ".senn/prefix-dictionary.txt"
                                      user-homedir-pathname))
    (senn.prefix-dictionary:load-dictionary s)))
