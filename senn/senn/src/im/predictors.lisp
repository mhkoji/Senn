(defpackage :senn.im.predictors
  (:use :cl)
  (:export :katakana
           :prefix
           :load-prefix-dictionary))
(in-package :senn.im.predictors)

(defclass katakana () ())

(defmethod senn.im:predict append ((predictor katakana)
                                   (string string))
  (list (hachee.ja:hiragana->katakana string)))


(defclass prefix ()
  ((dictionary
    :initarg :dictionary
    :reader prefix-dictionary)))

(defmethod senn.im:predict append ((predictor prefix)
                                   (string string))
  (let ((dict (prefix-dictionary predictor)))
    (let ((words (senn.prefix-dictionary:lookup dict string)))
      (mapcar #'hachee.kkc.word:word-form
              (subseq words 0 (min 10 (length words)))))))

(defun load-prefix-dictionary (user-homedir-pathname)
  (with-open-file (s (merge-pathnames ".senn/prefix-dictionary.txt"
                                      user-homedir-pathname))
    (senn.prefix-dictionary:load-dictionary s)))
