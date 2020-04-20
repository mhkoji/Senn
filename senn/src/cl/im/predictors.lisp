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
      (mapcar #'hachee.kkc.dictionary:unit-form
              (subseq words 0 (min 10 (length words)))))))

(defun load-prefix-dictionary (senn-homedir-pathname)
  (let ((path (merge-pathnames "prefix-dictionary.txt"
                               senn-homedir-pathname)))
    (if (cl-fad:file-exists-p path)
        (with-open-file (s path)
          (senn.prefix-dictionary:load-dictionary s))
        (senn.prefix-dictionary:make-dictionary))))
