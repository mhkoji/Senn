(defpackage :senn.im.predict.prefix
  (:use :cl)
  (:export :predict
           :load-dictionary))
(in-package :senn.im.predict.prefix)

(defun predict (dict string)
  (let ((words (senn.prefix-dictionary:lookup dict string)))
    (mapcar #'hachee.kkc.dictionary:unit-form
            (subseq words 0 (min 10 (length words))))))

(defun load-dictionary (senn-homedir-pathname)
  (let ((path (merge-pathnames "prefix-dictionary.txt"
                               senn-homedir-pathname)))
    (if (cl-fad:file-exists-p path)
        (with-open-file (s path)
          (senn.prefix-dictionary:load-dictionary s))
        (senn.prefix-dictionary:make-dictionary))))

;;;

(defclass predict ()
  ((dictionary
    :initarg :predict-prefix-dictionary
    :reader predict-prefix-dictionary)))

(defmethod senn.im.ime:predict append ((mixin predict) (string string))
  (predict (predict-prefix-dictionary mixin) string))
