(defpackage :senn.im.predict.prefix
  (:use :cl)
  (:export :predictor
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

(defclass predictor ()
  ((dictionary
    :initarg :predictor-prefix-dictionary
    :reader predictor-prefix-dictionary)))

(defmethod senn.im.predict:execute append ((mixin predictor) (string string))
  (predict (predictor-prefix-dictionary mixin) string))
