(defpackage :senn.im.prefix-dictionary
  (:use :cl)
  (:export :predict
           :load-prefix-dictionary))
(in-package :senn.im.prefix-dictionary)

(defun predict (dict string)
  (let ((words (senn.prefix-dictionary:lookup dict string)))
    (mapcar #'hachee.kkc.dictionary:unit-form
            (subseq words 0 (min 10 (length words))))))

(defun load-prefix-dictionary (senn-homedir-pathname)
  (let ((path (merge-pathnames "prefix-dictionary.txt"
                               senn-homedir-pathname)))
    (if (cl-fad:file-exists-p path)
        (with-open-file (s path)
          (senn.prefix-dictionary:load-dictionary s))
        (senn.prefix-dictionary:make-dictionary))))
