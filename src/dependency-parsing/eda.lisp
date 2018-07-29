(defpackage :hachee.dependency-parsing.eda
  (:use :cl :hachee.dependency-parsing)
  (:export :read-from-stream))
(in-package :hachee.dependency-parsing.eda)

(defun read-from-stream (stream)
  (let ((first-line (loop for line = (read-line stream nil nil)
                      when (or (null line) (string/= line ""))
                        return line)))
    (cond ((null first-line) nil)
          ((cl-ppcre:scan "^ID=" first-line)
           (let ((rows (loop for line = (read-line stream nil nil)
                             while (and (stringp line) (string/= line ""))
                         collect
                            (destructuring-bind
                                  (id head form postag cluster . rest)
                                (cl-ppcre:split "\\s+" line)
                              (declare (ignore id rest))
                              (setq head (parse-integer head
                                                        :junk-allowed t))
                              (make-row :head head
                                        :form form
                                        :postag postag
                                        :cluster cluster)))))
             (make-sentence :rows (cons +root+ rows) :id first-line)))
          (t (error "Error while reading sentences.")))))
