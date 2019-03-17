(defpackage :senn.win.im
  (:use :cl)
  (:export :make-im
           :input))
(in-package :senn.win.im)

(defstruct im kkc)

(defgeneric input (im state key))

(defun char-p (k)
  (<= (char-code #\A)
      (senn.win.keys:key-code k)
      (char-code #\Z)))

(defmethod input ((im im) buffer (key senn.win.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                (code-char (+ #x20 ;; to lower case
                              (senn.win.keys:key-code key)))))
           (setf buffer
                 (senn.buffer:insert-char buffer
                                          char-lower-case))))
        (t
         buffer)))
