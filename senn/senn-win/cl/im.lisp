(defpackage :senn.win.im
  (:use :cl :senn.win.states)
  (:export :make-im
           :input))
(in-package :senn.win.im)

(defstruct im kkc)

(defgeneric input (im state key))

(defun char-p (k)
  (<= (char-code #\A)
      (senn.win.keys:key-code k)
      (char-code #\Z)))

(defmethod input ((im im) (s editing) (key senn.win.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                (code-char (+ #x20 ;; to lower case
                              (senn.win.keys:key-code key)))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s)
                                          char-lower-case)))
         s)
        ((senn.win.keys:enter-p key)
         (make-committed :input (senn.buffer:buffer-string
                                 (editing-buffer s))))
        (t
         s)))
