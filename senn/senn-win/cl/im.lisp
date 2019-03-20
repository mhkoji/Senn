(defpackage :senn.win.im
  (:use :cl :senn.win.states)
  (:export :make-im
           :transit))
(in-package :senn.win.im)

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defstruct im kkc)

(defgeneric transit (im state key))

(defmethod transit ((im im) (s committed) key)
  (transit im (make-editing) key))


(defun char-p (k)
  (<= (char-code #\A)
      (senn.win.keys:key-code k)
      (char-code #\Z)))

(defmethod transit ((im im) (s editing) (key senn.win.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                (code-char (+ #x20 ;; to lower case
                              (senn.win.keys:key-code key)))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s)
                                          char-lower-case)))
         s)
        ((senn.win.keys:enter-p key)
         (let ((input (senn.buffer:buffer-string (editing-buffer s))))
           (make-committed :input
                           (if (string= input "")
                               +crlf+
                               input))))
        (t
         s)))
