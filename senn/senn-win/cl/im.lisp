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


(defmethod transit ((im im) (s converting) key)
  (cond ((senn.win.keys:enter-p key)
         (make-committed :input (converting-current-input s)))
        (t
         s)))


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
        ((senn.win.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (let ((words (senn.kkc:convert (im-kkc im) pron)))
             (let ((segments (mapcar (lambda (w)
                                       (senn.segment:make-segment
                                        :pron (senn.kkc:word-pron w)
                                        :forms (list (senn.kkc:word-form w))
                                        :has-more-forms-p t
                                        :current-index 0))
                                     words)))
               (make-converting :segments segments
                                :pronunciation pron)))))
        ((senn.win.keys:enter-p key)
         (let ((input (senn.buffer:buffer-string (editing-buffer s))))
           (make-committed :input
                           (if (string= input "")
                               +crlf+
                               input))))
        (t
         s)))
