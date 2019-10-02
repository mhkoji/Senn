(defpackage :senn.win.im
  (:use :cl :senn.win.states)
  (:export :transit)
  (:import-from :senn.im :ime))
(in-package :senn.win.im)

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defgeneric transit (ime state key))

(defmethod transit ((ime ime) (s committed) key)
  (transit ime (make-editing) key))


(defun move-segment-form-index! (seg diff ime)
  (labels ((get-forms (pron)
             (senn.im:lookup-forms ime pron)))
    (senn.segment:append-forms! seg #'get-forms))
  (senn.segment:try-move-cursor-pos! seg diff))

(defmethod transit ((ime ime) (s converting) key)
  (cond ((senn.win.keys:enter-p key)
         (make-committed :input (converting-current-input s)))
        ((or (senn.win.keys:space-p key)
             (senn.win.keys:up-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         s)
        ((senn.win.keys:down-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         s)
        ((senn.win.keys:left-p key)
         (converting-move-curret-segment s -1)
         s)
        ((senn.win.keys:right-p key)
         (converting-move-curret-segment s +1)
         s)
        (t
         s)))


(defun char-p (k)
  (<= (char-code #\A)
      (senn.win.keys:key-code k)
      (char-code #\Z)))

(defmethod transit ((ime ime) (s editing) (key senn.win.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                (code-char (+ #x20 ;; to lower case
                              (senn.win.keys:key-code key)))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s)
                                          char-lower-case)))
         s)
        ((senn.win.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (cond ((string= pron "") ;; Should not reach here
                  s)
                 (t
                  (setf (editing-buffer s)
                        (senn.buffer:delete-char (editing-buffer s)))
                  s))))
        ((senn.win.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (if (string= pron "")
               s
               (let ((segments (senn.im:convert ime pron)))
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
