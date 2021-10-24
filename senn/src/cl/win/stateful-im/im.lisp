(defpackage :senn.win.stateful-im.im
  (:use :cl)
  (:import-from :senn.win.stateful-im
                :editing
                :make-editing
                :editing-buffer
                :converting
                :make-converting
                :converting-current-input
                :converting-current-segment
                :converting-move-curret-segment
                :editing-view
                :converting-view
                :committed-view))
(in-package :senn.win.stateful-im.im)

(defmethod senn.win.stateful-im:make-initial-state ((ime senn.im:ime))
  (make-editing))

(defgeneric can-process (ime state key))

(defmethod senn.win.stateful-im:can-process ((ime senn.im:ime) state key)
  (format nil "~A~%" (if (can-process ime state key) 1 0)))

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defun move-segment-form-index! (seg diff ime)
  (senn.im:append-candidates ime seg)
  (senn.segment:try-move-cursor-pos! seg diff))

(defun char-p (k)
  (<= (char-code #\A) (senn.win.keys:key-code k) (char-code #\Z)))

(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defmethod senn.win.stateful-im:process-input
    ((ime senn.im:ime) (s converting)
     (key senn.win.keys:key))
  (cond ((senn.win.keys:enter-p key)
         (let ((editing (make-editing))
               (view (committed-view (converting-current-input s))))
           (list editing view)))

        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         (list s (converting-view t s)))

        ((senn.win.keys:up-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         (list s (converting-view t s)))

        ((senn.win.keys:left-p key)
         (converting-move-curret-segment s -1)
         (list s (converting-view t s)))

        ((senn.win.keys:right-p key)
         (converting-move-curret-segment s +1)
         (list s (converting-view t s)))

        (t
         (list s (converting-view nil s)))))

(defmethod can-process ((ime senn.im:ime) (s converting)
                        (key senn.win.keys:key))
  (cond ((senn.win.keys:enter-p key) t)
        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         t)
        ((senn.win.keys:up-p key) t)
        ((senn.win.keys:left-p key) t)
        ((senn.win.keys:right-p key) t)
        (t nil)))

(defmethod senn.win.stateful-im:process-input
    ((ime senn.im:ime) (s editing)
     (key senn.win.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                ;; to lower case by adding #x20
                (code-char (+ #x20 (senn.win.keys:key-code key)))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s)
                                          char-lower-case)))
         (list s (editing-view t s)))

        ((senn.win.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (cond ((string= pron "")
                  ;; IMEが文字を削除していない -> OSに文字を削除してもらう
                  (list s (editing-view nil s)))
                 (t
                  (setf (editing-buffer s)
                        (senn.buffer:delete-char (editing-buffer s)))
                  (list s (editing-view t s))))))

        ((senn.win.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (if (string= pron "")
               (list s (editing-view nil s))
               (let ((segments (senn.im:convert ime pron)))
                 (let ((converting (make-converting
                                    :segments segments
                                    :pronunciation pron)))
                   (let ((view (converting-view t converting)))
                     (list converting view)))))))

        ((senn.win.keys:enter-p key)
         (let ((buffer (editing-buffer s))
               (editing (make-editing)))
           (let ((view (committed-view
                        (if (buffer-empty-p buffer)
                            +crlf+
                            (senn.buffer:buffer-string buffer)))))
             (list editing view))))

        (t
         (list s (editing-view nil s)))))

(defmethod can-process ((ime senn.im:ime) (s editing)
                        (key senn.win.keys:key))
  (cond ((char-p key) t)
        ((senn.win.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           ;; IMEが文字を削除していない -> OSに文字を削除してもらう
           (not (string= pron ""))))
        ((senn.win.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (not (string= pron ""))))
        ((senn.win.keys:enter-p key) t)
        (t nil)))
