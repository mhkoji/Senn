(defpackage :senn.win.input-processor
  (:use :cl
        :senn.win.input-processor.states)
  (:export :process-input
           :can-process)
  (:import-from :senn.im
                :ime))
(in-package :senn.win.input-processor)

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defun move-segment-form-index! (seg diff ime)
  (senn.im:append-candidates ime seg)
  (senn.segment:try-move-cursor-pos! seg diff))

(defun char-p (k)
  (<= (char-code #\A)
      (senn.win.input-processor.keys:key-code k)
      (char-code #\Z)))

(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defgeneric process-input (ime state key))

(defgeneric can-process-internal (ime state key))

(defmethod process-input ((ime ime) (s converting) key)
  (cond ((senn.win.input-processor.keys:enter-p key)
         (let ((editing (make-editing))
               (view (senn.win.input-processor.view:committed
                      (converting-current-input s))))
           (list editing view)))

        ((or (senn.win.input-processor.keys:space-p key)
             (senn.win.input-processor.keys:down-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         (list s (senn.win.input-processor.view:converting t s)))

        ((senn.win.input-processor.keys:up-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         (list s (senn.win.input-processor.view:converting t s)))

        ((senn.win.input-processor.keys:left-p key)
         (converting-move-curret-segment s -1)
         (list s (senn.win.input-processor.view:converting t s)))

        ((senn.win.input-processor.keys:right-p key)
         (converting-move-curret-segment s +1)
         (list s (senn.win.input-processor.view:converting t s)))

        (t
         (list s (senn.win.input-processor.view:converting nil s)))))

(defmethod can-process-internal ((ime ime) (s converting) key)
  (cond ((senn.win.input-processor.keys:enter-p key) t)
        ((or (senn.win.input-processor.keys:space-p key)
             (senn.win.input-processor.keys:down-p key))
         t)
        ((senn.win.input-processor.keys:up-p key) t)
        ((senn.win.input-processor.keys:left-p key) t)
        ((senn.win.input-processor.keys:right-p key) t)
        (t nil)))

(defmethod process-input ((ime ime) (s editing)
                          (key senn.win.input-processor.keys:key))
  (cond ((char-p key)
         (let ((char-lower-case
                (code-char
                 (+ #x20 ;; to lower case
                    (senn.win.input-processor.keys:key-code key)))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s)
                                          char-lower-case)))
         (list s (senn.win.input-processor.view:editing t s)))

        ((senn.win.input-processor.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (cond ((string= pron "")
                  ;; IMEが文字を削除していない -> OSに文字を削除してもらう
                  (list s (senn.win.input-processor.view:editing nil s)))
                 (t
                  (setf (editing-buffer s)
                        (senn.buffer:delete-char (editing-buffer s)))
                  (list s (senn.win.input-processor.view:editing t s))))))

        ((senn.win.input-processor.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (if (string= pron "")
               (list s (senn.win.input-processor.view:editing nil s))
               (let ((segments (senn.im:convert ime pron)))
                 (let ((converting (make-converting :segments segments
                                                    :pronunciation pron)))
                   (let ((view (senn.win.input-processor.view:converting
                                t converting)))
                     (list converting view)))))))

        ((senn.win.input-processor.keys:enter-p key)
         (let ((buffer (editing-buffer s))
               (editing (make-editing)))
           (let ((view (senn.win.input-processor.view:committed
                        (if (buffer-empty-p buffer)
                            +crlf+
                            (senn.buffer:buffer-string buffer)))))
             (list editing view))))

        (t
         (list s (senn.win.input-processor.view:editing nil s)))))

(defmethod can-process-internal ((ime ime) (s editing)
                                 (key senn.win.input-processor.keys:key))
  (cond ((char-p key) t)
        ((senn.win.input-processor.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           ;; IMEが文字を削除していない -> OSに文字を削除してもらう
           (not (string= pron ""))))
        ((senn.win.input-processor.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (not (string= pron ""))))
        ((senn.win.input-processor.keys:enter-p key) t)
        (t nil)))

(defun can-process (ime state key)
  (format nil "~A~%" (if (can-process-internal ime state key) 1 0)))
