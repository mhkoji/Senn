(defpackage :senn.win.ime.process-input
  (:use :cl :senn.win.ime)
  (:export :execute))
(in-package :senn.win.ime.process-input)

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defun move-segment-form-index! (seg diff ime)
  (senn.im:append-candidates ime seg)
  (senn.segment:try-move-cursor-pos! seg diff))

(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))


(defun converting-move-curret-segment (c diff)
  (let ((new-index (+ (converting-current-segment-index c) diff)))
    (when (<= 0 new-index (1- (length (converting-segments c))))
      (setf (converting-current-segment-index c) new-index)))
  c)

(defun converting-current-segment (c)
  (elt (converting-segments c)
       (converting-current-segment-index c)))

(defun converting-current-input (c)
  (format nil "~{~A~}"
          (mapcar #'senn.segment:segment-current-form
                  (converting-segments c))))

;;; view

;; May be viewed as the side-effect to the system by process-input
(defun editing-view (editing-state)
  (let ((string (senn.buffer:buffer-string
                 (editing-buffer editing-state))))
    (format nil "EDITING ~A" string)))

(defun converting-view (converting-state)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
           ("forms"
            (mapcar #'senn.segment:segment-current-form
                    (converting-segments converting-state)))
           ("cursor-form-index"
            (converting-current-segment-index converting-state))
           ("cursor-form"
            (let ((segment (converting-current-segment converting-state)))
              (jsown:new-js
               ("candidates"
                (if (senn.segment:segment-has-more-candidates-p segment)
                    nil
                    (senn.segment:segment-forms segment)))
               ("candidate-index"
                (senn.segment:segment-current-index segment)))))))))
    (format nil "CONVERTING ~A" json-string)))

(defun committed-view (string)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
            ("input" string)))))
    (format nil "COMMITTED ~A" json-string)))


;;; interface

(defgeneric execute (ime state mode key))

(defun result (state mode can-process view)
  (list state mode can-process view))

(defmethod execute ((ime senn.im:ime)
                    (s editing)
                    (mode (eql :direct))
                    (key senn.win.keys:key))
  (result s mode t (editing-view s)))

(defmethod execute ((ime senn.im:ime)
                    (s converting)
                    (mode (eql :direct))
                    (key senn.win.keys:key))
  (result s mode t (converting-view s)))

(defmethod execute ((ime senn.im:ime)
                    (s t)
                    (mode (eql :direct))
                    (key senn.win.keys:key))
  (result s mode nil nil))


(defmethod execute ((ime senn.im:ime)
                    (s converting)
                    (mode (eql :hiragana))
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:enter-p key)
         (let ((new-s (make-editing))
               (view (committed-view (converting-current-input s))))
           (result new-s mode t view)))

        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         (result s mode t (converting-view s)))

        ((senn.win.keys:up-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         (result s mode t (converting-view s)))

        ((senn.win.keys:left-p key)
         (converting-move-curret-segment s -1)
         (result s mode t (converting-view s)))

        ((senn.win.keys:right-p key)
         (converting-move-curret-segment s +1)
         (result s mode t (converting-view s)))

        (t
         (result s mode nil nil))))


(defun editing-insert-char (state char)
  (with-accessors ((buffer editing-buffer)) state
    (setf buffer (senn.buffer:insert-char buffer char))))

(defmethod execute ((ime senn.im:ime)
                    (s editing)
                    (mode (eql :hiragana))
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:oem-minus-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＝ #\ー))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-7-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\〜  #\＾))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-5-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｜ #\￥))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-3-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｀ #\＠))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-4-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｛ #\「))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-plus-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＋ #\；))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-1-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＊ #\：))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-6-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｝ #\」))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-comma-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＜ #\、))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-period-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＞ #\。))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-2-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\？ #\・))
         (result s mode t (editing-view s)))
        ((senn.win.keys:oem-102-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＿ #\￥))
         (result s mode t (editing-view s)))

        ((senn.win.keys:number-p key)
         (editing-insert-char
          s (let ((code (senn.win.keys:key-code key)))
              (if (senn.win.keys:key-shift-p key)
                  (cond ((= code (char-code #\0)) #\〜)
                        ((= code (char-code #\1)) #\！)
                        ((= code (char-code #\2))
                         #\RIGHT_DOUBLE_QUOTATION_MARK)
                        ((= code (char-code #\3)) #\＃)
                        ((= code (char-code #\4)) #\＄)
                        ((= code (char-code #\5)) #\％)
                        ((= code (char-code #\6)) #\＆)
                        ((= code (char-code #\7))
                         #\RIGHT_SINGLE_QUOTATION_MARK)
                        ((= code (char-code #\8)) #\（)
                        ((= code (char-code #\9)) #\）)
                        (t (code-char (+ #xFEE0 code))))
                  (code-char (+ #xFEE0 code)))))
         (result s mode t (editing-view s)))

        ((senn.win.keys:alphabet-p key)
         (editing-insert-char
          ;; to lower case by adding #x20
          s (code-char (+ #x20 (senn.win.keys:key-code key))))
         (result s mode t (editing-view s)))

        ((senn.win.keys:backspace-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (cond ((string= pron "")
                  ;; IMEが文字を削除していない -> OSに文字を削除してもらう
                  (result s mode nil (editing-view s)))
                 (t
                  (setf (editing-buffer s)
                        (senn.buffer:delete-char (editing-buffer s)))
                  (result s mode t (editing-view s))))))

        ((senn.win.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (if (string= pron "")
               (result s mode nil (editing-view s))
               (let ((segments (senn.im:convert ime pron)))
                 (let ((converting (make-converting
                                    :segments segments
                                    :pronunciation pron)))
                   (let ((view (converting-view converting)))
                     (result converting mode t view)))))))

        ((senn.win.keys:enter-p key)
         (let ((buffer (editing-buffer s))
               (editing (make-editing)))
           (let ((view (committed-view
                        (if (buffer-empty-p buffer)
                            +crlf+
                            (senn.buffer:buffer-string buffer)))))
             (result editing mode t view))))

        (t
         (result s mode nil nil))))
