(defpackage :senn.win.im.process-input
  (:use :cl)
  (:export :execute))
(in-package :senn.win.im.process-input)

;;; view

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defun editing-view (inputting-state)
  (let ((json (jsown:new-js
                ("input"
                 (senn.im.inputting:state-buffer-string inputting-state))
                ("predictions"
                 (senn.im.inputting:state-predictions inputting-state)))))
    (format nil "EDITING ~A" (jsown:to-json json))))

(defun converting-view (converting-state)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
           ("forms"
            (mapcar #'senn.im.converting:segment-cursor-pos-form
                    (senn.im.converting:state-segments converting-state)))
           ("cursor-form-index"
            (senn.im.converting:state-current-segment-index
             converting-state))
           ("cursor-form"
            (let ((segment (senn.im.converting:current-segment
                            converting-state)))
              (jsown:new-js
               ("candidates"
                (if (senn.im.converting:segment-has-more-candidates-p
                     segment)
                    nil
                    (senn.im.converting:segment-forms segment)))
                ("candidate-index"
                 (senn.im.converting:segment-current-index segment)))))))))
    (format nil "CONVERTING ~A" json-string)))

(defun committed-view (string)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
            ("input" string)))))
    (format nil "COMMITTED ~A" json-string)))


;;; interface

(defgeneric execute (state mode ime key))

(defun result (can-process view &key state committed-segments)
  (list can-process view
        :state state
        :committed-segments committed-segments))

(defmethod execute ((s senn.im.inputting:state) (mode (eql :direct))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key))
  (result t (editing-view s)))

(defmethod execute ((s senn.im.converting:state) (mode (eql :direct))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key))
  (result t (converting-view s)))

(defmethod execute ((s t) (mode (eql :direct))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key))
  (result nil nil))


(defmethod execute ((s senn.im.converting:state) (mode (eql :hiragana))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:enter-p key)
         (let ((view (committed-view
                      (senn.im.converting:current-input s)))
               (segs (senn.im.converting:state-segments s)))
           (result t view
                   :state (senn.im.inputting:make-state)
                   :committed-segments segs)))

        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         (senn.im.converting:current-segment-candidates-move! s +1 ime)
         (result t (converting-view s) :state s))

        ((senn.win.keys:up-p key)
         (senn.im.converting:current-segment-candidates-move! s -1 ime)
         (result t (converting-view s) :state s))

        ((senn.win.keys:left-p key)
         (senn.im.converting:current-segment-move! s -1)
         (result t (converting-view s) :state s))

        ((senn.win.keys:right-p key)
         (senn.im.converting:current-segment-move! s +1)
         (result t (converting-view s) :state s))

        (t
         (result nil nil))))


(defmethod execute ((s senn.im.inputting:state) (mode (eql :hiragana))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:oem-minus-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＝ #\ー) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-7-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\〜  #\＾) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-5-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\｜ #\￥) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-3-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\｀ #\＠) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-4-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\｛ #\「) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-plus-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＋ #\；) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-1-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＊ #\：) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-6-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\｝ #\」) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-comma-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＜ #\、) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-period-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＞ #\。) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-2-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\？ #\・) ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-102-p key)
         (senn.im.inputting:insert-char!
          s (if (senn.win.keys:key-shift-p key) #\＿ #\￥) ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:number-p key)
         (senn.im.inputting:insert-char!
          s (let ((code (senn.win.keys:key-code key)))
              (if (senn.win.keys:key-shift-p key)
                  (cond ((= code (char-code #\0)) #\〜)
                        ((= code (char-code #\1)) #\！)
                        ((= code (char-code #\2)) #\U+201D)
                        ((= code (char-code #\3)) #\＃)
                        ((= code (char-code #\4)) #\＄)
                        ((= code (char-code #\5)) #\％)
                        ((= code (char-code #\6)) #\＆)
                        ((= code (char-code #\7)) #\U+2019)
                        ((= code (char-code #\8)) #\（)
                        ((= code (char-code #\9)) #\）)
                        (t (code-char (+ #xFEE0 code))))
                  (code-char (+ #xFEE0 code))))
          ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:alphabet-p key)
         (senn.im.inputting:insert-char!
          ;; to lower case by adding #x20
          s (code-char (+ #x20 (senn.win.keys:key-code key))) ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:backspace-p key)
         (if (senn.im.inputting:delete-char! s ime)
             (result t (editing-view s) :state s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (result nil nil)))

        ((senn.win.keys:space-p key)
         (if (senn.im.inputting:state-buffer-empty-p s)
             (result nil nil)
             (let ((new-state
                    (senn.im.converting:convert
                     ime
                     (senn.im.inputting:state-buffer-get-pron s))))
               (result t (converting-view new-state) :state new-state))))

        ((senn.win.keys:enter-p key)
         (let ((view (committed-view
                      (if (senn.im.inputting:state-buffer-empty-p s)
                          +crlf+
                          (senn.im.inputting:state-buffer-string s)))))
           (result t view :state (senn.im.inputting:make-state))))

        (t
         (result nil nil))))
