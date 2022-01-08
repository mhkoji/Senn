(defpackage :senn.win.im.process-input
  (:use :cl :senn.win.im)
  (:export :execute))
(in-package :senn.win.im.process-input)

(defvar +crlf+
  (format nil "~A~A" #\Return #\Newline))

(defun move-segment-form-index! (seg diff ime)
  (senn.im.ime:segment-append-candidates! ime seg)
  (senn.im.segment:try-move-cursor-pos! seg diff))

(defun buffer-empty-p (buffer)
  (string= (senn.im.buffer:buffer-string buffer) ""))


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
          (mapcar #'senn.im.segment:segment-current-form
                  (converting-segments c))))

;;; view

;; May be viewed as the side-effect to the system by process-input
(defun editing-view (editing-state)
  (let ((json (jsown:new-js
                ("input" (senn.im.buffer:buffer-string
                          (editing-buffer editing-state)))
                ("predictions" (editing-predictions editing-state)))))
    (format nil "EDITING ~A" (jsown:to-json json))))

(defun converting-view (converting-state)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
           ("forms"
            (mapcar #'senn.im.segment:segment-current-form
                    (converting-segments converting-state)))
           ("cursor-form-index"
            (converting-current-segment-index converting-state))
           ("cursor-form"
            (let ((segment (converting-current-segment converting-state)))
              (jsown:new-js
               ("candidates"
                (if (senn.im.segment:segment-has-more-candidates-p segment)
                    nil
                    (senn.im.segment:segment-forms segment)))
               ("candidate-index"
                (senn.im.segment:segment-current-index segment)))))))))
    (format nil "CONVERTING ~A" json-string)))

(defun committed-view (string)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
            ("input" string)))))
    (format nil "COMMITTED ~A" json-string)))


;;; interface

(defgeneric execute (ime state mode key))

(defun result (can-process view &key state committed-segments)
  (list can-process view
        :state state
        :committed-segments committed-segments))

(defmethod execute ((ime senn.im.ime:ime) (s editing) (mode (eql :direct))
                    (key senn.win.keys:key))
  (result t (editing-view s)))

(defmethod execute ((ime senn.im.ime:ime) (s converting) (mode (eql :direct))
                    (key senn.win.keys:key))
  (result t (converting-view s)))

(defmethod execute ((ime senn.im.ime:ime) (s t) (mode (eql :direct))
                    (key senn.win.keys:key))
  (result nil nil))


(defmethod execute ((ime senn.im.ime:ime) (s converting) (mode (eql :hiragana))
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:enter-p key)
         (let ((view (committed-view (converting-current-input s)))
               (segs (converting-segments s)))
           (result t view
                   :state (make-editing)
                   :committed-segments segs)))

        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         (result t (converting-view s) :state s))

        ((senn.win.keys:up-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         (result t (converting-view s) :state s))

        ((senn.win.keys:left-p key)
         (converting-move-curret-segment s -1)
         (result t (converting-view s) :state s))

        ((senn.win.keys:right-p key)
         (converting-move-curret-segment s +1)
         (result  t (converting-view s) :state s))

        (t
         (result nil nil))))


(defun editing-insert-char (state char)
  (with-accessors ((buffer editing-buffer)) state
    (setf buffer (senn.im.buffer:insert-char buffer char))))

(defun editing-update-predictions (state ime)
  (let ((input (senn.im.buffer:buffer-string (editing-buffer state))))
    (setf (editing-predictions state) (senn.im.ime:predict ime input))))

(defmethod execute ((ime senn.im.ime:ime) (s editing) (mode (eql :hiragana))
                    (key senn.win.keys:key))
  (cond ((senn.win.keys:oem-minus-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＝ #\ー))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-7-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\〜  #\＾))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-5-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｜ #\￥))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-3-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｀ #\＠))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-4-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｛ #\「))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-plus-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＋ #\；))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-1-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＊ #\：))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-6-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\｝ #\」))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-comma-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＜ #\、))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-period-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＞ #\。))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-2-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\？ #\・))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))
        ((senn.win.keys:oem-102-p key)
         (editing-insert-char
          s (if (senn.win.keys:key-shift-p key) #\＿ #\￥))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:number-p key)
         (editing-insert-char
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
                  (code-char (+ #xFEE0 code)))))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:alphabet-p key)
         (editing-insert-char
          ;; to lower case by adding #x20
          s (code-char (+ #x20 (senn.win.keys:key-code key))))
         (editing-update-predictions s ime)
         (result t (editing-view s) :state s))

        ((senn.win.keys:backspace-p key)
         (let ((pron (senn.im.buffer:buffer-string (editing-buffer s))))
           (if (string/= pron "")
               (progn
                 (with-accessors ((buffer editing-buffer)) s
                   (setf buffer (senn.im.buffer:delete-char buffer)))
                 (editing-update-predictions s ime)
                 (result t (editing-view s) :state s))
               ;; IMEが文字を削除していない -> OSに文字を削除してもらう
               (result nil nil))))

        ((senn.win.keys:space-p key)
         (let ((pron (senn.im.buffer:buffer-string (editing-buffer s))))
           (if (string/= pron "")
               (let ((segments (senn.im.ime:convert ime pron)))
                 (let ((converting (make-converting
                                    :segments segments
                                    :pronunciation pron)))
                   (let ((view (converting-view converting)))
                     (result t view :state converting))))
               (result nil nil))))

        ((senn.win.keys:enter-p key)
         (with-accessors ((buffer editing-buffer)) s
           (let ((view (committed-view
                        (if (buffer-empty-p buffer)
                            +crlf+
                            (senn.im.buffer:buffer-string buffer)))))
             (result t view :state (make-editing)))))

        (t
         (result nil nil))))
