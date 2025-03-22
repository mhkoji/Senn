(defpackage :senn.win.im.process-input
  (:use :cl)
  (:export :dry-run))
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

(defun result (view &key state committed-segments)
  (list view
        :state state
        :committed-segments committed-segments))

(defgeneric dry-run (state ime key mode))

(defmethod dry-run ((s (eql :direct-state))
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key)
                    (mode t))
  (assert (senn.win.im.input-mode:mode=
           mode
           senn.win.im.input-mode:+direct+))
  nil)

(defmethod dry-run ((s senn.im.converting:state)
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key)
                    (mode t))
  (cond ((senn.win.keys:enter-p key)
         (lambda ()
           (let ((view (committed-view
                        (senn.im.converting:current-input s)))
                 (segs (senn.im.converting:state-segments s))
                 (state (senn.win.im.input-mode:mode-case mode
                          (:hiragana (senn.im.inputting:make-state))
                          (:direct   nil))))
             (result view
                     :state state
                     :committed-segments segs))))

        ((or (senn.win.keys:space-p key)
             (senn.win.keys:down-p key))
         (lambda ()
           (senn.im.converting:current-segment-candidates-move! s +1 ime)
           (result (converting-view s) :state s)))

        ((senn.win.keys:up-p key)
         (lambda ()
           (senn.im.converting:current-segment-candidates-move! s -1 ime)
           (result (converting-view s) :state s)))

        ((senn.win.keys:left-p key)
         (lambda ()
           (senn.im.converting:current-segment-move! s -1)
           (result (converting-view s) :state s)))

        ((senn.win.keys:right-p key)
         (lambda ()
           (senn.im.converting:current-segment-move! s +1)
           (result (converting-view s) :state s)))

        (t nil)))

(defun oem-key->char (mode key)
  (let ((shift-p (senn.win.keys:key-shift-p key)))
    (senn.win.im.input-mode:mode-case mode
      (:hiragana
       (cond ((senn.win.keys:oem-minus-p key)  (if shift-p #\＝ #\ー))
             ((senn.win.keys:oem-7-p key)      (if shift-p #\〜 #\＾))
             ((senn.win.keys:oem-5-p key)      (if shift-p #\｜ #\￥))
             ((senn.win.keys:oem-3-p key)      (if shift-p #\｀ #\＠))
             ((senn.win.keys:oem-4-p key)      (if shift-p #\｛ #\「))
             ((senn.win.keys:oem-plus-p key)   (if shift-p #\＋ #\；))
             ((senn.win.keys:oem-1-p key)      (if shift-p #\＊ #\：))
             ((senn.win.keys:oem-6-p key)      (if shift-p #\｝ #\」))
             ((senn.win.keys:oem-comma-p key)  (if shift-p #\＜ #\、))
             ((senn.win.keys:oem-period-p key) (if shift-p #\＞ #\。))
             ((senn.win.keys:oem-2-p key)      (if shift-p #\？ #\・))
             ((senn.win.keys:oem-102-p key)    (if shift-p #\＿ #\￥))))
      (:direct
       (cond ((senn.win.keys:oem-minus-p key)  (if shift-p #\= #\-))
             ((senn.win.keys:oem-7-p key)      (if shift-p #\~ #\^))
             ((senn.win.keys:oem-5-p key)      (if shift-p #\| #\\))
             ((senn.win.keys:oem-3-p key)      (if shift-p #\` #\@))
             ((senn.win.keys:oem-4-p key)      (if shift-p #\{ #\[))
             ((senn.win.keys:oem-plus-p key)   (if shift-p #\+ #\;))
             ((senn.win.keys:oem-1-p key)      (if shift-p #\* #\:))
             ((senn.win.keys:oem-6-p key)      (if shift-p #\} #\]))
             ((senn.win.keys:oem-comma-p key)  (if shift-p #\< #\,))
             ((senn.win.keys:oem-period-p key) (if shift-p #\> #\.))
             ((senn.win.keys:oem-2-p key)      (if shift-p #\? #\/))
             ((senn.win.keys:oem-102-p key)    (if shift-p #\_ #\\)))))))

(defun number-key->char (mode key)
  (when (senn.win.keys:number-p key)
    (let ((code (senn.win.keys:key-code key))
          (shift-p (senn.win.keys:key-shift-p key)))
      (senn.win.im.input-mode:mode-case mode
        (:hiragana
         (if shift-p
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
        (:direct
         (if shift-p
             (cond ((= code (char-code #\0)) #\~)
                   ((= code (char-code #\1)) #\!)
                   ((= code (char-code #\2)) #\")
                   ((= code (char-code #\3)) #\#)
                   ((= code (char-code #\4)) #\$)
                   ((= code (char-code #\5)) #\%)
                   ((= code (char-code #\6)) #\&)
                   ((= code (char-code #\7)) #\')
                   ((= code (char-code #\8)) #\()
                   ((= code (char-code #\9)) #\))
                   (t (code-char code)))
             (code-char code)))))))

(defun alphabet-key->char (key)
  (when (senn.win.keys:alphabet-p key)
    ;; to lower case by adding #x20
    (code-char (+ #x20 (senn.win.keys:key-code key)))))

(defmethod dry-run ((s senn.im.inputting:state)
                    (ime senn.win.im:ime)
                    (key senn.win.keys:key)
                    (mode t))
  (block nil
    (let ((char (or (oem-key->char mode key)
                    (number-key->char mode key)
                    (alphabet-key->char key))))
      (when char
        (return (lambda ()
	          (senn.win.im.input-mode:mode-case mode
	            (:hiragana
	             (senn.im.inputting:insert-char! s char ime))
	            (:direct
	             (senn.im.inputting:insert-char-direct! s char ime)))
                  (result (editing-view s) :state s)))))
    (when (senn.win.keys:backspace-p key)
      (return (if (senn.im.inputting:state-buffer-empty-p s)
                  (lambda ()
                    (senn.im.inputting:delete-char! s ime)
                    (result (editing-view s) :state s))
                  ;; IMEが文字を削除していない -> OSに文字を削除してもらう
                  nil)))
    (when (senn.win.keys:space-p key)
      (return (if (senn.im.inputting:state-buffer-empty-p s)
                  nil
                  (lambda ()
                    (let* ((pron (senn.im.inputting:state-buffer-get-pron s))
                           (new-state (senn.im.converting:convert ime pron)))
                      (result (converting-view new-state)
                              :state new-state))))))
    (when (senn.win.keys:enter-p key)
      (return (lambda ()
                (let ((view (committed-view
                             (if (senn.im.inputting:state-buffer-empty-p s)
                                 +crlf+
                                 (senn.im.inputting:state-buffer-string s))))
                      (state (senn.win.im.input-mode:mode-case mode
                               (:hiragana (senn.im.inputting:make-state))
                               (:direct   :direct-state))))
                  (result view :state state)))))
    nil))

(defmethod senn.win.ime:process-input ((state t)
                                       (ime senn.win.im:ime)
                                       (key senn.win.keys:key)
                                       (mode t))
  (let ((fn (dry-run state ime key mode)))
    (when fn
      (funcall fn))))
