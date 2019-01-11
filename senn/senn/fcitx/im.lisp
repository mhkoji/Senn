(defpackage :senn.fcitx.im
  (:use :cl :senn.fcitx.states)
  (:export :make-im
           :make-key
           :input))
(in-package :senn.fcitx.im)

(defstruct im kkc)

(defstruct key sym state)

(defgeneric input (im state key))


(defvar +space-key+ 32)
(defvar +backspace-key+ 65288)
(defvar +enter-key+ 65293)
(defvar +left-key+  65361)
(defvar +right-key+ 65363)

(defmethod input ((im im) (s committed) key)
  (input im (make-editing) key))


(defun committed (input)
  (list (make-committed :input input)
        (if (string= input "")
            :IRV_TO_PROCESS
            ;; 何らかの文字が確定された場合
            ;; エンターキーによる改行は無効化させる
            :IRV_DO_NOTHING)))

(defmethod input ((im im) (s converting) (key key))
  (let ((code (key-sym key)))
    (cond ((= code +space-key+)
           (senn.segment:append-forms!
            (converting-current-segment s)
            (lambda (pron)
              (let ((words (senn.kkc:lookup (im-kkc im) pron)))
                (mapcar #'senn.kkc:word-form words))))
           (senn.segment:try-move-cursor-pos! (converting-current-segment s)
                                              +1)
           (list s :IRV_TO_PROCESS))

          ((= code +backspace-key+)
           (list (let ((pron (converting-pronunciation s)))
                   (make-editing :buffer (senn.buffer:make-buffer
                                          :string pron
                                          :cursor-pos (length pron))))
                 :IRV_DO_NOTHING))

          ((= code +left-key+)
           (list (converting-move-curret-segment s -1)
                 :IRV_TO_PROCESS))

          ((= code +right-key+)
           (list (converting-move-curret-segment s +1)
                 :IRV_TO_PROCESS))

          (t
           (committed (converting-current-input s))))))


(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defun editing-buffer-empty-p (editing)
  (buffer-empty-p (editing-buffer editing)))

(defmethod input ((im im) (s editing) (key key))
  (let ((code (key-sym key)))
    (cond ((<= (char-code #\!) code (char-code #\~))
           (if (/= (key-state key) 0)
               ;; おそらく、Ctrl-pなどのキーが押された
               ;;     -> カーソルが動くような処理を抑制
               (list s :IRV_DO_NOTHING)
               (progn
                 (setf (editing-buffer s)
                       (senn.buffer:insert-char (editing-buffer s)
                                                (code-char code)))
                 (list s :IRV_TO_PROCESS))))

          ((= code +space-key+)
           (let ((pronunciation (senn.buffer:buffer-string
                                 (editing-buffer s))))
             (let ((words (senn.kkc:convert (im-kkc im) pronunciation)))
               (let ((segments (mapcar (lambda (w)
                                         (senn.segment:make-segment
                                          :pron (senn.kkc:word-pron w)
                                          :forms (list (senn.kkc:word-form w))
                                          :has-more-forms-p t
                                          :current-index 0))
                                       words)))
                 (list (make-converting :segments segments
                                        :pronunciation pronunciation)
                       :IRV_TO_PROCESS)))))

          ((= code +enter-key+)
           (committed (senn.buffer:buffer-string (editing-buffer s))))

          ((= code +backspace-key+)
           (if (editing-buffer-empty-p s)
               ;; IMEが文字を削除していない -> OSに文字を削除してもらう
               (list s :IRV_TO_PROCESS)
               (progn
                 (setf (editing-buffer s)
                       (senn.buffer:delete-char (editing-buffer s)))
                 ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
                 (list s :IRV_DO_NOTHING))))

          ((and (= code +left-key+)
                (not (editing-buffer-empty-p s)))
           (setf (editing-buffer s)
                 (senn.buffer:move-cursor-pos (editing-buffer s) -1))
           (list s :IRV_TO_PROCESS))

          ((and (= code +right-key+)
                (not (editing-buffer-empty-p s)))
           (setf (editing-buffer s)
                 (senn.buffer:move-cursor-pos (editing-buffer s) +1))
           (list s :IRV_TO_PROCESS))

          ((editing-buffer-empty-p s)
           ;; バッファが空の状態での、上下左右の矢印キー対応
           ;; とりあえずこれで動くか様子見
           (list s :IRV_FLAG_FORWARD_KEY))

          (t
           (list s :IRV_DO_NOTHING)))))
