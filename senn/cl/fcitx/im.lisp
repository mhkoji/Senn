(defpackage :senn.fcitx.im
  (:use :cl :senn.fcitx.states)
  (:export :make-im
           :input))
(in-package :senn.fcitx.im)

(defstruct im kkc)

(defgeneric input (im state key))

(defmethod input ((im im) (s committed) key)
  (input im (make-editing) key))


(defun committed (input)
  (list (make-committed :input input)
        (if (string= input "")
            :IRV_TO_PROCESS
            ;; 何らかの文字が確定された場合
            ;; エンターキーによる改行は無効化させる
            :IRV_DO_NOTHING)))

(defun move-segment-form-index! (seg diff im)
  (senn.segment:append-forms!
   seg
   (lambda (pron)
     (let ((words (senn.kkc:lookup (im-kkc im) pron)))
       (mapcar #'senn.kkc:word-form words))))
  (senn.segment:try-move-cursor-pos! seg diff))

(defmethod input ((im im) (s converting) (key senn.fcitx.keys:key))
  (cond ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:up-p key))
         (move-segment-form-index! (converting-current-segment s) +1 im)
         (list s :IRV_TO_PROCESS))

        ((senn.fcitx.keys:down-p key)
         (move-segment-form-index! (converting-current-segment s) -1 im)
         (list s :IRV_TO_PROCESS))

        ((senn.fcitx.keys:backspace-p key)
         (list (let ((pron (converting-pronunciation s)))
                 (make-editing :buffer
                               (senn.buffer:make-buffer
                                :string pron
                                :cursor-pos (length pron))))
               :IRV_DO_NOTHING))

        ((senn.fcitx.keys:left-p key)
         (list (converting-move-curret-segment s -1)
               :IRV_TO_PROCESS))

        ((senn.fcitx.keys:right-p key)
         (list (converting-move-curret-segment s +1)
               :IRV_TO_PROCESS))

        (t
         (committed (converting-current-input s)))))


(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defun editing-buffer-empty-p (editing)
  (buffer-empty-p (editing-buffer editing)))

(defun char-p (k)
  (<= (char-code #\!)
      (senn.fcitx.keys:key-sym k)
      (char-code #\~)))

(defmethod input ((im im) (s editing) (key senn.fcitx.keys:key))
  (cond ((char-p key)
         (if (/= (senn.fcitx.keys:key-state key) 0)
             ;; おそらく、Ctrl-pなどのキーが押された
             ;;     -> カーソルが動くような処理を抑制
             (list s :IRV_DO_NOTHING)
             (let ((char (code-char (senn.fcitx.keys:key-sym key))))
               (setf (editing-buffer s)
                     (senn.buffer:insert-char (editing-buffer s) char))
               (list s :IRV_TO_PROCESS))))

        ((senn.fcitx.keys:space-p key)
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

        ((senn.fcitx.keys:enter-p key)
         (committed (senn.buffer:buffer-string (editing-buffer s))))

        ((senn.fcitx.keys:backspace-p key)
         (if (editing-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (list s :IRV_TO_PROCESS)
             (progn
               (setf (editing-buffer s)
                     (senn.buffer:delete-char (editing-buffer s)))
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (list s :IRV_DO_NOTHING))))

        ((and (senn.fcitx.keys:left-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) -1))
         (list s :IRV_TO_PROCESS))

        ((and (senn.fcitx.keys:right-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) +1))
         (list s :IRV_TO_PROCESS))

        ((editing-buffer-empty-p s)
         ;; バッファが空の状態での、上下左右の矢印キー対応
         ;; とりあえずこれで動くか様子見
         (list s :IRV_FLAG_FORWARD_KEY))

        (t
         (list s :IRV_DO_NOTHING))))
