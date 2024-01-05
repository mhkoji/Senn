;; The this module processes user (keyboard) inputs.
;; This process is described by state transition that includes:
;;  - Latin-to-Hiragana conversion
;;  - Kana-Kanji Conversion
(defpackage :senn.fcitx.im.immutable.process-input
  (:use :cl :senn.fcitx.im.view)
  (:export :mixin
           :execute))
(in-package :senn.fcitx.im.immutable.process-input)

(defclass mixin (senn.fcitx.im.state.inputting:ime
                 senn.fcitx.im.state.converting:ime)
  ())

(defgeneric execute (state key mixin))

;;;

(defun resp (consumed-p view &key state)
  (list (list consumed-p view) state))

(defmethod execute ((s senn.fcitx.im.state.katakana:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (senn.fcitx.im.state.inputting:make-state))
               (committed-string (senn.fcitx.im.state.katakana:state-input s)))
           (let ((view (senn.fcitx.im.state.inputting:editing-view
                        new-state
                        :committed-string committed-string)))
             (resp t view :state new-state))))
        (t
         (resp t (senn.fcitx.im.state.katakana:editing-view s)))))

(defmethod execute ((s senn.fcitx.im.state.selecting-from-predictions:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (senn.fcitx.im.state.inputting:make-state))
               (committed-string (senn.fcitx.im.state.selecting-from-predictions:state-current-input s)))
           (let ((view (senn.fcitx.im.state.inputting:editing-view
                        new-state
                        :committed-string committed-string)))
           (resp t view :state new-state))))

        ((or (senn.fcitx.keys:tab-p key)
             (senn.fcitx.keys:down-p key))
         (senn.fcitx.im.state.selecting-from-predictions:state-move! s +1)
         (resp t
               (senn.fcitx.im.state.selecting-from-predictions:editing-view s)
               :state s))

        ((senn.fcitx.keys:up-p key)
         (senn.fcitx.im.state.selecting-from-predictions:state-move! s -1)
         (resp t
               (senn.fcitx.im.state.selecting-from-predictions:editing-view s)
               :state s))

        (t
         (resp t (senn.fcitx.im.state.selecting-from-predictions:editing-view s)))))

(defmethod execute ((s senn.fcitx.im.state.converting:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:left-p key)
         (senn.fcitx.im.state.converting:current-segment-move! s -1)
         (resp t (senn.fcitx.im.state.converting:converting-view s) :state s))

        ((senn.fcitx.keys:right-p key)
         (senn.fcitx.im.state.converting:current-segment-move! s +1)
         (resp t (senn.fcitx.im.state.converting:converting-view s) :state s))

        ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:down-p key))
         (senn.fcitx.im.state.converting:current-segment-candidates-move!
          s +1 ime)
         ;; Return t because the OS may move the current corsor in the candidate window.
         (resp t (senn.fcitx.im.state.converting:converting-view s) :state s))

        ((senn.fcitx.keys:up-p key)
         (senn.fcitx.im.state.converting:current-segment-candidates-move!
          s -1 ime)
         ;; Return t because the OS may move the current corsor in the candidate window.
         (resp t (senn.fcitx.im.state.converting:converting-view s) :state s))

        ((senn.fcitx.keys:f7-p key)
         (senn.fcitx.im.state.converting:current-segment-katakana! s)
         (resp t (senn.fcitx.im.state.converting:converting-view s) :state s))

        ((senn.fcitx.keys:backspace-p key)
         (let* ((pron (senn.fcitx.im.state.converting:state-pronunciation s))
                (new-state (senn.fcitx.im.state.inputting:make-state
                            :buffer (senn.im.buffer:make-buffer
                                     :string pron
                                     :cursor-pos (length pron)))))
           (resp t
                 (senn.fcitx.im.state.inputting:editing-view new-state)
                 :state new-state)))

        ((senn.fcitx.keys:char-p key)
         (let* ((char (code-char (senn.fcitx.keys:key-sym key)))
                (new-state (senn.fcitx.im.state.inputting:make-state
                            :buffer (senn.im.buffer:insert-char
                                     (senn.im.buffer:make-buffer) char)))
                (committed-string (senn.fcitx.im.state.converting:current-input s)))
           (resp t (senn.fcitx.im.state.inputting:editing-view
                    new-state
                    :committed-string committed-string)
                 :state new-state)))

        (t
         (let ((committed-string (senn.fcitx.im.state.converting:current-input s))
               (new-state (senn.fcitx.im.state.inputting:make-state)))
           (resp t ;; Disable inserting a new line by the return key
                 (senn.fcitx.im.state.inputting:editing-view
                  new-state
                  :committed-string committed-string)
                 :state new-state)))))

(defmethod execute ((s senn.fcitx.im.state.inputting:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((/= (logand (senn.fcitx.keys:key-state key)
                     #b1000000)
             0)
         ;; When FcitxKeyState_Super is on. we cannot hanndle.
         (resp nil nil))
        ((/= (logand (senn.fcitx.keys:key-state key)
                     #b100)
             0)
         ;; When FcitxKeyState_Ctr is on.
         (if (senn.fcitx.im.state.inputting:state-buffer-empty-p s)
             ;; Let the OS process the key.
             ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
             (resp nil nil)
             ;; Do not let the OS process the key.
             ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
             (resp t (senn.fcitx.im.state.inputting:editing-view s))))

        ((senn.fcitx.keys:tab-p key)
         (let ((predictions
                (senn.fcitx.im.state.inputting:state-predictions s)))
           (if (null predictions)
                 ;;; IME does nothing
               (resp nil nil)
               (let ((new-state (senn.fcitx.im.state.selecting-from-predictions:make-state
                                 :predictions predictions
                                 :current-index 0)))
                 (resp t
                       (senn.fcitx.im.state.selecting-from-predictions:editing-view new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:char-p key)
         (let ((ch (code-char (senn.fcitx.keys:key-sym key))))
           (senn.fcitx.im.state.inputting:insert-char! s ch ime))
         (resp t (senn.fcitx.im.state.inputting:editing-view s) :state s))

        ((and (senn.fcitx.keys:f7-p key)
              (not (senn.fcitx.im.state.inputting:state-buffer-empty-p s)))
         (let ((new-state
                (senn.fcitx.im.state.katakana:make-state
                 :input
                 (senn.fcitx.im.state.inputting:state-buffer-string s))))
           (resp t (senn.fcitx.im.state.katakana:editing-view new-state)
                 :state new-state)))

        ((senn.fcitx.keys:space-p key)
         (if (senn.fcitx.im.state.inputting:state-buffer-empty-p s)
             (let ((new-state (senn.fcitx.im.state.inputting:make-state)))
               (resp t (senn.fcitx.im.state.inputting:editing-view
                        new-state
                        :committed-string "　")
                     :state new-state))
             (let ((pron (senn.fcitx.im.state.inputting:state-buffer-get-pron
                          s)))
               (let ((new-state (senn.fcitx.im.state.converting:convert
                                 ime pron)))
                 (resp t (senn.fcitx.im.state.converting:converting-view new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:enter-p key)
         (let ((committed-string
                (senn.fcitx.im.state.inputting:state-buffer-string s)))
           (if (string= committed-string "")
               (resp nil nil)
               ;; 何らかの文字が確定された場合
               ;; エンターキーによる改行は無効化させる
               (let ((new-state (senn.fcitx.im.state.inputting:make-state)))
                 (resp t (senn.fcitx.im.state.inputting:editing-view
                          new-state
                          :committed-string committed-string)
                       :state new-state)))))

        ((senn.fcitx.keys:backspace-p key)
         (if (senn.fcitx.im.state.inputting:delete-char! s ime)
             ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
             (resp t (senn.fcitx.im.state.inputting:editing-view s) :state s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp nil nil)))

        ;; left/right keys
        ((and (senn.fcitx.keys:left-p key)
              (not (senn.fcitx.im.state.inputting:state-buffer-empty-p s)))
         (senn.fcitx.im.state.inputting:state-buffer-cursor-pos-move! s -1)
         (resp t (senn.fcitx.im.state.inputting:editing-view s) :state s))
        ((and (senn.fcitx.keys:right-p key)
              (not (senn.fcitx.im.state.inputting:state-buffer-empty-p s)))
         (senn.fcitx.im.state.inputting:state-buffer-cursor-pos-move! s +1)
         (resp t (senn.fcitx.im.state.inputting:editing-view s) :state s))

        (t
         (resp nil nil))))
