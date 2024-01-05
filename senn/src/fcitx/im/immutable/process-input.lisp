;; The this module processes user (keyboard) inputs.
;; This process is described by state transition that includes:
;;  - Latin-to-Hiragana conversion
;;  - Kana-Kanji Conversion
(defpackage :senn.fcitx.im.immutable.process-input
  (:use :cl)
  (:local-nicknames (:inputting :senn.fcitx.im.state.inputting)
                    (:converting :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions)
                    (:katakana :senn.fcitx.im.state.katakana))
  (:export :mixin
           :execute))
(in-package :senn.fcitx.im.immutable.process-input)

(defclass mixin (inputting:ime
                 converting:ime)
  ())

(defgeneric execute (state key mixin))

;;;

(defun resp (consumed-p view &key state)
  (list (list consumed-p view) state))

(defmethod execute ((s katakana:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (inputting:make-state))
               (committed-string (katakana:state-input s)))
           (resp t (inputting:editing-view
                    new-state
                    :committed-string committed-string)
                 :state new-state)))
        (t
         (resp t (katakana:editing-view s)))))

(defmethod execute ((s selecting-from-predictions:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (inputting:make-state))
               (committed-string
                (selecting-from-predictions:state-current-input s)))
           (resp t (inputting:editing-view
                    new-state
                    :committed-string committed-string)
                 :state new-state)))

        ((or (senn.fcitx.keys:tab-p key)
             (senn.fcitx.keys:down-p key))
         (selecting-from-predictions:state-move! s +1)
         (resp t (selecting-from-predictions:editing-view s) :state s))

        ((senn.fcitx.keys:up-p key)
         (selecting-from-predictions:state-move! s -1)
         (resp t (selecting-from-predictions:editing-view s) :state s))

        (t
         (resp t (selecting-from-predictions:editing-view s)))))

(defmethod execute ((s converting:state)
                    (key senn.fcitx.keys:key)
                    (ime mixin))
  (cond ((senn.fcitx.keys:left-p key)
         (converting:current-segment-move! s -1)
         (resp t (converting:converting-view s) :state s))

        ((senn.fcitx.keys:right-p key)
         (converting:current-segment-move! s +1)
         (resp t (converting:converting-view s) :state s))

        ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:down-p key))
         (converting:current-segment-candidates-move! s +1 ime)
         ;; Return t because the OS may move the current corsor in the candidate window.
         (resp t (converting:converting-view s) :state s))

        ((senn.fcitx.keys:up-p key)
         (converting:current-segment-candidates-move! s -1 ime)
         ;; Return t because the OS may move the current corsor in the candidate window.
         (resp t (converting:converting-view s) :state s))

        ((senn.fcitx.keys:f7-p key)
         (converting:current-segment-katakana! s)
         (resp t (converting:converting-view s) :state s))

        ((senn.fcitx.keys:backspace-p key)
         (let* ((pron (converting:state-pronunciation s))
                (new-state (inputting:make-state
                            :buffer (senn.im.buffer:make-buffer
                                     :string pron
                                     :cursor-pos (length pron)))))
           (resp t (inputting:editing-view new-state) :state new-state)))
                 

        ((senn.fcitx.keys:char-p key)
         (let* ((char (code-char (senn.fcitx.keys:key-sym key)))
                (new-state (inputting:make-state
                            :buffer (senn.im.buffer:insert-char
                                     (senn.im.buffer:make-buffer) char)))
                (committed-string (converting:current-input s)))
           (resp t (inputting:editing-view
                    new-state
                    :committed-string committed-string)
                 :state new-state)))

        (t
         (let ((committed-string (converting:current-input s))
               (new-state (inputting:make-state)))
           (resp t ;; Disable inserting a new line by the return key
                 (inputting:editing-view
                  new-state
                  :committed-string committed-string)
                 :state new-state)))))

(defmethod execute ((s inputting:state)
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
         (if (inputting:state-buffer-empty-p s)
             ;; Let the OS process the key.
             ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
             (resp nil nil)
             ;; Do not let the OS process the key.
             ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
             (resp t (inputting:editing-view s))))

        ((senn.fcitx.keys:tab-p key)
         (let ((predictions
                (inputting:state-predictions s)))
           (if (null predictions)
                 ;;; IME does nothing
               (resp nil nil)
               (let ((new-state (selecting-from-predictions:make-state
                                 :predictions predictions
                                 :current-index 0)))
                 (resp t (selecting-from-predictions:editing-view new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:char-p key)
         (let ((ch (code-char (senn.fcitx.keys:key-sym key))))
           (inputting:insert-char! s ch ime))
         (resp t (inputting:editing-view s) :state s))

        ((and (senn.fcitx.keys:f7-p key)
              (not (inputting:state-buffer-empty-p s)))
         (let ((new-state (katakana:make-state
                           :input (inputting:state-buffer-string s))))
           (resp t (katakana:editing-view new-state)
                 :state new-state)))

        ((senn.fcitx.keys:space-p key)
         (if (inputting:state-buffer-empty-p s)
             (let ((new-state (inputting:make-state)))
               (resp t (inputting:editing-view
                        new-state
                        :committed-string "　")
                     :state new-state))
             (let ((pron (inputting:state-buffer-get-pron s)))
               (let ((new-state (converting:convert ime pron)))
                 (resp t (converting:converting-view new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:enter-p key)
         (let ((committed-string
                (inputting:state-buffer-string s)))
           (if (string= committed-string "")
               (resp nil nil)
               ;; 何らかの文字が確定された場合
               ;; エンターキーによる改行は無効化させる
               (let ((new-state (inputting:make-state)))
                 (resp t (inputting:editing-view
                          new-state
                          :committed-string committed-string)
                       :state new-state)))))

        ((senn.fcitx.keys:backspace-p key)
         (if (inputting:delete-char! s ime)
             ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
             (resp t (inputting:editing-view s) :state s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp nil nil)))

        ;; left/right keys
        ((and (senn.fcitx.keys:left-p key)
              (not (inputting:state-buffer-empty-p s)))
         (inputting:state-buffer-cursor-pos-move! s -1)
         (resp t (inputting:editing-view s) :state s))
        ((and (senn.fcitx.keys:right-p key)
              (not (inputting:state-buffer-empty-p s)))
         (inputting:state-buffer-cursor-pos-move! s +1)
         (resp t (inputting:editing-view s) :state s))

        (t
         (resp nil nil))))
