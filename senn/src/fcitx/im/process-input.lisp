;; The this module processes user (keyboard) inputs.
;; This process is described by state transition that includes:
;;  - Latin-to-Hiragana conversion
;;  - Kana-Kanji Conversion
(defpackage :senn.fcitx.im.process-input
  (:use :cl)
  (:export :execute)
  (:import-from :senn.fcitx.im
                :editing-view/inputting-state
                :editing-view/katakana-state
                :editing-view/selecting-from-predictions
                :converting-view/converting-state))
(in-package :senn.fcitx.im.process-input)

;;; Execute
(defgeneric execute (state ime key))

(defun resp (consumed-p view &key state)
  (list consumed-p view :state state))

(defmethod execute ((s senn.fcitx.im:katakana)
                    (ime senn.fcitx.im:ime)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (senn.im.inputting:make-state))
               (committed-string (senn.fcitx.im:katakana-input s)))
           (let ((view (editing-view/inputting-state
                        new-state
                        :committed-string committed-string)))
             (resp t view :state new-state))))
        (t
         (resp t (editing-view/katakana-state s)))))

(defmethod execute ((s senn.fcitx.im:selecting-from-predictions)
                    (ime senn.fcitx.im:ime)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (senn.im.inputting:make-state))
               (committed-string
                (senn.fcitx.im:selecting-from-predictions-current-input s)))
           (let ((view (editing-view/inputting-state
                        new-state
                        :committed-string committed-string)))
           (resp t view :state new-state))))

        ((or (senn.fcitx.keys:tab-p key)
             (senn.fcitx.keys:down-p key))
         (senn.fcitx.im:selecting-from-predictions-move! s +1)
         (resp t (editing-view/selecting-from-predictions s) :state s))

        ((senn.fcitx.keys:up-p key)
         (senn.fcitx.im:selecting-from-predictions-move! s -1)
         (resp t (editing-view/selecting-from-predictions s) :state s))

        (t
         (resp t (editing-view/selecting-from-predictions s)))))
           
(defmethod execute ((s senn.im.converting:state)
                    (ime senn.fcitx.im:ime)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:left-p key)
         (senn.im.converting:current-segment-move! s -1)
         (resp t (converting-view/converting-state s) :state s))

        ((senn.fcitx.keys:right-p key)
         (senn.im.converting:current-segment-move! s +1)
         (resp t (converting-view/converting-state s) :state s))

        ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:down-p key))
         (senn.im.converting:current-segment-candidates-move!
          s +1 (senn.fcitx.im:ime-kkc ime))
         ;; t because the OS may move the current corsor in the candidate window.
         (resp t (converting-view/converting-state s) :state s))

        ((senn.fcitx.keys:up-p key)
         (senn.im.converting:current-segment-candidates-move!
          s -1 (senn.fcitx.im:ime-kkc ime))
         ;; t because the OS may move the current corsor in the candidate window.
         (resp t (converting-view/converting-state s) :state s))

        ((senn.fcitx.keys:f7-p key)
         (senn.im.converting:current-segment-katakana! s)
         (resp t (converting-view/converting-state s) :state s))

        ((senn.fcitx.keys:backspace-p key)
         (let* ((pron (senn.im.converting:state-pronunciation s))
                (new-state (senn.im.inputting:make-state
                            :buffer (senn.im.buffer:make-buffer
                                     :string pron
                                     :cursor-pos (length pron)))))
           (resp t (editing-view/inputting-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:char-p key)
         (let* ((char (code-char (senn.fcitx.keys:key-sym key)))
                (new-state (senn.im.inputting:make-state
                            :buffer (senn.im.buffer:insert-char
                                     (senn.im.buffer:make-buffer) char)))
                (committed-string (senn.im.converting:current-input s)))
           (resp t (editing-view/inputting-state
                    new-state
                    :committed-string committed-string)
                 :state new-state)))

        (t
         (let ((committed-string (senn.im.converting:current-input s))
               (new-state (senn.im.inputting:make-state)))
           (resp t ;; Disable inserting a new line by the return key
                 (editing-view/inputting-state
                  new-state
                  :committed-string committed-string)
                 :state new-state)))))

(defmethod execute ((s senn.im.inputting:state)
                    (ime senn.fcitx.im:ime)
                    (key senn.fcitx.keys:key))
  (cond ((/= (logand (senn.fcitx.keys:key-state key)
                     #b1000000)
             0)
         ;; When FcitxKeyState_Super is on. we cannot hanndle.
         (resp nil nil))
        ((/= (logand (senn.fcitx.keys:key-state key)
                     #b100)
             0)
         ;; When FcitxKeyState_Ctr is on.
         (if (senn.im.inputting:state-buffer-empty-p s)
             ;; Let the OS process the key.
             ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
             (resp nil nil)
             ;; Do not let the OS process the key.
             ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
             (resp t (editing-view/inputting-state s))))

        ((senn.fcitx.keys:tab-p key)
         (let ((predictions (senn.im.inputting:state-predictions s)))
           (if (null predictions)
                 ;;; IME does nothing
               (resp nil nil)
               (let ((new-state
                      (senn.fcitx.im:make-selecting-from-predictions
                       :predictions predictions
                       :current-index 0)))
                 (resp t (editing-view/selecting-from-predictions new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:char-p key)
         (senn.im.inputting:insert-char!
          s (code-char (senn.fcitx.keys:key-sym key))
          (senn.fcitx.im:ime-predictor ime))
         (resp t (editing-view/inputting-state s) :state s))

        ((and (senn.fcitx.keys:f7-p key)
              (not (senn.im.inputting:state-buffer-empty-p s)))
         (let ((new-state (senn.fcitx.im:make-katakana
                           :input
                           (senn.im.inputting:state-buffer-string s))))
           (resp t (editing-view/katakana-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:space-p key)
         (if (senn.im.inputting:state-buffer-empty-p s)
             (let ((new-state (senn.im.inputting:make-state)))
               (resp t (editing-view/inputting-state
                        new-state
                        :committed-string "　")
                     :state new-state))
             (let ((new-state (senn.im.converting:convert
                               (senn.fcitx.im:ime-kkc ime)
                               (senn.im.inputting:state-buffer-get-pron s))))
               (resp t (converting-view/converting-state new-state)
                     :state new-state))))

        ((senn.fcitx.keys:enter-p key)
         (let ((committed-string (senn.im.inputting:state-buffer-string s)))
           (if (string= committed-string "")
               (resp nil nil)
               ;; 何らかの文字が確定された場合
               ;; エンターキーによる改行は無効化させる
               (let ((new-state (senn.im.inputting:make-state)))
                 (resp t (editing-view/inputting-state
                          new-state
                          :committed-string committed-string)
                       :state new-state)))))

        ((senn.fcitx.keys:backspace-p key)
         (if (senn.im.inputting:delete-char!
              s (senn.fcitx.im:ime-predictor ime))
             ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
             (resp t (editing-view/inputting-state s) :state s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp nil nil)))

        ;; left/right keys
        ((and (senn.fcitx.keys:left-p key)
              (not (senn.im.inputting:state-buffer-empty-p s)))
         (senn.im.inputting:state-buffer-cursor-pos-move! s -1)
         (resp t (editing-view/inputting-state s) :state s))
        ((and (senn.fcitx.keys:right-p key)
              (not (senn.im.inputting:state-buffer-empty-p s)))
         (senn.im.inputting:state-buffer-cursor-pos-move! s +1)
         (resp t (editing-view/inputting-state s) :state s))

        (t
         (resp nil nil))))
