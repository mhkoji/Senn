;; The this module processes user (keyboard) inputs.
;; This process is described by state transition that includes:
;;  - Latin-to-Hiragana conversion
;;  - Kana-Kanji Conversion
(defpackage :senn.fcitx.im.process-input
  (:use :cl :senn.fcitx.im)
  (:export :execute))
(in-package :senn.fcitx.im.process-input)

;;; Utilities
(defun move-segment-form-index! (seg diff ime)
  (senn.im:append-candidates ime seg)
  (senn.segment:try-move-cursor-pos! seg diff))

(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defun inputting-buffer-empty-p (inputting)
  (buffer-empty-p (inputting-buffer inputting)))

;;; Execute
(defgeneric execute (ime state key))

(defun resp (consumed-p view &key state)
  (list consumed-p view :state state))

(defmethod execute ((ime senn.im:ime) (s katakana)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (make-inputting))
               (committed-string (katakana-input s)))
           (let ((view (editing-view/inputing-state
                        new-state
                        :committed-string committed-string)))
             (resp t view :state new-state))))
        (t
         (resp t (editing-view/katakana-state s)))))

(defmethod execute ((ime senn.im:ime) (s selecting-from-predictions)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (make-inputting))
               (committed-string (selecting-from-predictions-current-input
                                  s)))
           (let ((view (editing-view/inputing-state
                        new-state
                        :committed-string committed-string)))
           (resp t view :state new-state))))

        ((or (senn.fcitx.keys:tab-p key)
             (senn.fcitx.keys:down-p key))
         (selecting-from-predictions-move-prediction s +1)
         (resp t (editing-view/selecting-from-predictions s) :state s))

        ((senn.fcitx.keys:up-p key)
         (selecting-from-predictions-move-prediction s -1)
         (resp t (editing-view/selecting-from-predictions s) :state s))

        (t
         (resp t (editing-view/selecting-from-predictions s)))))
           
(defmethod execute ((ime senn.im:ime) (s converting)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:left-p key)
         (converting-move-curret-segment s -1)
         (resp t (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:right-p key)
         (converting-move-curret-segment s +1)
         (resp t (converting-view/converting-state s)
               :state s))

        ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:down-p key))
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg  +1 ime))
         ;; t because the OS may move the current corsor in the candidate window.
         (resp t (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:up-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg -1 ime))
         ;; t because the OS may move the current corsor in the candidate window.
         (resp t (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:f7-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) t))
         (resp t (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:backspace-p key)
         (let* ((pron (converting-pronunciation s))
                (new-state (make-inputting
                            :buffer (senn.buffer:make-buffer
                                     :string pron
                                     :cursor-pos (length pron)))))
           (resp t (editing-view/inputing-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:char-p key)
         (let* ((char (code-char (senn.fcitx.keys:key-sym key)))
                (new-state (make-inputting
                            :buffer (senn.buffer:insert-char
                                     (senn.buffer:make-buffer) char)))
                (committed-string (converting-current-input s)))
           (resp t (editing-view/inputing-state
                    new-state
                    :committed-string committed-string)
                 :state new-state)))
                 
        (t
         (let ((committed-string (converting-current-input s))
               (new-state (make-inputting)))
           (resp t ;; Disable inserting a new line by the return key
                 (editing-view/inputing-state
                  new-state
                  :committed-string committed-string)
                 :state new-state)))))

(defun inputting-update-predictions (s ime)
  (setf (inputting-predictions s)
        (let ((buffer (inputting-buffer s)))
          (if (buffer-empty-p buffer)
              nil
              (senn.im:predict
               ime (senn.buffer:buffer-string buffer))))))

(defmethod execute ((ime senn.im:ime) (s inputting)
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
         (if (buffer-empty-p (inputting-buffer s))
             ;; Let the OS process the key.
             ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
             (resp nil nil)
             ;; Do not let the OS process the key.
             ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
             (resp t (editing-view/inputing-state s))))

        ((senn.fcitx.keys:tab-p key)
         (let ((predictions (inputting-predictions s)))
           (if (null predictions)
                 ;;; IME does nothing
               (resp nil nil)
               (let ((new-state (make-selecting-from-predictions
                                 :predictions predictions
                                 :current-index 0)))
                 (resp t (editing-view/selecting-from-predictions new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:char-p key)
         (let ((char (code-char (senn.fcitx.keys:key-sym key))))
           (setf (inputting-buffer s)
                 (senn.buffer:insert-char (inputting-buffer s) char))
           (inputting-update-predictions s ime)
           (resp t (editing-view/inputing-state s)
                 :state s)))

        ((and (senn.fcitx.keys:f7-p key)
              (not (inputting-buffer-empty-p s)))
         (let ((new-state (make-katakana
                           :input (senn.buffer:buffer-string
                                   (inputting-buffer s)))))
           (resp t (editing-view/katakana-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:space-p key)
         (let ((buffer (inputting-buffer s)))
           (if (buffer-empty-p buffer)
               (let ((new-state (make-inputting)))
                 (resp t (editing-view/inputing-state
                          new-state
                          :committed-string "　")
                       :state new-state))
               (let ((pron (senn.buffer:buffer-string (inputting-buffer s))))
                 ;; It is convenient to add an additional #\n to make "ん" if the pron ends with a single "n".
                 (when (char= (alexandria:last-elt pron) #\n)
                   (setq pron (senn.buffer:buffer-string
                               (senn.buffer:insert-char (inputting-buffer s)
                                                        #\n))))
                 (let ((segments (senn.im:convert ime pron)))
                   (let ((new-state (make-converting
                                     :segments segments
                                     :pronunciation pron)))
                     (resp t (converting-view/converting-state
                              new-state)
                           :state new-state)))))))

        ((senn.fcitx.keys:enter-p key)
         (let ((committed-string (senn.buffer:buffer-string
                                  (inputting-buffer s))))
           (if (string= committed-string "")
               (resp nil nil)
               ;; 何らかの文字が確定された場合
               ;; エンターキーによる改行は無効化させる
               (let ((new-state (make-inputting)))
                 (resp t (editing-view/inputing-state
                          new-state
                          :committed-string committed-string)
                       :state new-state)))))

        ((senn.fcitx.keys:backspace-p key)
         (if (inputting-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp nil nil)
             (progn
               (setf (inputting-buffer s)
                     (senn.buffer:delete-char (inputting-buffer s)))
               (inputting-update-predictions s ime)
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (list t (editing-view/inputing-state s)
                     :state s))))

        ;; left/right keys
        ((and (senn.fcitx.keys:left-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) -1))
         (resp t (editing-view/inputing-state s) :state s))
        ((and (senn.fcitx.keys:right-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) +1))
         (resp t (editing-view/inputing-state s) :state s))

        (t
         (resp nil nil))))

