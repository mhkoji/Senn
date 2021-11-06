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


;;; Views
(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((subseq (subseq (senn.buffer:buffer-string buffer)
                        0
                        (senn.buffer:buffer-cursor-pos buffer))))
    (length-utf8 subseq)))

(defun make-editing-view (cursor-pos
                          input
                          predictions
                          prediction-index
                          committed-string)
  (let ((json (jsown:new-js
                ("cursor-pos"       cursor-pos)
                ("input"            input)
                ("predictions"      predictions)
                ("prediction-index" (or prediction-index -1))
                ("committed-input"  committed-string))))
    (format nil "EDITING ~A" (jsown:to-json json))))

(defun editing-view/inputing-state (s &key committed-string)
  (let ((buffer (inputting-buffer s)))
    (make-editing-view (buffer-cursor-pos-utf8 buffer)
                       (senn.buffer:buffer-string buffer)
                       (inputting-predictions s)
                       nil
                       (or committed-string ""))))

(defun editing-view/katakana-state (s)
  (let ((katakana-input (katakana-input s)))
    (make-editing-view (length-utf8 katakana-input)
                       katakana-input nil nil "")))

(defun editing-view/selecting-from-predictions (s)
  (let ((input (selecting-from-predictions-current-input s)))
    (make-editing-view (length-utf8 input)
                       input
                       (selecting-from-predictions-predictions s)
                       (selecting-from-predictions-current-index s)
                       "")))

(defun converting-view/converting-state (s)
  (let ((json
         (jsown:new-js
           ("forms"
            (mapcar #'senn.segment:segment-current-form
                    (converting-segments s)))
           ("cursor-form-index"
            (converting-current-segment-index s))
           ("cursor-form"
            (let ((segment (converting-current-segment s)))
              (if (senn.segment:segment-shows-katakana-p segment)
                  (jsown:new-js
                    ("candidates"      nil)
                    ("candidate-index" -1))
                  (jsown:new-js
                    ("candidates"
                     (if (senn.segment:segment-has-more-candidates-p segment)
                         nil
                         (senn.segment:segment-forms segment)))
                    ("candidate-index"
                     (senn.segment:segment-current-index segment)))))))))
    (format nil "CONVERTING ~A" (jsown:to-json json))))


;;; Execute
(defgeneric execute (ime state key))

(defun resp (irv view &key state)
  (list irv view :state state))

(defmethod execute ((ime senn.im:ime) (s katakana)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (make-inputting))
               (committed-string (katakana-input s)))
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/inputing-state
                  new-state :committed-string committed-string)
                 :state new-state)))
        (t
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/katakana-state s)))))

(defmethod execute ((ime senn.im:ime) (s selecting-from-predictions)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:enter-p key)
         (let ((new-state (make-inputting))
               (committed-string (selecting-from-predictions-current-input
                                  s)))
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/inputing-state
                  new-state :committed-string committed-string)
                 :state new-state)))
        ((or (senn.fcitx.keys:tab-p key)
             (senn.fcitx.keys:down-p key))
         (selecting-from-predictions-move-prediction
          s +1)
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/selecting-from-predictions s)
               :state s))
        ((senn.fcitx.keys:up-p key)
         (selecting-from-predictions-move-prediction
          s -1)
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/selecting-from-predictions s)
               :state s))
        (t
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/selecting-from-predictions s)))))
           
(defmethod execute ((ime senn.im:ime) (s converting)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:left-p key)
         (converting-move-curret-segment s -1)
         (resp senn.fcitx.irv:+DO-NOTHING+
               (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:right-p key)
         (converting-move-curret-segment s +1)
         (resp senn.fcitx.irv:+DO-NOTHING+
               (converting-view/converting-state s)
               :state s))

        ((or (senn.fcitx.keys:space-p key)
             (senn.fcitx.keys:down-p key))
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg  +1 ime))
         ;; senn.fcitx.irv:+DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (resp senn.fcitx.irv:+DO-NOTHING+
               (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:up-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg -1 ime))
         ;; senn.fcitx.irv:+DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (resp senn.fcitx.irv:+DO-NOTHING+
               (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:f7-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) t))
         (resp senn.fcitx.irv:+TO-PROCESS+
               (converting-view/converting-state s)
               :state s))

        ((senn.fcitx.keys:backspace-p key)
         (let* ((pron (converting-pronunciation s))
                (new-state (make-inputting
                            :buffer (senn.buffer:make-buffer
                                     :string pron
                                     :cursor-pos (length pron)))))
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/inputing-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:char-p key)
         (let* ((char (code-char (senn.fcitx.keys:key-sym key)))
                (new-state (make-inputting
                            :buffer (senn.buffer:insert-char
                                     (senn.buffer:make-buffer) char)))
                (committed-string (converting-current-input s)))
           (resp senn.fcitx.irv:+TO-PROCESS+
                 (editing-view/inputing-state
                  new-state :committed-string committed-string)
                 :state new-state)))
                 
        (t
         (let ((committed-string (converting-current-input s))
               (new-state (make-inputting)))
           ;; Disable inserting a new line by the return key
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/inputing-state
                  new-state :committed-string committed-string)
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
         (resp senn.fcitx.irv:+TO-PROCESS+
               (editing-view/inputing-state s)))
        ((/= (logand (senn.fcitx.keys:key-state key)
                     #b100)
             0)
         ;; When FcitxKeyState_Ctr is on.
         (resp (if (buffer-empty-p (inputting-buffer s))
                   ;; Let the OS process the key.
                   ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
                   senn.fcitx.irv:+TO-PROCESS+
                   ;; Do not let the OS process the key.
                   ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
                   senn.fcitx.irv:+DO-NOTHING+)
               (editing-view/inputing-state s)))

        ((senn.fcitx.keys:tab-p key)
         (let ((predictions (inputting-predictions s)))
           (if (null predictions)
                 ;;; Do nothing
               (resp senn.fcitx.irv:+TO-PROCESS+
                     (editing-view/inputing-state s))
               (let ((new-state (make-selecting-from-predictions
                                 :predictions predictions
                                 :current-index 0)))
                 (resp senn.fcitx.irv:+DO-NOTHING+
                       (editing-view/selecting-from-predictions new-state)
                       :state new-state)))))

        ((senn.fcitx.keys:char-p key)
         (let ((char (code-char (senn.fcitx.keys:key-sym key))))
           (setf (inputting-buffer s)
                 (senn.buffer:insert-char (inputting-buffer s) char))
           (inputting-update-predictions s ime)
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/inputing-state s)
                 :state s)))

        ((and (senn.fcitx.keys:f7-p key)
              (not (inputting-buffer-empty-p s)))
         (let ((new-state (make-katakana
                           :input (senn.buffer:buffer-string
                                   (inputting-buffer s)))))
           (resp senn.fcitx.irv:+DO-NOTHING+
                 (editing-view/katakana-state new-state)
                 :state new-state)))

        ((senn.fcitx.keys:space-p key)
         (let ((buffer (inputting-buffer s)))
           (if (buffer-empty-p buffer)
               (let ((new-state (make-inputting)))
                 (resp senn.fcitx.irv:+TO-PROCESS+
                       (editing-view/inputing-state
                        new-state :committed-string "　")
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
                     (resp senn.fcitx.irv:+DO-NOTHING+
                           (converting-view/converting-state
                            new-state)
                           :state new-state)))))))

        ((senn.fcitx.keys:enter-p key)
         (let ((new-state (make-inputting))
               (committed-string (senn.buffer:buffer-string
                                  (inputting-buffer s))))
           (if (string= committed-string "")
               (resp senn.fcitx.irv:+TO-PROCESS+
                     (editing-view/inputing-state new-state)
                     :state new-state)
               ;; 何らかの文字が確定された場合
               ;; エンターキーによる改行は無効化させる
               (resp senn.fcitx.irv:+DO-NOTHING+
                     (editing-view/inputing-state
                      new-state :committed-string committed-string)
                     :state new-state))))

        ((senn.fcitx.keys:backspace-p key)
         (if (inputting-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp senn.fcitx.irv:+TO-PROCESS+
                   (editing-view/inputing-state s))
             (progn
               (setf (inputting-buffer s)
                     (senn.buffer:delete-char (inputting-buffer s)))
               (inputting-update-predictions s ime)
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (list senn.fcitx.irv:+DO-NOTHING+
                     (editing-view/inputing-state s)
                     :state s))))

        ((and (senn.fcitx.keys:left-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) -1))
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/inputing-state s)
               :state s))

        ((and (senn.fcitx.keys:right-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) +1))
         (resp senn.fcitx.irv:+DO-NOTHING+
               (editing-view/inputing-state s)
               :state s))

        ((inputting-buffer-empty-p s)
         ;; バッファが空の状態での、上下左右の矢印キー対応
         ;; とりあえずこれで動くか様子見
         (resp senn.fcitx.irv:+FLAG-FORWARD-KEY+
               (editing-view/inputing-state s)
               :state s))

        (t
         (list senn.fcitx.irv:+DO-NOTHING+
               (editing-view/inputing-state s)))))
