(defpackage :senn.fcitx.transit
  (:use :cl
        :senn.fcitx.transit.states)
  (:import-from :senn.im
                :ime)
  (:export :transit))
(in-package :senn.fcitx.transit)

;;; Utilities
(defun move-segment-form-index! (seg diff ime)
  (senn.im:append-candidates ime seg)
  (senn.segment:try-move-cursor-pos! seg diff))


(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defun inputting-buffer-empty-p (inputting)
  (buffer-empty-p (inputting-buffer inputting)))


(defun char-p (k)
  (<= (char-code #\!)
      (senn.fcitx.transit.keys:key-sym k)
      (char-code #\~)))

;;; The transit generic function that manipulates states
(defgeneric transit (ime state key))

(defparameter +IRV-TO-PROCESS+       :IRV_TO_PROCESS)
(defparameter +IRV-DO-NOTHING+       :IRV_DO_NOTHING)
(defparameter +IRV-FLAG-FORWARD-KEY+ :IRV_FLAG_FORWARD_KEY)


(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((subseq (subseq (senn.buffer:buffer-string buffer)
                        0
                        (senn.buffer:buffer-cursor-pos buffer))))
    (length-utf8 subseq)))

(defun make-editing-view (input-return-value
                          cursor-pos input predictions committed-string)
  (let ((json-string (jsown:to-json
                      (jsown:new-js
                       ("cursor-pos"      cursor-pos)
                       ("input"           input)
                       ("predictions"     predictions)
                       ("committed-input" committed-string)))))
    (format nil "~A EDITING ~A" input-return-value json-string)))

(defun inputting->editing-view (input-return-value inputting-state
                                &key predictions
                                     committed-string)
  (let ((buffer (inputting-buffer inputting-state)))
    (make-editing-view input-return-value
                       (buffer-cursor-pos-utf8 buffer)
                       (senn.buffer:buffer-string buffer)
                       predictions
                       (or committed-string ""))))

(defun katakana->editing-view (input-return-value katakana-state)
  (let ((katakana-input (katakana-input katakana-state)))
    (make-editing-view input-return-value
                       (length-utf8 katakana-input)
                       katakana-input
                       nil
                       "")))

(defun converting->converting-view (input-return-value converting-state)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
            ("forms"
             (mapcar #'senn.segment:segment-current-form
                     (converting-segments converting-state)))
            ("cursor-form-index"
             (converting-current-segment-index converting-state))
            ("cursor-form"
             (let ((segment (converting-current-segment converting-state)))
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
                      (senn.segment:segment-current-index segment))))))))))
    (format nil "~A CONVERTING ~A" input-return-value json-string)))

(defmethod transit ((ime ime) (s katakana)
                    (key senn.fcitx.transit.keys:key))
  (cond ((senn.fcitx.transit.keys:enter-p key)
         (let ((inputting-state (make-inputting))
               (committed-string (katakana-input s)))
           (list inputting-state
                 (inputting->editing-view
                  +IRV-DO-NOTHING+ inputting-state
                  :committed-string committed-string))))
        (t
         (list s (katakana->editing-view +IRV-DO-NOTHING+ s)))))

(defmethod transit ((ime ime) (s converting)
                    (key senn.fcitx.transit.keys:key))
  (cond ((senn.fcitx.transit.keys:left-p key)
         (converting-move-curret-segment s -1)
         (list s (converting->converting-view +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:right-p key)
         (converting-move-curret-segment s +1)
         (list s (converting->converting-view +IRV-TO-PROCESS+ s)))

        ((or (senn.fcitx.transit.keys:space-p key)
             (senn.fcitx.transit.keys:up-p key))
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg  +1 ime))
         ;; +IRV-DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (list s (converting->converting-view +IRV-DO-NOTHING+ s)))

        ((senn.fcitx.transit.keys:down-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg -1 ime))
         ;; +IRV-DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (list s (converting->converting-view +IRV-DO-NOTHING+ s)))

        ((senn.fcitx.transit.keys:f7-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) t))
         (list s (converting->converting-view +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:backspace-p key)
         (let ((pron (converting-pronunciation s)))
           (let ((inputting-state (make-inputting
                                   :buffer (senn.buffer:make-buffer
                                            :string pron
                                            :cursor-pos (length pron)))))
             (list inputting-state
                   (inputting->editing-view
                    +IRV-DO-NOTHING+ inputting-state)))))

        ((char-p key)
         (let* ((char (code-char (senn.fcitx.transit.keys:key-sym key)))
                (inputting-state (make-inputting
                                  :buffer (senn.buffer:insert-char
                                           (senn.buffer:make-buffer) char)))
                (committed-string (converting-current-input s)))
           (list inputting-state
                 (inputting->editing-view
                  +IRV-TO-PROCESS+ inputting-state
                  :committed-string committed-string))))
        (t
         (let ((committed-string (converting-current-input s))
               (inputting-state (make-inputting)))
           (list inputting-state
                 ;; Disable inserting a new line by the return key
                 (inputting->editing-view
                  +IRV-DO-NOTHING+ inputting-state
                  :committed-string committed-string))))))

(defmethod transit ((ime ime) (s inputting)
                    (key senn.fcitx.transit.keys:key))
  (cond ((/= (senn.fcitx.transit.keys:key-state key) 0)
         ;; Case when a modifier key such as ctrl is pressed.
         (list s (inputting->editing-view
                  (if (buffer-empty-p (inputting-buffer s))
                      ;; Let the OS process the key.
                      ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
                      +IRV-TO-PROCESS+
                      ;; Do not let the OS process the key.
                      ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
                      +IRV-DO-NOTHING+)
                  s)))

        ((char-p key)
         (let ((char (code-char (senn.fcitx.transit.keys:key-sym key))))
           (setf (inputting-buffer s)
                 (senn.buffer:insert-char (inputting-buffer s) char))
           (let ((inputted-string
                  (senn.buffer:buffer-string (inputting-buffer s))))
             (let ((predictions (senn.im:predict ime inputted-string)))
               (list s (inputting->editing-view
                        ;; +IRV-DO-NOTHING+ must be returned when predictions exist.
                        ;; Otherwise, the view is collapsed when the buffer string is ended with `ー',
                        ;; where the char by the key is `-'.
                        (if predictions
                            +IRV-DO-NOTHING+
                            +IRV-TO-PROCESS+)
                        s
                        :predictions predictions))))))

        ((and (senn.fcitx.transit.keys:f7-p key)
              (not (inputting-buffer-empty-p s)))
         (let ((katakana-state (make-katakana
                                :input (senn.buffer:buffer-string
                                        (inputting-buffer s)))))
           (list katakana-state
                 (katakana->editing-view +IRV-DO-NOTHING+ katakana-state))))

        ((senn.fcitx.transit.keys:space-p key)
         (let ((buffer (inputting-buffer s)))
           (if (buffer-empty-p buffer)
               (let ((inputting-state (make-inputting)))
                 (list inputting-state
                       (inputting->editing-view
                        +IRV-TO-PROCESS+ inputting-state
                        :committed-string "　")))
               (let ((pron (senn.buffer:buffer-string (inputting-buffer s))))
                 ;; It is convenient to add an additional #\n to make "ん" if the pron ends with a single "n".
                 (when (char= (alexandria:last-elt pron) #\n)
                   (setq pron (senn.buffer:buffer-string
                               (senn.buffer:insert-char (inputting-buffer s)
                                                        #\n))))
                 (let ((segments (senn.im:convert ime pron)))
                   (let ((converting-state (make-converting
                                            :segments segments
                                            :pronunciation pron)))
                     (list converting-state
                           (converting->converting-view
                            +IRV-TO-PROCESS+ converting-state))))))))

        ((senn.fcitx.transit.keys:enter-p key)
         (let ((inputting-state (make-inputting))
               (committed-string
                (senn.buffer:buffer-string (inputting-buffer s))))
           (list inputting-state
                 (if (string= committed-string "")
                     (inputting->editing-view
                      +IRV-TO-PROCESS+ inputting-state)
                     (inputting->editing-view
                      ;; 何らかの文字が確定された場合
                      ;; エンターキーによる改行は無効化させる
                      +IRV-DO-NOTHING+ inputting-state
                      :committed-string committed-string)))))

        ((senn.fcitx.transit.keys:backspace-p key)
         (if (inputting-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (list s (inputting->editing-view +IRV-TO-PROCESS+ s))
             (progn
               (setf (inputting-buffer s)
                     (senn.buffer:delete-char (inputting-buffer s)))
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (list s (inputting->editing-view +IRV-DO-NOTHING+ s)))))

        ((and (senn.fcitx.transit.keys:left-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) -1))
         (list s (inputting->editing-view +IRV-TO-PROCESS+ s)))

        ((and (senn.fcitx.transit.keys:right-p key)
              (not (inputting-buffer-empty-p s)))
         (setf (inputting-buffer s)
               (senn.buffer:move-cursor-pos (inputting-buffer s) +1))
         (list s (inputting->editing-view +IRV-TO-PROCESS+ s)))

        ((inputting-buffer-empty-p s)
         ;; バッファが空の状態での、上下左右の矢印キー対応
         ;; とりあえずこれで動くか様子見
         (list s (inputting->editing-view +IRV-FLAG-FORWARD-KEY+ s)))

        (t
         (list s (inputting->editing-view +IRV-DO-NOTHING+ s)))))
