(defpackage :senn.fcitx.transit
  (:use :cl
        :senn.fcitx.transit.states)
  (:import-from :senn.im
                :ime)
  (:export :transit))
(in-package :senn.fcitx.transit)

;;; Utilities
(defun move-segment-form-index! (seg diff ime)
  (labels ((get-forms (pron)
             (senn.im:lookup-forms ime pron)))
    (senn.segment:append-forms! seg #'get-forms))
  (senn.segment:try-move-cursor-pos! seg diff))


(defun buffer-empty-p (buffer)
  (string= (senn.buffer:buffer-string buffer) ""))

(defun editing-buffer-empty-p (editing)
  (buffer-empty-p (editing-buffer editing)))


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
                          cursor-pos input commited-input)
  (let ((json-string (jsown:to-json
                      (jsown:new-js
                       ("cursor-pos"      cursor-pos)
                       ("input"           input)
                       ("committed-input" commited-input)))))
    (format nil "~A EDITING ~A" input-return-value json-string)))

(defun editing->editing-view (input-return-value editing-state
                              &key committed-input)
  (let ((buffer (editing-buffer editing-state)))
    (make-editing-view input-return-value
                       (buffer-cursor-pos-utf8 buffer)
                       (senn.buffer:buffer-string buffer)
                       (or committed-input ""))))

(defun katakana->editing-view (input-return-value katakana-state)
  (let ((katakana-input (katakana-input katakana-state)))
    (make-editing-view input-return-value
                       (length-utf8 katakana-input)
                       katakana-input
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
                      (if (senn.segment:segment-has-more-forms-p segment)
                          nil
                          (senn.segment:segment-forms segment)))
                     ("candidate-index"
                      (senn.segment:segment-current-index segment))))))))))
    (format nil "~A CONVERTING ~A" input-return-value json-string)))

(defmacro resp (state (state->view-fn input-return-value &rest args))
  (let ((g (gensym)))
    `(let ((,g ,state))
       (list ,g (,state->view-fn ,input-return-value ,g ,@args)))))

(defmethod transit ((ime ime) (s katakana)
                    (key senn.fcitx.transit.keys:key))
  (cond ((senn.fcitx.transit.keys:enter-p key)
         (resp (make-editing)
               (editing->editing-view +IRV-DO-NOTHING+
                                      :committed-input (katakana-input s))))
        (t
         (resp s (katakana->editing-view +IRV-DO-NOTHING+)))))

(defmethod transit ((ime ime) (s converting)
                    (key senn.fcitx.transit.keys:key))
  (cond ((senn.fcitx.transit.keys:left-p key)
         (converting-move-curret-segment s -1)
         (resp s (converting->converting-view +IRV-TO-PROCESS+)))

        ((senn.fcitx.transit.keys:right-p key)
         (converting-move-curret-segment s +1)
         (resp s (converting->converting-view +IRV-TO-PROCESS+)))

        ((or (senn.fcitx.transit.keys:space-p key)
             (senn.fcitx.transit.keys:up-p key))
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg  +1 ime))
         ;; +IRV-DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (resp s (converting->converting-view +IRV-DO-NOTHING+)))

        ((senn.fcitx.transit.keys:down-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) nil)
           (move-segment-form-index! curr-seg -1 ime))
         ;; +IRV-DO-NOTHING+ because the OS may move the current corsor in the candidate window.
         (resp s (converting->converting-view +IRV-DO-NOTHING+)))

        ((senn.fcitx.transit.keys:f7-p key)
         (let ((curr-seg (converting-current-segment s)))
           (setf (senn.segment:segment-shows-katakana-p curr-seg) t))
         (resp s (converting->converting-view +IRV-TO-PROCESS+)))

        ((senn.fcitx.transit.keys:backspace-p key)
         (let* ((pron (converting-pronunciation s))
                (next (make-editing
                       :buffer (senn.buffer:make-buffer
                                :string pron
                                :cursor-pos (length pron)))))
           (resp next (editing->editing-view +IRV-DO-NOTHING+))))

        ((char-p key)
         (let* ((char (code-char (senn.fcitx.transit.keys:key-sym key)))
                (next (make-editing
                       :buffer (senn.buffer:insert-char
                                (senn.buffer:make-buffer) char)))
                (commited-input (converting-current-input s)))
           (resp next (editing->editing-view +IRV-TO-PROCESS+
                       :committed-input commited-input))))
        (t
         (let ((commited-input (converting-current-input s)))
           (resp (make-editing)
                 ;; Disable inserting a new line by the return key
                 (editing->editing-view
                  +IRV-DO-NOTHING+
                  :committed-input commited-input))))))

(defmethod transit ((ime ime) (s editing)
                    (key senn.fcitx.transit.keys:key))
  (cond ((/= (senn.fcitx.transit.keys:key-state key) 0)
         ;; Case when a modifier key such as ctrl is pressed.
         (resp s (editing->editing-view
                  (if (buffer-empty-p (editing-buffer s))
                      ;; Let the OS process the key.
                      ;; For example, if the key is ctrl-p, then the OS may move the cursor up.
                      +IRV-TO-PROCESS+
                      ;; Do not let the OS process the key.
                      ;; If the key is ctrl-p, the OS may move the current input up without this, which is very annoying.
                      +IRV-DO-NOTHING+))))

        ((char-p key)
         (let ((char (code-char (senn.fcitx.transit.keys:key-sym key))))
           (setf (editing-buffer s)
                 (senn.buffer:insert-char (editing-buffer s) char))
           (resp s (editing->editing-view +IRV-TO-PROCESS+))))

        ((and (senn.fcitx.transit.keys:f7-p key)
              (not (buffer-empty-p (editing-buffer s))))
         (resp (make-katakana
                :input (senn.buffer:buffer-string (editing-buffer s)))
               (katakana->editing-view +IRV-DO-NOTHING+)))

        ((senn.fcitx.transit.keys:space-p key)
         (let ((buffer (editing-buffer s)))
           (if (buffer-empty-p buffer)
               (resp (make-editing)
                     (editing->editing-view +IRV-TO-PROCESS+
                                            :committed-input "　"))
               (let* ((pron (senn.buffer:buffer-string (editing-buffer s)))
                      (segments (senn.im:convert ime pron)))
                 (resp (make-converting :segments segments
                                        :pronunciation pron)
                       (converting->converting-view +IRV-TO-PROCESS+))))))

        ((senn.fcitx.transit.keys:enter-p key)
         (let ((input (senn.buffer:buffer-string (editing-buffer s))))
           (resp (make-editing)
                 (editing->editing-view
                  (if (string= input "")
                      +IRV-TO-PROCESS+
                      ;; 何らかの文字が確定された場合
                      ;; エンターキーによる改行は無効化させる
                      +IRV-DO-NOTHING+)
                  :committed-input input))))

        ((senn.fcitx.transit.keys:backspace-p key)
         (if (editing-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (resp s (editing->editing-view +IRV-TO-PROCESS+))
             (progn
               (setf (editing-buffer s)
                     (senn.buffer:delete-char (editing-buffer s)))
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (resp s (editing->editing-view +IRV-DO-NOTHING+)))))

        ((and (senn.fcitx.transit.keys:left-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) -1))
         (resp s (editing->editing-view +IRV-TO-PROCESS+)))

        ((and (senn.fcitx.transit.keys:right-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) +1))
         (resp s (editing->editing-view +IRV-TO-PROCESS+)))

        ((editing-buffer-empty-p s)
         ;; バッファが空の状態での、上下左右の矢印キー対応
         ;; とりあえずこれで動くか様子見
         (resp s (editing->editing-view +IRV-FLAG-FORWARD-KEY+)))

        (t
         (resp s (editing->editing-view +IRV-DO-NOTHING+)))))
