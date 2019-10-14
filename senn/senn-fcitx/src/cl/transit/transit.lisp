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

(defun buffer-cursor-pos-in-utf-8 (buffer)
  (length (babel:string-to-octets
           (subseq (senn.buffer:buffer-string buffer)
                   0
                   (senn.buffer:buffer-cursor-pos buffer))
           :encoding :utf-8)))

(defun editing->view-update (input-return-value editing-state
                             &key committed-input)
  (let ((buffer (editing-buffer editing-state)))
    (let ((json-string
           (jsown:to-json
            (jsown:new-js
              ("cursor-pos"
               (buffer-cursor-pos-in-utf-8 buffer))
              ("input"
               (senn.buffer:buffer-string buffer))
              ("committed-input"
               (or committed-input ""))))))
      (format nil "~A EDITING ~A" input-return-value json-string))))

(defun converting->view-update (input-return-value converting-state)
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
               (jsown:new-js
                 ("candidates"
                  (if (senn.segment:segment-has-more-forms-p segment)
                      nil
                      (senn.segment:segment-forms segment)))
                 ("candidate-index"
                  (senn.segment:segment-current-index segment)))))))))
    (format nil "~A CONVERTING ~A" input-return-value json-string)))

(defmethod transit ((ime ime) (s converting)
                    (key senn.fcitx.transit.keys:key))
  (cond ((or (senn.fcitx.transit.keys:space-p key)
             (senn.fcitx.transit.keys:up-p key))
         (move-segment-form-index! (converting-current-segment s) +1 ime)
         (list s (converting->view-update +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:down-p key)
         (move-segment-form-index! (converting-current-segment s) -1 ime)
         (list s (converting->view-update +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:left-p key)
         (converting-move-curret-segment s -1)
         (list s (converting->view-update +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:right-p key)
         (converting-move-curret-segment s +1)
         (list s (converting->view-update +IRV-TO-PROCESS+ s)))

        ((senn.fcitx.transit.keys:backspace-p key)
         (let ((pron (converting-pronunciation s)))
           (let ((editing-state (make-editing
                                 :buffer (senn.buffer:make-buffer
                                          :string pron
                                          :cursor-pos (length pron)))))
             (list editing-state
                   (editing->view-update +IRV-DO-NOTHING+ editing-state)))))

        (t
         (let ((commited-input (converting-current-input s))
               (editing-state (make-editing)))
           (list editing-state
                 ;; Disable inserting a new line by the return key
                 (editing->view-update +IRV-DO-NOTHING+ editing-state
                                       :committed-input commited-input))))))

(defmethod transit ((ime ime) (s editing)
                    (key senn.fcitx.transit.keys:key))
  (cond ((char-p key)
         (if (/= (senn.fcitx.transit.keys:key-state key) 0)
             ;; おそらく、Ctrl-pなどのキーが押された
             ;;     -> カーソルが動くような処理を抑制
             (list s (editing->view-update +IRV-DO-NOTHING+ s))
             (let ((char (code-char (senn.fcitx.transit.keys:key-sym key))))
               (setf (editing-buffer s)
                     (senn.buffer:insert-char (editing-buffer s) char))
               (list s (editing->view-update +IRV-TO-PROCESS+ s)))))

        ((senn.fcitx.transit.keys:space-p key)
         (let ((pron (senn.buffer:buffer-string (editing-buffer s))))
           (let ((segments (senn.im:convert ime pron)))
             (let ((converting-state (make-converting
                                      :segments segments
                                      :pronunciation pron)))
               (list converting-state
                     (converting->view-update
                      +IRV-TO-PROCESS+ converting-state))))))

        ((senn.fcitx.transit.keys:enter-p key)
         (let ((input (senn.buffer:buffer-string (editing-buffer s)))
               (editing-state (make-editing)))
           (list editing-state
                 (editing->view-update
                  (if (string= input "")
                      +IRV-TO-PROCESS+
                      ;; 何らかの文字が確定された場合
                      ;; エンターキーによる改行は無効化させる
                      +IRV-DO-NOTHING+)
                  editing-state
                  :committed-input input))))

        ((senn.fcitx.transit.keys:backspace-p key)
         (if (editing-buffer-empty-p s)
             ;; IMEが文字を削除していない -> OSに文字を削除してもらう
             (list s (editing->view-update +IRV-TO-PROCESS+ s))
             (progn
               (setf (editing-buffer s)
                     (senn.buffer:delete-char (editing-buffer s)))
               ;; IMEが文字を削除した -> OSが文字が削除するのを抑制
               (list s (editing->view-update +IRV-DO-NOTHING+ s)))))

        ((and (senn.fcitx.transit.keys:left-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) -1))
         (list s (editing->view-update +IRV-TO-PROCESS+ s)))

        ((and (senn.fcitx.transit.keys:right-p key)
              (not (editing-buffer-empty-p s)))
         (setf (editing-buffer s)
               (senn.buffer:move-cursor-pos (editing-buffer s) +1))
         (list s (editing->view-update +IRV-TO-PROCESS+ s)))

        ((editing-buffer-empty-p s)
         ;; バッファが空の状態での、上下左右の矢印キー対応
         ;; とりあえずこれで動くか様子見
         (list s (editing->view-update +IRV-FLAG-FORWARD-KEY+ s)))

        (t
         (list s (editing->view-update +IRV-DO-NOTHING+ s)))))
