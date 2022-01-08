(defpackage :senn.t.scenario.fcitx
  (:use :cl))
(in-package :senn.t.scenario.fcitx)

(defun resp (consumed-p view)
  (format nil "~A ~A" (if consumed-p 1 0) view))

(defun editing-view (&key cursor-pos
                          input
                          predictions
                          prediction-index
                          committed-input)
  (let ((json (jsown:new-js
                ("cursor-pos"       cursor-pos)
                ("input"            input)
                ("predictions"      predictions)
                ("prediction-index" (or prediction-index -1))
                ("committed-input"  committed-input))))
    (format nil "EDITING ~A" (jsown:to-json json))))

(defclass ime (senn.fcitx.stateful-ime:ime)
  ())

(defun make-ime ()
  (let ((state (senn.fcitx.stateful-ime:make-initial-state)))
    (make-instance 'ime :state state)))

(defmacro when-space-key-is-first-inputted-then-full-width-space-is-inserted
    (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             (resp t (editing-view
                      :cursor-pos 0
                      :input ""
                      :committed-input "　"))))))

(defmacro cursor-can-move-around-in-the-buffer (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 6
                                   :input "ああ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "ああ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (editing-view :cursor-pos 0
                                   :input "ああ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "ああ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (editing-view :cursor-pos 6
                                   :input "ああ"
                                   :committed-input ""))))))

(defmacro cursor-does-not-go-beyond-the-left-end (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (editing-view :cursor-pos 0
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (editing-view :cursor-pos 0
                                   :input "あ"
                                   :committed-input ""))))))

(defmacro cursor-does-not-go-beyond-the-right-end (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))))

(fiveam:def-suite :senn.fcitx)
(fiveam:in-suite* :senn.fcitx.stateful-ime :in :senn.fcitx)

(fiveam:test
    when-space-key-is-first-inputted-then-full-width-space-is-inserted
  (when-space-key-is-first-inputted-then-full-width-space-is-inserted
   :test fiveam:is))

(fiveam:test cursor-can-move-around-in-the-buffer
  (cursor-can-move-around-in-the-buffer :test fiveam:is))

(fiveam:test cursor-does-not-go-beyond-the-left-end
  (cursor-does-not-go-beyond-the-left-end :test fiveam:is))

(fiveam:test cursor-does-not-go-beyond-the-right-end
  (cursor-does-not-go-beyond-the-right-end :test fiveam:is))
