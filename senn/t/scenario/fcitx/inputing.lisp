(defpackage :senn.t.scenario.fcitx.inputing
  (:use :cl))
(in-package :senn.t.scenario.fcitx.inputing)

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

(defmacro cursor-goes-around-in-the-buffer (&key test)
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

(defmacro f7-then-katakana (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "ア"
                                   :committed-input ""))))))

(defmacro f7-then-nothing-if-empty (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
             (resp nil "NONE")))))


(defmacro enter-then-commit (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             (resp t (editing-view :cursor-pos 0
                                   :input ""
                                   :committed-input "あ"))))))

(defmacro enter-then-nothing-if-empty (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             (resp nil "NONE")))))

(defmacro backspace-then-delete (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             (resp t (editing-view :cursor-pos 3
                                   :input "あ"
                                   :committed-input ""))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             (resp t (editing-view :cursor-pos 0
                                   :input ""
                                   :committed-input ""))))))

(defmacro backspace-then-nothing-if-empty (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             (resp nil "NONE")))))


(defmacro space-then-full-width-space-if-empty
    (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             (resp t (editing-view
                      :cursor-pos 0
                      :input ""
                      :committed-input "　"))))))


(defmacro add-tests (&rest syms)
  `(progn
     ,@(mapcar (lambda (sym)
                 `(fiveam:test ,sym (,sym :test fiveam:is)))
               syms)))


(fiveam:def-suite :senn.fcitx)
(fiveam:in-suite* :senn.fcitx.inputing :in :senn.fcitx)

(add-tests
 cursor-goes-around-in-the-buffer
 cursor-does-not-go-beyond-the-left-end
 cursor-does-not-go-beyond-the-right-end 
 f7-then-katakana
 f7-then-nothing-if-empty
 enter-then-commit
 enter-then-nothing-if-empty
 backspace-then-delete
 backspace-then-nothing-if-empty
 space-then-full-width-space-if-empty)
