(defpackage :senn.win.im.toggle-input-mode
  (:use :cl)
  (:export :execute))
(in-package :senn.win.im.toggle-input-mode)

(defgeneric execute (state mode))

(defun result (state mode)
  (list state mode))

(defmethod execute ((s (eql nil))
                    (mode t))
  (assert (eql mode :direct))
  (result (senn.im.inputting:make-state) :hiragana))

(defmethod execute ((s senn.im.inputting:state)
                    (mode t))
  (ecase mode
    (:hiragana
     (if (senn.im.inputting:state-buffer-empty-p s)
         (result nil :direct)
         (result s :direct)))
    (:direct
     (result s :hiragana))))

(defmethod execute ((s senn.im.converting:state)
                    (mode t))
  ;; Do not toggle
  (result s mode))
