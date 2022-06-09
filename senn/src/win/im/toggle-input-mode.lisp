(defpackage :senn.win.im.toggle-input-mode
  (:use :cl)
  (:export :execute))
(in-package :senn.win.im.toggle-input-mode)

(defgeneric execute (state mode))

(defun result (state mode)
  (list state mode))

(defmethod execute ((s (eql nil))
                    (mode t))
  (assert (senn.win.im.input-mode:mode=
           mode
           senn.win.im.input-mode:+direct+))
  (result (senn.im.inputting:make-state)
          senn.win.im.input-mode:+hiragana+))

(defmethod execute ((s senn.im.inputting:state)
                    (mode t))
  (senn.win.im.input-mode:mode-case mode
    (:hiragana
     (if (senn.im.inputting:state-buffer-empty-p s)
         (result nil senn.win.im.input-mode:+direct+)
         (result s   senn.win.im.input-mode:+direct+)))
    (:direct
     (result s senn.win.im.input-mode:+hiragana+))))

(defmethod execute ((s senn.im.converting:state)
                    (mode t))
  ;; Do not toggle
  (result s mode))
