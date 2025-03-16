(defpackage :senn.win.im.toggle-input-mode
  (:use :cl))
(in-package :senn.win.im.toggle-input-mode)

(defun result (state mode)
  (list state mode))

(defmethod senn.win.ime:toggle-input-mode ((s (eql :direct-state))
                                           (ime senn.win.im:ime)
                                           (mode t))
  (assert (senn.win.im.input-mode:mode=
           mode
           senn.win.im.input-mode:+direct+))
  (result (senn.im.inputting:make-state)
          senn.win.im.input-mode:+hiragana+))

(defmethod senn.win.ime:toggle-input-mode ((s senn.im.inputting:state)
                                           (ime senn.win.im:ime)
                                           (mode t))
  (senn.win.im.input-mode:mode-case mode
    (:hiragana
     (result (if (senn.im.inputting:state-buffer-empty-p s)
		 :direct-state
		 s)
	     senn.win.im.input-mode:+direct+))
    (:direct
     (result s
	     senn.win.im.input-mode:+hiragana+))))

(defmethod senn.win.ime:toggle-input-mode ((s senn.im.converting:state)
                                           (ime senn.win.im:ime)
                                           (mode t))
  ;; Do not toggle
  (result s mode))
