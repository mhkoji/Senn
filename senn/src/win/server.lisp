(defpackage :senn.win.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.win.server)

(defun handle-request (stateful-ime line)
  (let ((jsown (jsown:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (jsown:val jsown "op")))))
      (case op
        (:get-input-mode
         (senn.win.stateful-ime:get-input-mode
          stateful-ime))
        (:toggle-input-mode
         (senn.win.stateful-ime:toggle-input-mode
          stateful-ime))
        (:process-input
         (senn.win.stateful-ime:process-input
          stateful-ime
          (senn.win.keys:make-key
           :code (jsown:val (jsown:val jsown "args") "keycode")
           :shift-p (jsown:val (jsown:val jsown "args") "shift"))))
        (:can-process
         (senn.win.stateful-ime:can-process
          stateful-ime
          (senn.win.keys:make-key
           :code (jsown:val (jsown:val jsown "args") "keycode"))))))))
