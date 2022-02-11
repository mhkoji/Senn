(defpackage :senn.ibus.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.ibus.server)

(defun handle-request (stateful-ime line)
  (let ((jsown (jsown:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (jsown:val jsown "op")))))
      (case op
        (:process-input
         (senn.ibus.stateful-ime:process-input
          stateful-ime
          (senn.fcitx.keys:make-key
           :sym (jsown:val (jsown:val jsown "args") "sym")
           :state (jsown:val (jsown:val jsown "args") "state"))))
        (:select-candidate
         (senn.ibus.stateful-ime:select-candidate
          stateful-ime
          (jsown:val (jsown:val jsown "args") "index")))
        (:toggle-input-mode
         (senn.ibus.stateful-ime:toggle-input-mode
          stateful-ime))))))
