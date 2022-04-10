(defpackage :senn.fcitx.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.fcitx.server)

(defun format-resp (resp)
  (destructuring-bind (consumed-p view) resp
    (format nil "~A ~A"
            (if consumed-p 1 0)
            (if (and consumed-p view) view "NONE"))))

(defun handle-request (stateful-ime line)
  (let ((jsown (jsown:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (jsown:val jsown "op")))))
      (case op
        (:process-input
         (format-resp
          (senn.fcitx.stateful-ime:process-input
           stateful-ime
           (senn.fcitx.keys:make-key
            :sym (jsown:val (jsown:val jsown "args") "sym")
            :state (jsown:val (jsown:val jsown "args") "state")))))
        (:reset-im
         (senn.fcitx.stateful-ime:reset-im stateful-ime)
         "OK")
        (:select-candidate
         (format-resp
          (senn.fcitx.stateful-ime:select-candidate
           stateful-ime
           (jsown:val (jsown:val jsown "args") "index"))))))))
