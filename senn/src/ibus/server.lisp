(defpackage :senn.ibus.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.ibus.server)

(defun format-resp (resp)
  (destructuring-bind (consumed-p view) resp
    (format nil "~A ~A"
            (if consumed-p 1 0)
            (if (and consumed-p view) view "NONE"))))

(defun handle-request (stateful-ime line)
  (let ((hash (yason:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (gethash "op" hash)))))
      (case op
        (:reset-im
         (senn.ibus.stateful-ime:reset-im stateful-ime)
         "OK")
        (:process-input
         (format-resp
          (senn.ibus.stateful-ime:process-input
           stateful-ime
           (senn.fcitx.keys:make-key
            :sym (gethash "sym" (gethash "args" hash))
            :state (gethash "state" (gethash "args" hash))))))
        (:select-candidate
         (format-resp
          (senn.ibus.stateful-ime:select-candidate
           stateful-ime
           (gethash "index" (gethash "args" hash)))))
        (:toggle-input-mode
         (senn.ibus.stateful-ime:toggle-input-mode stateful-ime))))))
