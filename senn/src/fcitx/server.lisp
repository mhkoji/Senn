(defpackage :senn.fcitx.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.fcitx.server)

(defun format-output (output)
  (destructuring-bind (consumed-p view) output
    (format nil "~A ~A"
            (if consumed-p 1 0)
            (or view "NONE"))))

(defun handle-request (stateful-ime line)
  (let ((hash (yason:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (gethash "op" hash)))))
      (case op
        (:process-input
         (format-output
          (senn.fcitx.stateful-ime:process-input
           stateful-ime
           (senn.fcitx.keys:make-key
            :sym (gethash "sym" (gethash "args" hash))
            :state (gethash "state" (gethash "args" hash))))))
        (:reset-im
         (senn.fcitx.stateful-ime:reset-im stateful-ime)
         "OK")
        (:select-candidate
         (format-output
          (senn.fcitx.stateful-ime:select-candidate
           stateful-ime
           (gethash "index" (gethash "args" hash)))))))))
