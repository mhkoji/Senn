(defpackage :senn.fcitx.im.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.fcitx.im.server)

(defun format-output (output)
  (destructuring-bind (consumed-p view) output
    (format nil "~A ~A"
            (if consumed-p 1 0)
            (or view "NONE"))))

(defun handle-request (mutable-ime line)
  (let ((hash (yason:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (gethash "op" hash)))))
      (case op
        (:process-input
         (format-output
          (senn.fcitx.im.mutable:process-input
           mutable-ime
           (senn.fcitx.keys:make-key
            :sym (gethash "sym" (gethash "args" hash))
            :state (gethash "state" (gethash "args" hash))))))
        (:reset-im
         (senn.fcitx.im.mutable:reset-im mutable-ime)
         "OK")
        (:select-candidate
         (format-output
          (senn.fcitx.im.mutable:select-candidate
           mutable-ime
           (gethash "index" (gethash "args" hash)))))))))
