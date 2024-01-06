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
  (let ((jsown (jsown:parse line)))
    (let ((op (alexandria:make-keyword
               (string-upcase
                (jsown:val jsown "op")))))
      (case op
        (:process-input
         (format-output
          (senn.fcitx.im.mutable:process-input
           mutable-ime
           (senn.fcitx.keys:make-key
            :sym (jsown:val (jsown:val jsown "args") "sym")
            :state (jsown:val (jsown:val jsown "args") "state")))))
        (:reset-im
         (senn.fcitx.im.mutable:reset-im mutable-ime)
         "OK")
        (:select-candidate
         (format-output
          (senn.fcitx.im.mutable:select-candidate
           mutable-ime
           (jsown:val (jsown:val jsown "args") "index"))))))))