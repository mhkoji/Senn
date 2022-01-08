(defpackage :senn.bin.fcitx-server
  (:use :cl)
  (:export :unix-run
           :unix-run-engine))
(in-package :senn.bin.fcitx-server)

(defun unix-run (kkc)
  (senn.server.unix:start-server
   (lambda (client)
     (let ((ime (senn.fcitx.stateful-ime-hachee:make-ime kkc)))
       (labels ((handle (req)
                  (senn.fcitx.server:handle-request ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))

(defun unix-run-engine (runner)
  (senn.server.unix:start-server
   (lambda (client)
     (let ((ime (senn.fcitx.stateful-ime-engine:make-ime runner)))
       (unwind-protect
            (labels ((handle (req)
                       (senn.fcitx.server:handle-request ime req)))
              (senn.server:client-loop client :handle-fn #'handle))
         (senn.fcitx.stateful-ime-engine:close-ime ime))))))
