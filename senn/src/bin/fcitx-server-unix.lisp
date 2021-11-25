(defpackage :senn.bin.fcitx-server-unix
  (:use :cl)
  (:export :run
           :run-engine))
(in-package :senn.bin.fcitx-server-unix)

(defun run (kkc)
  (senn.server.unix:start-server
   (lambda (client)
     (let ((sf-ime (senn.fcitx.stateful-ime:make-kkc-ime kkc)))
       (labels ((handle (req)
                  (senn.fcitx.server:handle-request sf-ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))

#+sbcl
(defun run-engine (runner)
  (senn.server.unix:start-server
   (lambda (client)
     (senn.im.mixin.engine:with-engine (engine runner)
       (let ((sf-ime (senn.fcitx.stateful-ime:make-engine-ime engine)))
         (labels ((handle (req)
                    (senn.fcitx.server:handle-request sf-ime req)))
           (senn.server:client-loop client :handle-fn #'handle)))))))
