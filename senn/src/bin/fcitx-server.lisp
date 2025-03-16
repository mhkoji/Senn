(defpackage :senn.bin.fcitx-server
  (:use :cl)
  (:export :unix-run
           :unix-run-engine))
(in-package :senn.bin.fcitx-server)

(defun make-hachee-ime (kkc-impl)
  (senn.fcitx.stateful-ime:make-service
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
         :hachee-impl-lm-kkc kkc-impl)
   :predictor (make-instance 'senn.im.predict.katakana:predictor)))

(defmacro with-engine-ime ((ime runner) &body body)
  `(let ((kkc (senn.im.kkc.engine:start-kkc ,runner)))
     (unwind-protect
         (let ((,ime (senn.fcitx.stateful-ime:make-service
                      :kkc kkc
                      :predictor (make-instance
                                  'senn.im.predict.katakana:predictor))))
           ,@body)
       (senn.im.kkc.engine:close-kkc kkc))))

(defun ime-client-loop (client ime)
  (labels ((handle (req)
             (senn.fcitx.server:handle-request ime req)))
    (senn-ipc.server:client-loop client :handle-fn #'handle)))

(defun unix-run (kkc)
  (senn-ipc.server.unix:start-server
   (lambda (client)
     (let ((ime (make-hachee-ime kkc)))
       (ime-client-loop client ime)))))

(defun unix-run-engine (runner)
  (senn-ipc.server.unix:start-server
   (lambda (client)
     (with-engine-ime (ime runner)
       (ime-client-loop client ime)))))
