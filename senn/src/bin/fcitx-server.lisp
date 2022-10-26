(defpackage :senn.bin.fcitx-server
  (:use :cl)
  (:export :unix-run
           :unix-run-engine))
(in-package :senn.bin.fcitx-server)

(defun make-hachee-ime (kkc-impl)
  (senn.fcitx.stateful-ime:make-ime
   :kkc-store (senn-kkc.store.hachee:make-store kkc-impl)
   :predictor (make-instance 'senn.im.predict.katakana:predictor)))

(defmacro with-engine-ime ((ime runner) &body body)
  `(let ((store (senn-kkc.store.engine:make-store-and-run ,runner)))
    (unwind-protect
         (let ((,ime (senn.fcitx.stateful-ime:make-ime
                      :kkc-store store
                      :predictor (make-instance
                                  'senn.im.predict.katakana:predictor))))
           ,@body)
      (senn-kkc.store.engine:close-store store))))

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
