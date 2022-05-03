(in-package :senn.bin.win-server)

(defclass kkc (senn.win.stateful-ime:history-overwrite-mixin
               senn.im.kkc.hachee:kkc)
  ())

(defun hachee-ime-make (hachee-impl-lm-kkc)
  (let ((state (make-initial-state)))
    (senn.win.stateful-ime:make-ime
     :kkc (make-instance 'kkc
           :history (state-history state)
           :hachee-impl-lm-kkc hachee-impl-lm-kkc)
     :predictor (make-instance 'senn.im.predict.katakana:predictor))))

(defun run (kkc)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (let ((ime (hachee-ime-make kkc)))
       (labels ((handle (req)
                  (senn.win.server:handle-request ime req)))
         (senn-ipc.server:client-loop client :handle-fn #'handle))))))
