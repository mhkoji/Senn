(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run
           :run-engine))
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

(defun engine-ime-make (engine-path)
  (let ((engine-runner (senn.im.kkc.engine:make-engine-runner
                        :program engine-path)))
    (senn.win.stateful-ime:make-ime
     :kkc (make-instance 'senn.im.kkc.engine:kkc
           :engine-store
           (senn.im.kkc.engine:make-engine-store
            :engine (senn.im.kkc.engine:run-engine engine-runner)
            :engine-runner engine-runner))
     :predictor nil)))

(defun engine-ime-close (ime)
  (senn.im.kkc.engine:close-kkc (senn.win.stateful-ime:ime-kkc ime)))

;;;

(defun run (kkc)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (let ((ime (hachee-ime-make kkc)))
       (labels ((handle (req)
                  (senn.win.server:handle-request ime req)))
         (senn-ipc.server:client-loop client :handle-fn #'handle))))))

(defun run-engine (runner)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (let ((ime (engine-ime-make runner)))
       (unwind-protect
            (labels ((handle (req)
                       (senn.win.server:handle-request ime req)))
              (senn-ipc.server:client-loop client :handle-fn #'handle))
         (engine-ime-close ime))))))
