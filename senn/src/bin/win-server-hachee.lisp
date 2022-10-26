(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun start-server (make-ime-fn)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (let ((ime (funcall make-ime-fn)))
       (labels ((handle (req)
                  (senn.win.server:handle-request ime req)))
         (senn-ipc.server:client-loop client :handle-fn #'handle))))))

(defclass kkc (senn.win.stateful-ime:history-overwrite-mixin
               senn-kkc.hachee:kkc)
  ())

(defvar *hachee-impl-lm-kkc* nil)

(defun run ()
  (when (not *hachee-impl-lm-kkc*)
    (setq *hachee-impl-lm-kkc*
          (senn-kkc.hachee:build-hachee-impl-lm-kkc)))
  (start-server
   (lambda ()
     (senn.win.stateful-ime:make-ime
      :kkc (make-instance 'kkc
            :hachee-impl-lm-kkc *hachee-impl-lm-kkc*)
      #+nil
      :predictor
      #+nil
      (make-instance 'senn.im.predict.katakana:predictor)))))
