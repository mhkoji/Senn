(in-package :senn.lib.win)

(defvar *kkc*
  nil)

(defun init (engine-path)
  (setq *kkc* (senn.im.kkc.engine:start-kkc
               (senn.im.kkc.engine:make-engine-runner
                :program engine-path))))

(defun destroy ()
  (when *kkc*
    (senn.im.kkc.engine:close-kkc *kkc*)))

(defclass kkc (senn.win.history:convert-overwrite-mixin
               senn.im.kkc.wrapper:wrapper)
  ())

(defun make-ime ()
  (log:info "Making IME ...")
  (assert *kkc*)
  (let ((kkc (make-instance 'kkc :kkc *kkc*)))
    (let ((service (make-instance 'senn.win.stateful-ime:service
                    :ime (make-instance 'senn.win.im:ime
                          :kkc kkc
                          :predictor nil))))
      (senn.win.history:convert-overwrite-mixin-set-holder kkc service)
      service)))

(defun close-ime (ime)
  (declare (ignore ime))
  (log:info "Closing IME ..."))
