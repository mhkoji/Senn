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

(defun make-ime ()
  (log:info "Making IME ...")
  (assert *kkc*)
  (senn.win.stateful-ime:make-ime
   :kkc *kkc*
   :predictor nil))

(defun close-ime (ime)
  (declare (ignore ime))
  (log:info "Closing IME ..."))
