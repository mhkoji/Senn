(in-package :senn.lib.win)

(defvar *kkc*
  nil)

(defun init (engine-path)
  (let ((engine-runner
         (senn-kkc.engine:make-engine-runner
          :program engine-path)))
    (setq *kkc*
          (make-instance 'senn-kkc.engine:kkc
           :engine-store
           (senn-kkc.engine:make-engine-store
            :engine (senn-kkc.engine:run-engine engine-runner)
            :engine-runner engine-runner)))))

(defun destroy ()
  (when *kkc*
    (senn-kkc.engine:close-kkc *kkc*)))

(defun make-ime ()
  (log:info "Making IME ...")
  (assert *kkc*)
  (senn.win.stateful-ime:make-ime
   :kkc *kkc*
   :predictor nil))

(defun close-ime (ime)
  (declare (ignore ime))
  (log:info "Closing IME ..."))