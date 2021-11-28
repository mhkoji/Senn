(in-package :senn.lib.fcitx)

(defvar *engine*
  (senn.im.mixin.engine:run-engine
   (senn.im.mixin.engine:make-engine-runner
    :program "/usr/lib/senn/kkc-engine")))

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-engine-ime *engine*))
