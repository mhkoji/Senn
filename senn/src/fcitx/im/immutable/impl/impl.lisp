(defpackage :senn.fcitx.im.immutable.impl
  (:use :cl)
  (:import-from :senn.fcitx.im.immutable
                :ime-mixin
                :make-output)
  (:local-nicknames (:inputting
                     :senn.fcitx.im.immutable.state.inputting)
                    (:converting
                     :senn.fcitx.im.immutable.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.immutable.state.selecting-from-predictions)
                    (:katakana
                     :senn.fcitx.im.immutable.state.katakana)))
(in-package :senn.fcitx.im.immutable.impl)

(defun resp (view &key state)
  (list (make-output view) state))
