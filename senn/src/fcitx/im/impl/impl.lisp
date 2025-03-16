(defpackage :senn.fcitx.im.impl
  (:use :cl)
  (:import-from :senn.fcitx.im
                :ime-mixin
                :make-output)
  (:local-nicknames (:inputting
                     :senn.fcitx.im.state.inputting)
                    (:converting
                     :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions)
                    (:katakana
                     :senn.fcitx.im.state.katakana)))
(in-package :senn.fcitx.im.impl)

(defun resp (view &key state)
  (list (make-output view) state))
