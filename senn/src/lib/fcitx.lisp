(defpackage :senn.lib.fcitx
  (:use :cl)
  (:import-from :senn.fcitx.im.server
                :handle-request)
  (:export :make-ime
           :close-ime
           :handle-request))
