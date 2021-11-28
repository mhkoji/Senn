(defpackage :senn.lib.fcitx
  (:use :cl)
  (:import-from :senn.fcitx.server
                :handle-request)
  (:export :make-ime
           :close-ime
           :handle-request))
