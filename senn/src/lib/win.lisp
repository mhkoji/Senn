(defpackage :senn.lib.win
  (:use :cl)
  (:import-from :senn.win.server
                :handle-request)
  (:export :init
           :destroy
           :make-ime
           :close-ime
           :handle-request))
