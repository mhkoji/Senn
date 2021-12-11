(defpackage :senn.lib.ibus
  (:use :cl)
  (:import-from :senn.ibus.server
                :handle-request)
  (:export :make-ime
           :close-ime
           :handle-request))
