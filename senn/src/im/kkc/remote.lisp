;; TODO: test
(defpackage :senn.im.kkc.remote
  (:use :cl)
  (:export :convert
           :lookup
           :kkc
           :close-mixin
           :connect
           :disconnect
           :make-connector))
(in-package :senn.im.kkc.remote)

(defstruct connector
  host port)

(defstruct connection
  socket stream)

(defun connect (connector)
  (let ((socket (usocket:socket-connect
                 (connector-host connector)
                 (connector-port connector))))
    (make-connection
     :socket socket
     :stream (usocket:socket-stream socket))))

(defun disconnect (connection)
  (usocket:socket-close (connection-socket connection)))

(defmethod senn.im.kkc.request:send-line ((conn connection)
                                          (line string))
  (write-line line (connection-stream conn)))


(defclass mixin ()
  ((connection
    :initarg :connection
    :reader connection)))

(defclass convert (mixin) ())

(defmethod senn.im.kkc:convert ((mixin convert) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (handler-case (senn.im.kkc.request:convert
                 (connection mixin)
                 pron)
    (error ()
      (list (senn.im.kkc:make-segment :pron pron :form pron)))))
             

(defclass lookup (mixin) ())

(defmethod senn.im.kkc:lookup ((mixin lookup) (pron string)
                               &key prev next)
  (declare (ignore next prev))
    (handler-case (senn.im.kkc.request:lookup
                   (connection mixin)
                   pron)
      (error ()
        nil)))

(defclass kkc (convert lookup)
  ())

(defun close-mixin (mixin)
  (disconnect (connection mixin)))
