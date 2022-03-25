;; TODO: test
(defpackage :senn.im.kkc.remote
  (:use :cl)
  (:export :kkc
           :close-kkc
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


(defclass kkc ()
  ((connection
    :initarg :connection
    :reader connection)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (handler-case (senn.im.kkc.request:convert
                 (connection kkc)
                 pron)
    (error ()
      (list (senn.im.kkc:make-segment
             :pron pron
             :candidates (list (senn.im.kkc:make-candidate
                                  :form pron)))))))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (handler-case (senn.im.kkc.request:list-candidates
                 (connection kkc)
                 pron)
    (error ()
      nil)))

(defun close-kkc (kkc)
  (disconnect (connection kkc)))
