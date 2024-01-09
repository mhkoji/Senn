(defpackage :senn.im.kkc.unix
  (:use :cl)
  (:export :kkc
           :close-kkc
           :make-kkc-and-connect))
(in-package :senn.im.kkc.unix)

(defstruct connection socket stream socket-name)

(defun connect (socket-name)
  (let ((socket (senn-ipc.unix:connect-to socket-name)))
    (make-connection
     :stream (senn-ipc.unix:socket-stream socket)
     :socket socket
     :socket-name socket-name)))

(defun disconnect (conn)
  (senn-ipc.unix:socket-close (connection-socket conn)))

(defmethod senn.im.kkc.request:send-line ((conn connection)
                                          (line string))
  (let ((stream (connection-stream conn)))
    (write-line line stream)
    (force-output stream)
    (read-line stream nil nil nil)))

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
    (error (e)
      (format *error-output* "~A" e)
      (list (senn.im.kkc:make-segment
             :pron pron
             :candidates (list (senn.im.kkc:make-candidate
                                :form pron)))))))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (handler-case (senn.im.kkc.request:list-candidates
                 (connection kkc)
                 pron)
    (error (e)
      (format *error-output* "~A" e)
      nil)))

(defun close-kkc (kkc)
  (disconnect (connection kkc)))

(defun make-kkc-and-connect (socket-name)
  (let ((conn (connect socket-name)))
    (make-instance 'kkc :connection conn)))
