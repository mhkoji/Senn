(defpackage :senn.im.net.stdio
  (:use :cl)
  (:export :start-server))
(in-package :senn.im.net.stdio)

(defstruct connection)

(defmethod senn.im.net.server:read-message ((connection connection))
  (read-line *standard-input* nil nil))

(defmethod senn.im.net.server:send-message ((connection connection) resp)
  (format *standard-output* "~A~%" resp)
  (force-output *standard-output*))

(defun start-server (ime)
  (senn.im.net.server:loop-handling-request ime (make-connection)))
