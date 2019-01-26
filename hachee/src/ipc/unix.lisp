;;; Unix domain socket for IPC
(defpackage :hachee.ipc.unix
  (:use :cl)
  (:export :socket-listen
           :socket-accept
           :socket-close
           :socket-stream))
(in-package :hachee.ipc.unix)

(defstruct server-socket socket)

(defstruct stream-socket socket stream)

(defun socket-listen (socket-name &key use-abstract)
  (let ((socket (make-instance (if use-abstract
                                   'sb-bsd-sockets:local-abstract-socket
                                   'sb-bsd-sockets:local-socket)
                               :type :stream)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-bind socket socket-name)
          (sb-bsd-sockets:socket-listen socket 100)
          (make-server-socket :socket socket))
      (sb-bsd-sockets:address-in-use-error ()
        nil))))


(defgeneric socket-close (socket))

(defmethod socket-close ((socket server-socket))
  (sb-bsd-sockets:socket-close (server-socket-socket socket)))

(defmethod socket-close ((socket stream-socket))
  (sb-bsd-sockets:socket-close (stream-socket-socket socket)))


(defun socket-accept (server-socket)
  (let ((socket (sb-bsd-sockets:socket-accept
                 (server-socket-socket server-socket))))
    (let ((stream (sb-bsd-sockets:socket-make-stream
                   socket
                   :input t
                   :output t
                   :buffering :full)))
      (make-stream-socket :socket socket :stream stream))))


(defun socket-stream (stream-socket)
  (stream-socket-stream stream-socket))
