;;; Unix domain socket for IPC
(defpackage :senn.ipc.unix
  (:use :cl)
  (:export :socket-listen
           :socket-accept
           :socket-close
           :socket-stream
           :connect-abstract-to))
(in-package :senn.ipc.unix)

(defstruct server-socket socket)

(defstruct stream-socket socket stream)

#+sbcl
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


(defun socket-stream (stream-socket)
  (stream-socket-stream stream-socket))

(defgeneric socket-close (socket))

#+sbcl
(defmethod socket-close ((socket server-socket))
  (sb-bsd-sockets:socket-close (server-socket-socket socket)))

#+sbcl
(defmethod socket-close ((socket stream-socket))
  (sb-bsd-sockets:socket-close (stream-socket-socket socket)))

#+sbcl
(defun socket-accept (server-socket)
  (let ((socket (sb-bsd-sockets:socket-accept
                 (server-socket-socket server-socket))))
    (let ((stream (sb-bsd-sockets:socket-make-stream
                   socket
                   :input t
                   :output t
                   :buffering :full)))
      (make-stream-socket :socket socket :stream stream))))


#+sbcl
(defun connect-abstract-to (socket-name &key timeout)
  (let ((socket (make-instance 'sb-bsd-sockets:local-abstract-socket
                               :type :stream)))
    (sb-bsd-sockets:socket-connect socket socket-name)
    (make-stream-socket
     :socket socket
     :stream (sb-bsd-sockets:socket-make-stream
              socket
              :input t
              :output t
              :timeout timeout ;; The unit seems to be sec.
              :buffering :full))))
