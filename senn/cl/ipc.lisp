(defpackage :senn.ipc
  (:use :cl)
  (:export :server-listen
           :server-accept
           :server-close
           :client-read-line
           :client-write-line
           :client-close))
(in-package :senn.ipc)

(defun server-listen (socket-name &key use-abstract)
  (let ((sbcl-socket (make-instance
                      (if use-abstract
                          'sb-bsd-sockets:local-abstract-socket
                          'sb-bsd-sockets:local-socket)
                      :type :stream)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-bind sbcl-socket socket-name)
          (sb-bsd-sockets:socket-listen sbcl-socket 100)
          sbcl-socket)
      (sb-bsd-sockets:address-in-use-error ()
        nil))))

(defun server-close (server-socket)
  (sb-bsd-sockets:socket-close server-socket))

(defstruct client-socket soc stream)

(defun server-accept (server-socket)
  (let ((soc (sb-bsd-sockets:socket-accept server-socket)))
    (let ((stream (sb-bsd-sockets:socket-make-stream
                   soc
                   :input t :output t :buffering :full)))
      (make-client-socket :soc soc :stream stream))))

(defun client-read-line (client-socket)
  (read-line (client-socket-stream client-socket) nil))

(defun client-write-line (client-socket string)
  (let ((stream (client-socket-stream client-socket)))
    (write-line string stream)
    (force-output stream)))

(defun client-close (client-socket)
  (sb-bsd-sockets:socket-close (client-socket-soc client-socket)))
