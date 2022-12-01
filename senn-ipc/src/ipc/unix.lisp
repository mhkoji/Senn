;;; Unix domain socket for IPC
(defpackage :senn-ipc.unix
  (:use :cl)
  (:export :socket-listen
           :socket-accept
           :socket-close
           :socket-stream
           #+ecl :socket-set-coloexec
           :connect-to
           :connect-abstract-to))
(in-package :senn-ipc.unix)

(defstruct server-socket socket)

(defstruct stream-socket socket stream)

#+ecl
(defun bsd-socket-set-cloexec (bsd-socket)
  (let ((fd (sb-bsd-sockets::socket-file-descriptor
             bsd-socket)))
    (ffi:clines "#include <fcntl.h>")
    (ffi:c-inline (fd) (:int) :int
     "fcntl(#0, F_SETFD, FD_CLOEXEC)" :one-liner t)))

#+ecl
(defun socket-set-cloexec (server-socket)
  (bsd-socket-set-cloexec
   (server-socket-socket server-socket)))

(defun socket-listen (socket-name &key use-abstract)
  #-sbcl
  (assert (not use-abstract))
  (let ((socket (make-instance
                 #+sbcl
                 (if use-abstract
                     'sb-bsd-sockets:local-abstract-socket
                     'sb-bsd-sockets:local-socket)
                 #-sbcl
                 'sb-bsd-sockets:local-socket
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

(defun connect-to (socket-name &key timeout)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket
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
