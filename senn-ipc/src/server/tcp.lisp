(defpackage :senn-ipc.server.tcp
  (:use :cl)
  (:export :start-server))
(in-package :senn-ipc.server.tcp)

(defstruct client socket)

(defmethod senn-ipc.server:client-read-line ((client client))
  (let ((stream (usocket:socket-stream (client-socket client))))
    (let ((line (read-line stream nil nil nil)))
      (senn-ipc.server.log:info "Read: ~A" line)
      line)))

(defmethod senn-ipc.server:client-send-line ((client client) resp)
  (let ((stream (usocket:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)
    (senn-ipc.server.log:info "Written: ~A" resp)))

(defun spawn-thread (socket client-loop-fn name)
  (bordeaux-threads:make-thread
   (lambda ()
     (senn-ipc.server.log:info "Connected")
     (funcall client-loop-fn (make-client :socket socket))
     (ignore-errors
      (usocket:socket-close socket))
     (senn-ipc.server.log:info "Disconnected"))
   :name name))

(defun start-server (client-loop-fn &key (port 5678))
  (usocket:with-server-socket
      (server-socket (usocket:socket-listen "0.0.0.0" port))
    (let ((threads nil)
          (sockets nil))
      (senn-ipc.server.log:info "Waiting for client...")
      (unwind-protect
           (loop for id from 1
                 for socket = (usocket:socket-accept server-socket) do
             (progn
               (push socket sockets)
               (push (spawn-thread socket
                                   client-loop-fn
                                   (format nil "process-thread-~d" id))
                     threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)
        (mapc #'usocket:socket-close sockets)))))
