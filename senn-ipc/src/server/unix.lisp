(defpackage :senn-ipc.server.unix
  (:use :cl)
  (:export :handle-request
           :start-server))
(in-package :senn-ipc.server.unix)

(defstruct client socket)

(defmethod senn-ipc.server:client-read-line ((client client))
  (let ((stream (senn-ipc.unix:socket-stream (client-socket client))))
    (let ((line (read-line stream nil nil nil)))
      (senn-ipc.server.log:info "Read: ~A" line)
      line)))

(defmethod senn-ipc.server:client-send-line ((client client) resp)
  (let ((stream (senn-ipc.unix:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)
    (senn-ipc.server.log:info "Written: ~A" resp)))

(defun spawn-thread (socket client-loop-fn name)
  (bordeaux-threads:make-thread
   (lambda ()
     (senn-ipc.server.log:info "Connected")
     (funcall client-loop-fn (make-client :socket socket))
     (ignore-errors
      (senn-ipc.unix:socket-close socket))
     (senn-ipc.server.log:info "Disconnected"))
   :name name))

(defun start-server (client-loop-fn
                     &key (socket-name "/tmp/senn-server-socket")
                          (use-abstract t))
  (when (and (not use-abstract)
             (uiop:file-exists-p socket-name))
    (delete-file socket-name))
  (let ((server-socket (senn-ipc.unix:socket-listen
                        socket-name
                        :use-abstract use-abstract)))
    (when server-socket
      (let ((threads nil)
            (sockets nil))
        (senn-ipc.server.log:info "Waiting for client...")
        (unwind-protect
             (loop for id from 1
                   for socket = (senn-ipc.unix:socket-accept server-socket) do
               (progn
                 (push socket sockets)
                 (push (spawn-thread socket
                                     client-loop-fn
                                     (format nil "process-thread-~d" id))
                       threads)))
          (mapc #'bordeaux-threads:destroy-thread threads)
          (mapc #'senn-ipc.unix:socket-close sockets)
          (senn-ipc.unix:socket-close server-socket))))))
