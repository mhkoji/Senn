(defpackage :senn-ipc.server.unix
  (:use :cl)
  (:export :handle-request
           :start-server))
(in-package :senn-ipc.server.unix)

(defstruct client id socket)

(defmacro client-log-info (client &rest args)
  `(with-accessors ((client-id client-id)) ,client
     (log:info client-id ,@args)))

(defmethod senn-ipc.server:client-read-line ((client client))
  (let ((stream (senn-ipc.unix:socket-stream (client-socket client))))
    (let ((line (read-line stream nil nil nil)))
      (client-log-info client line)
      line)))

(defmethod senn-ipc.server:client-send-line ((client client) resp)
  (let ((stream (senn-ipc.unix:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)
    (let ((line (format nil "~A~%" resp)))
      (client-log-info client line))))


(defun spawn-client-thread (client client-loop-fn)
  (bordeaux-threads:make-thread
   (lambda ()
     (funcall client-loop-fn client)
     (ignore-errors
      (senn-ipc.unix:socket-close (client-socket client)))
     (client-log-info client "Disconnected"))))

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
      (let ((threads nil))
        (log:info "Waiting for client...")
        (unwind-protect
             (loop for client-id from 1 do
               (let* ((socket (senn-ipc.unix:socket-accept server-socket))
                      (client (make-client :id client-id :socket socket)))
                 (client-log-info client "Connected")
                 (push (spawn-client-thread client client-loop-fn)
                       threads)))
          (mapc #'bordeaux-threads:destroy-thread threads)
          (senn-ipc.unix:socket-close server-socket))))))