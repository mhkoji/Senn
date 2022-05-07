(defpackage :senn-ipc.server.named-pipe
  (:use :cl)
  (:export :start-server))
(in-package :senn-ipc.server.named-pipe)

(defstruct client pipe)

(defmethod senn-ipc.server:client-read-line ((client client))
  (let ((octets (senn-ipc.named-pipe:read-file (client-pipe client))))
    (let ((line (when octets
                  (babel:octets-to-string octets :encoding :utf-8))))
      (senn-ipc.server.log:info "Read: ~A" line)
      line)))

(defmethod senn-ipc.server:client-send-line ((client client) resp)
  (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
    (senn-ipc.named-pipe:write-file (client-pipe client) octets))
  (senn-ipc.server.log:info "Written: ~A" resp))

(defun spawn-thread (pipe client-loop-fn name)
  (bordeaux-threads:make-thread
   (lambda ()
     (senn-ipc.server.log:info "Connected")
     (funcall client-loop-fn (make-client :pipe pipe))
     (senn-ipc.named-pipe:disconnect-and-close pipe)
     (senn-ipc.server.log:info "Disconnected"))
   :name name))

(defun start-server (client-loop-fn &key (pipe-name "\\\\.\\Pipe\\senn"))
  (let ((pipes nil)
        (threads nil))
    (unwind-protect
         (loop for id from 1
               for pipe = (senn-ipc.named-pipe:create-server-pipe
                           pipe-name)
               while pipe do
                 (progn
                   (senn-ipc.server.log:info "Waiting for pipe...")
                   (senn-ipc.named-pipe:connect pipe)
                   (push pipe pipes)
                   (push (spawn-thread pipe
                                       client-loop-fn
                                       (format nil "process-thread-~d" id))
                         threads)))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (mapc #'senn-ipc.named-pipe:disconnect-and-close pipes))))
