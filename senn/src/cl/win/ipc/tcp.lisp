(defpackage :senn.win.ipc.tcp
  (:use :cl)
  (:export :start-server)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.ipc.tcp)

(defstruct client id usocket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.win.ipc:read-request ((client client))
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (when-let ((line (read-line stream nil nil nil)))
      (hachee.ipc.op:as-expr line))))

(defmethod senn.win.ipc:send-response ((client client) resp)
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (write-line resp stream)
    (force-output stream)))

(defmethod senn.win.ipc:read-request :around ((client client))
  (let ((req (call-next-method)))
    (log/info client "Read: ~A" req)
    req))

(defmethod senn.win.ipc:send-response :after ((client client)
                                                          resp)
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (ime client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case
         (senn.win.ipc:loop-handling-request ime client)
       (error (c)
         (log:warn "~A" c)))
     (ignore-errors
       (usocket:socket-close (client-usocket client)))
     (log/info client "Disconnected"))))

(defun start-server (ime &key (port 5678))
  (usocket:with-socket-listener (server-socket "0.0.0.0" port)
    (let ((threads nil))
      (log:info "Waiting for client...")
      (unwind-protect
           (loop for client-id from 1 do
             (let* ((socket (usocket:socket-accept server-socket))
                    (client (make-client :id client-id :usocket socket)))
               (push (spawn-client-thread ime client) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)))))
