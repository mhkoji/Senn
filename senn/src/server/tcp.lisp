(defpackage :senn.server.tcp
  (:use :cl)
  (:export :start-server))
(in-package :senn.server.tcp)

(defstruct client id usocket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.server:read-request ((client client))
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (let ((line (read-line stream nil nil nil)))
      (log/info client "Read: ~A" line)
      line)))

(defmethod senn.server:send-response ((client client) resp)
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (write-line resp stream)
    (force-output stream)
    (log/info client "Written: ~A" resp)))

(defun spawn-client-thread (handler client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (senn.server:client-loop handler client)
     (ignore-errors
       (usocket:socket-close (client-usocket client)))
     (log/info client "Disconnected"))))

(defun start-server (create-handler-fn &key (port 5678))
  (usocket:with-server-socket
      (server-socket (usocket:socket-listen "0.0.0.0" port))
    (let ((threads nil))
      (log:info "Waiting for client...")
      (unwind-protect
           (loop for client-id from 1 do
             (let* ((socket (usocket:socket-accept server-socket))
                    (client (make-client :id client-id :usocket socket))
                    (handler (funcall create-handler-fn)))
               (push (spawn-client-thread handler client) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)))))
