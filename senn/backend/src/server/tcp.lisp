(defpackage :senn.server.tcp
  (:use :cl)
  (:export :read-request
           :send-response
           :start-server)
  (:import-from :alexandria
                :when-let))
(in-package :senn.server.tcp)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defstruct client id usocket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod read-request ((client client))
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (when-let ((line (read-line stream nil nil nil)))
      (log/info client "Read: ~A" line)
      (hachee.ipc.op:as-expr line))))

(defmethod send-response ((client client) resp)
  (let ((stream (usocket:socket-stream (client-usocket client))))
    (write-line resp stream)
    (force-output stream)))

(defmethod send-response :after ((client client) resp)
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (client-loop-fn client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (funcall client-loop-fn client)
     (ignore-errors
       (usocket:socket-close (client-usocket client)))
     (log/info client "Disconnected"))))

(defun start-server (client-loop-fn &key (port 5678))
  (usocket:with-server-socket
      (server-socket (usocket:socket-listen "0.0.0.0" port))
    (let ((threads nil))
      (log:info "Waiting for client...")
      (unwind-protect
           (loop for client-id from 1 do
             (let* ((socket (usocket:socket-accept server-socket))
                    (client (make-client :id client-id :usocket socket)))
               (push (spawn-client-thread client-loop-fn client) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)))))
