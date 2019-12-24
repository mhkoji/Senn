(defpackage :senn.im.net.ipc
  (:use :cl)
  (:export :start-server
           :connect)
  (:import-from :alexandria
                :when-let))
(in-package :senn.im.net.ipc)

(defstruct connection id socket)

(defmethod senn.im.net:read-message ((conn connection))
  (let ((stream (hachee.ipc.unix:socket-stream (connection-socket conn))))
    (handler-case
        (read-line stream nil nil nil)
      (sb-sys:io-timeout ()
        nil))))

(defmethod senn.im.net:send-message ((conn connection) (msg string))
  (let ((stream (hachee.ipc.unix:socket-stream (connection-socket conn))))
    (write-line msg stream)
    (force-output stream)))

(defmacro log/info (conn format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (connection-id ,conn)
             ,@args))

(defun spawn-thread (conn ime)
  (log/info conn "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (senn.im.net:loop-handling-request ime conn)
     (hachee.ipc.unix:socket-close (connection-socket conn))
     (log/info conn "Disconnected"))))

(defun start-server (ime &key (socket-name "/tmp/senn-ime-socket")
                              (use-abstract t))
  (when (and (not use-abstract)
             (cl-fad:file-exists-p socket-name))
    (delete-file socket-name))
  (when-let ((server-socket (hachee.ipc.unix:socket-listen
                             socket-name
                             :use-abstract use-abstract)))
    (let ((threads nil))
      (log:info "Waiting for connection...")
      (unwind-protect
           (loop for conn-id from 1 do
             (let* ((socket (hachee.ipc.unix:socket-accept server-socket))
                    (conn (make-connection :id conn-id :socket socket)))
               (push (spawn-thread conn ime) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)
        (hachee.ipc.unix:socket-close server-socket)))))

(defun connect (&key (socket-name "/tmp/senn-ime-socket"))
  (let ((socket (hachee.ipc.unix:connect-abstract-to
                 socket-name
                 :timeout 1)))
    (make-connection :id 0 :socket socket)))
