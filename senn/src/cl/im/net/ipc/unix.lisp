(defpackage :senn.im.net.ipc.unix
  (:use :cl)
  (:export :start-server
           :connect)
  (:import-from :alexandria
                :when-let))
(in-package :senn.im.net.ipc.unix)

(defstruct connection id socket)

(defun read-message (conn)
  (let ((stream (hachee.ipc.unix:socket-stream (connection-socket conn))))
    (handler-case
        (read-line stream nil nil nil)
      (sb-sys:io-timeout ()
        nil))))

(defun send-message (conn string)
  (let ((stream (hachee.ipc.unix:socket-stream (connection-socket conn))))
    (write-line string stream)
    (force-output stream)))

;;; Server
(defmethod senn.im.net.server:read-message ((conn connection))
  (read-message conn))

(defmethod senn.im.net.server:send-message ((conn connection) (msg string))
  (send-message conn msg))

(defmacro log/info (conn format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (connection-id ,conn)
             ,@args))

(defun spawn-thread (conn ime)
  (log/info conn "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (senn.im.net.server:loop-handling-request ime conn)
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


;;; Client
(defun connect (&key (socket-name "/tmp/senn-ime-socket"))
  (let ((socket (hachee.ipc.unix:connect-abstract-to
                 socket-name
                 :timeout 1)))
    (make-connection :id 0 :socket socket)))

(defmethod senn.im.net.client:read-message ((conn connection))
  (read-message conn))

(defmethod senn.im.net.client:send-message ((conn connection) (msg string))
  (send-message conn msg))
