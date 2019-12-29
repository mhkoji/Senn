(defpackage :senn.fcitx.net.ipc
  (:use :cl)
  (:export :start-server)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.net.ipc)

(defstruct client id socket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.fcitx.net:read-request ((client client))
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (when-let ((line (read-line stream nil nil nil)))
      (hachee.ipc.op:as-expr line))))

(defmethod senn.fcitx.net:send-response ((client client) resp)
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)))

(defmethod senn.fcitx.net:read-request :around ((client client))
  (let ((req (call-next-method)))
    (log/info client "Read: ~A" req)
    req))

(defmethod senn.fcitx.net:send-response :after ((client client) resp)
  (log/info client "Written: ~A" resp))


(defun spawn-client-thread (ime handler-fn client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case (funcall handler-fn ime client)
       (error (c)
         (log:warn "~A" c)))
     (ignore-errors
       (hachee.ipc.unix:socket-close (client-socket client)))
     (log/info client "Disconnected"))))


(defun start-server (ime handler-fn
                     &key (socket-name "/tmp/senn-server-socket")
                          (use-abstract t))
  (when (and (not use-abstract)
             (cl-fad:file-exists-p socket-name))
    (delete-file socket-name))
  (when-let ((server-socket (hachee.ipc.unix:socket-listen
                             socket-name
                             :use-abstract use-abstract)))
    (let ((threads nil))
      (log:info "Waiting for client...")
      (unwind-protect
           (loop for client-id from 1 do
             (let* ((socket (hachee.ipc.unix:socket-accept server-socket))
                    (client (make-client :id client-id :socket socket)))
               (push (spawn-client-thread ime handler-fn client)
                     threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)
        (hachee.ipc.unix:socket-close server-socket)))))
