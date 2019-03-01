(defpackage :senn.fcitx.server.ipc
  (:use :cl)
  (:export :enter-loop)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.server.ipc)

(defstruct client id socket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.fcitx.server:read-request ((client client))
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (when-let ((line (read-line stream)))
      (hachee.ipc.op:as-expr line))))

(defmethod senn.fcitx.server:send-response ((client client) resp)
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)))

(defmethod senn.fcitx.server:read-request :around ((client client))
  (let ((req (call-next-method)))
    (log/info client "Read: ~A" req)
    req))

(defmethod senn.fcitx.server:send-response :after ((client client) resp)
  (log/info client "Written: ~A" resp))


(defun spawn-client-thread (client im)
  (log/info client "New client")
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((initial-state (senn.fcitx.states:make-editing)))
       (senn.fcitx.server:loop-handling-request initial-state im client))
     (hachee.ipc.unix:socket-close (client-socket client))
     (log/info client "Disconnected"))))


(defun enter-loop (kkc &key (socket-name "/tmp/senn-server-socket")
                            (use-abstract t))
  (when (and (not use-abstract)
             (cl-fad:file-exists-p socket-name))
    (delete-file socket-name))
  (when-let ((server-socket (hachee.ipc.unix:socket-listen
                             socket-name
                             :use-abstract use-abstract)))
    (let ((im (senn.fcitx.im:make-im :kkc kkc))
          (threads nil))
      (log:info "Wait for client...")
      (unwind-protect
           (loop for client-id from 1 do
             (let* ((socket (hachee.ipc.unix:socket-accept server-socket))
                    (client (make-client :id client-id :socket socket)))
               (push (spawn-client-thread client im) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)
        (hachee.ipc.unix:socket-close server-socket)))))
