(defpackage :senn.fcitx.stateful-im.ipc
  (:use :cl)
  (:export :enter-loop)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.stateful-im.ipc)

(defstruct client id socket)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.fcitx.stateful-im:read-request ((client client))
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (when-let ((line (read-line stream nil nil nil)))
      (hachee.ipc.op:as-expr line))))

(defmethod senn.fcitx.stateful-im:send-response ((client client) resp)
  (let ((stream (hachee.ipc.unix:socket-stream (client-socket client))))
    (write-line resp stream)
    (force-output stream)))

(defmethod senn.fcitx.stateful-im:read-request :around ((client client))
  (let ((req (call-next-method)))
    (log/info client "Read: ~A" req)
    req))

(defmethod senn.fcitx.stateful-im:send-response :after ((client client) resp)
  (log/info client "Written: ~A" resp))


(defun spawn-client-thread (client ime)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((initial-state (senn.fcitx.transit.states:make-inputting)))
       (handler-case
           (senn.fcitx.stateful-im:loop-handling-request
            initial-state
            ime
            client)
         (error (c)
           (log:warn "~A" c))))
     (ignore-errors
       (hachee.ipc.unix:socket-close (client-socket client)))
     (log/info client "Disconnected"))))


(defun enter-loop (ime &key (socket-name "/tmp/senn-server-socket")
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
               (push (spawn-client-thread client ime) threads)))
        (mapc #'bordeaux-threads:destroy-thread threads)
        (hachee.ipc.unix:socket-close server-socket)))))
