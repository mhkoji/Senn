(defpackage :senn.fcitx.server.ipc
  (:use :cl
        :senn.ipc)
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
  (when-let ((line (client-read-line (client-socket client))))
    (log/info client "Read: ~A" line)
    (senn.op:as-expr line)))

(defmethod senn.fcitx.server:send-response ((client client) resp)
  (client-write-line (client-socket client) resp)
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (client im)
  (log/info client "New client")
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((initial-state (senn.fcitx.states:make-editing)))
       (senn.fcitx.server:loop-handling-request initial-state im client))
     (client-close (client-socket client))
     (log/info client "Disconnected"))))


(defun enter-loop (kkc &key (socket-name "/tmp/senn.sock"))
  (when (cl-fad:file-exists-p socket-name)
    (delete-file socket-name))
  (let ((im (senn.fcitx.im:make-im :kkc kkc))
        (threads nil)
        (server-socket (server-listen socket-name)))
    (log:info "Wait for client...")
    (unwind-protect
         (loop for client-id from 1
               for client-socket = (server-accept server-socket)
               for client = (make-client :id client-id :socket client-socket)
               do (push (spawn-client-thread client im) threads))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (server-close server-socket))))
