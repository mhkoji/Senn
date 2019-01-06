(defpackage :senn.fcitx.server.ipc
  (:use :cl
        :senn.ipc)
  (:export :enter-loop)
  (:import-from :senn.fcitx.server.client
                :make-client
                :client-id))
(in-package :senn.fcitx.server.ipc)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defun spawn-client-thread (client client-socket)
  (log/info client "New client")
  (bordeaux-threads:make-thread
   (lambda ()
     (senn.fcitx.server:process-client client
      :reader (lambda ()
                (let ((expr (client-read-line client-socket)))
                  (log/info client "Read: ~A" expr)
                  expr))
      :writer (lambda (line)
                (client-write-line client-socket line)
                (log/info client "Written: ~A" line)))
     (client-close client-socket)
     (log/info client "Disconnected"))))


(defun enter-loop (kkc &key (socket-name "/tmp/senn.sock"))
  (when (cl-fad:file-exists-p socket-name)
    (delete-file socket-name))
  (let ((threads nil)
        (server-socket (server-listen socket-name)))
    (log:info "Wait for client...")
    (unwind-protect
         (loop for id from 1
               for client-socket = (server-accept server-socket)
               do (let ((client (make-client :id id :kkc kkc)))
                    (push (spawn-client-thread client client-socket)
                          threads)))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (server-close server-socket))))
