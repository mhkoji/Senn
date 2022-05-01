(defpackage :senn-ipc.server.named-pipe
  (:use :cl)
  (:export :run))
(in-package :senn-ipc.server.named-pipe)

(defstruct client id pipe)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn-ipc.server:client-read-line ((client client))
  (let ((octets (senn-ipc.named-pipe:read-file (client-pipe client))))
    (let ((line (when octets
                  (babel:octets-to-string octets :encoding :utf-8))))
      (log/info client "Read: ~A" line)
      line)))

(defmethod senn-ipc.server:client-send-line ((client client) resp)
  (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
    (senn-ipc.named-pipe:write-file (client-pipe client) octets))
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (client client-loop-fn)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (funcall client-loop-fn client)
     (log/info client "Disconnected"))))

(defun run (client-loop-fn &key (pipe-name "\\\\.\\Pipe\\senn"))
  (let ((threads nil)
        (clients nil))
    (unwind-protect
         (loop
            for client-id from 1
            for pipe = (senn-ipc.named-pipe:create-server-pipe pipe-name)
            while pipe
            do (progn
                 (log:info "Waiting for client...")
                 (senn-ipc.named-pipe:connect pipe)
                 (let ((client (make-client :id client-id :pipe pipe)))
                   (push client clients)
                   (push (spawn-client-thread client client-loop-fn)
			 threads))))
      (mapc #'senn-ipc.named-pipe:disconnect-and-close
            (mapcar #'client-pipe clients))
      (mapc #'bordeaux-threads:destroy-thread threads))))
