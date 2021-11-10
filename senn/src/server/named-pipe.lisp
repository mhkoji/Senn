(defpackage :senn.server.named-pipe
  (:use :cl)
  (:export :run))
(in-package :senn.server.named-pipe)

(defstruct client id pipe)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.server:read-request ((client client))
  (let ((octets (hachee.ipc.named-pipe:read-file (client-pipe client))))
    (let ((line (when octets
                  (babel:octets-to-string octets :encoding :utf-8))))
      (log/info client "Read: ~A" line)
      line)))

(defmethod senn.server:send-response ((client client) resp)
  (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
    (hachee.ipc.named-pipe:write-file (client-pipe client) octets))
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (handler client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (senn.server:client-loop handler client)
     (log/info client "Disconnected"))))

(defun run (create-handler-fn &key (pipe-name "\\\\.\\Pipe\\senn"))
  (let ((threads nil)
        (clients nil))
    (unwind-protect
         (loop
            for client-id from 1
            for pipe = (hachee.ipc.named-pipe:create pipe-name)
            while pipe
            do (progn
                 (log:info "Waiting for client...")
                 (hachee.ipc.named-pipe:connect pipe)
                 (let ((client (make-client :id client-id :pipe pipe))
                       (handler (funcall create-handler-fn)))
                   (push client clients)
                   (push (spawn-client-thread handler client) threads))))
      (mapc #'hachee.ipc.named-pipe:disconnect-and-close
            (mapcar #'client-pipe clients))
      (mapc #'bordeaux-threads:destroy-thread threads))))
