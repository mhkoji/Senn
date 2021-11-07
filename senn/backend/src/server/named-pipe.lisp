(defpackage :senn.server.named-pipe
  (:use :cl)
  (:export :client
           :read-request
           :send-response
           :start-server))
(in-package :senn.server.named-pipe)

(defstruct client id pipe)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defun read-request (client)
  (let ((octets (hachee.ipc.named-pipe:read-file (client-pipe client))))
    (let ((line (when octets
                  (babel:octets-to-string octets :encoding :utf-8))))
      (log/info client "Read: ~A" line)
      line)))

(defun send-response (client resp)
  (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
    (hachee.ipc.named-pipe:write-file (client-pipe client) octets))
  (log/info client "Written: ~A" resp))

(defun spawn-client-thread (client-loop-fn client)
  (log/info client "Connected")
  (bordeaux-threads:make-thread
   (lambda ()
     (funcall client-loop-fn client)
     (log/info client "Disconnected"))))

(defun enter-loop (client-loop-fn &key (pipe-name "\\\\.\\Pipe\\senn"))
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
                 (let ((client (make-client :id client-id :pipe pipe)))
                   (push client clients)
                   (push (spawn-client-thread client-loop-fn client)
                         threads))))
      (mapc #'hachee.ipc.named-pipe:disconnect-and-close
            (mapcar #'client-pipe clients))
      (mapc #'bordeaux-threads:destroy-thread threads))))
