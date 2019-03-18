(defpackage :senn.win.server.ipc
  (:use :cl)
  (:export :enter-loop)
  (:import-from :alexandria
                :if-let
                :when-let))
(in-package :senn.win.server.ipc)

(defstruct client id pipe)

(defmacro log/info (client format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (client-id ,client)
             ,@args))

(defmethod senn.win.server:read-request ((client client))
  (when-let ((octets (hachee.ipc.named-pipe:read-file (client-pipe client))))
    (let ((string (babel:octets-to-string octets :encoding :utf-8)))
      (hachee.ipc.op:as-expr string))))

(defmethod senn.win.server:send-response ((client client) (resp string))
  (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
    (hachee.ipc.named-pipe:write-file (client-pipe client) octets)))

(defmethod senn.win.server:read-request :around ((client client))
  (let ((req (call-next-method)))
    (log/info client "Read: ~A" req)
    req))

(defmethod senn.win.server:send-response :after ((client client) resp)
  (log/info client "Written: ~A" resp))


(defun enter-loop (kkc &key (pipe-name "\\\\.\\Pipe\\senn"))
  (if-let ((pipe (hachee.ipc.named-pipe:create pipe-name)))
    (progn
      (log:info "Wait for client...")
      (hachee.ipc.named-pipe:connect pipe)
      (let ((client (make-client :id 0 :pipe pipe)))
        (log/info client "New client")
        (unwind-protect
             (senn.win.server:loop-handling-request
              (senn.win.states:make-editing)
              (senn.win.im:make-im :kkc kkc)
              client)
          (hachee.ipc.named-pipe:disconnect-and-close pipe))
        (log/info client "Disconnected")))
    (log:info "Could not create pipe")))
