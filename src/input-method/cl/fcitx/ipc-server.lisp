(defpackage :hachee.input-method.fcitx.ipc-server
  (:use :cl :hachee.input-method.ipc)
  (:export :enter-loop)
  (:import-from :hachee.input-method.fcitx.controller
                :make-controller
                :process-client))
(in-package :hachee.input-method.fcitx.ipc-server)

(defun spawn-client-thread (client-socket)
  (let ((id (get-universal-time))
        (state (hachee.input-method.fcitx.state:make-state)))
    (log:info "[~A] New client" id)
    (bordeaux-threads:make-thread
     (lambda ()
       (let ((controller (make-controller :id id :state state)))
         (process-client controller
                         :reader (lambda ()
                                   (client-read-line client-socket))
                         :writer (lambda (line)
                                   (client-write-line client-socket line)))
         (client-close client-socket))))))

(defun enter-loop (&key (socket-name "/tmp/hachee.sock"))
  (when (cl-fad:file-exists-p socket-name)
    (delete-file socket-name))
  (let ((threads nil)
        (server-socket (server-listen socket-name)))
    (unwind-protect
         (loop for client-socket = (server-accept server-socket)
               do (push (spawn-client-thread client-socket) threads))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (server-close server-socket))))
