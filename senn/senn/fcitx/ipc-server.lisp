(defpackage :senn.fcitx.ipc-server
  (:use :cl :senn.ipc)
  (:export :enter-loop)
  (:import-from :senn.fcitx.controller
                :make-controller
                :controller-id
                :process-client))
(in-package :senn.fcitx.ipc-server)

(defmacro log/info (controller format-str &rest args)
  `(log:info ,(concatenate 'string "[~A]: " format-str)
             (controller-id ,controller)
             ,@args))

(defun spawn-client-thread (controller client-socket)
  (log/info controller "New client")
  (bordeaux-threads:make-thread
   (lambda ()
     (process-client controller
      :reader (lambda ()
                (let ((expr (client-read-line client-socket)))
                  (log/info controller "Read: ~A" expr)
                  expr))
      :writer (lambda (line)
                (client-write-line client-socket line)
                (log/info controller "Written: ~A" line)))
     (client-close client-socket)
     (log/info controller "Disconnected"))))


(defun enter-loop (&key (socket-name "/tmp/senn.sock"))
  (when (cl-fad:file-exists-p socket-name)
    (delete-file socket-name))
  (let ((threads nil)
        (server-socket (server-listen socket-name))
        (kkc (hachee.kkc:create-kkc
              (cl-fad:list-directory
               (merge-pathnames
                "src/kkc/data/aozora/word-pron-utf8/"
                (asdf:system-source-directory :hachee))))))
    (log:info "Wait for client...")
    (unwind-protect
         (loop for id from 1
               for client-socket = (server-accept server-socket)
               do (let ((controller (make-controller :id id :kkc kkc)))
                    (push (spawn-client-thread controller
                                               client-socket)
                          threads)))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (server-close server-socket))))
