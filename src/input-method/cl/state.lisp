(defpackage :hachee.input-method.state
  (:use :cl
        :hachee.input-method.op
        :hachee.input-method.ipc)
  (:export :server-loop))
(in-package :hachee.input-method.state)

(defun process-client (client-socket)
  (let ((id (get-universal-time)))
    (log:info "[~A] New client" id)
    (loop for line = (client-read-line client-socket) do
      (if (not line)
          (sleep 1)
          (let ((expr (as-expr line)))
            (log:info "[~A] expr: ~A" id expr)
            (ecase (expr-op expr)
              (:process-key
               (let ((result (hachee.input-method:romaji->hiragana
                              (expr-arg expr "state")
                              (expr-arg expr "code"))))
                 (log:info "~A" result)
                 (client-write-line
                  client-socket
                  (format nil "~A~%" result))))))))))


(defun server-loop (&key (socket-name "/tmp/hachee.sock"))
  (let ((threads nil)
        (server-socket (server-listen socket-name)))
    (unwind-protect
         (loop for client-socket = (server-accept server-socket)
               do (push (bordeaux-threads:make-thread
                         (lambda () (process-client client-socket)))
                        threads))
      (mapc #'bordeaux-threads:destroy-thread threads)
      (server-close server-socket))))
