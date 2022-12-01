(defpackage :senn.bin.kkc-proxy-unix
  (:use :cl)
  (:export :main))
(in-package :senn.bin.kkc-proxy-unix)

(defun send-recv (in-stream out-stream line)
  (write-line line in-stream)
  (force-output in-stream)
  (read-line out-stream nil nil nil))

(defun start-server (socket-name handle-fn)
  (senn-ipc.server.unix:start-server
   (lambda (client)
     (senn-ipc.server:client-loop client :handle-fn handle-fn))
   :socket-name socket-name
   :use-abstract nil))

(defun start (socket-name engine-path)
  (let ((process (sb-ext:run-program
                  engine-path nil
                  :input :stream
                  :output :stream
                  :error t
                  :external-format :utf8
                  :wait nil)))
    (unwind-protect
         (start-server socket-name
                       (lambda (req)
                         (send-recv (sb-ext:process-input process)
                                    (sb-ext:process-output process)
                                    req)))
      (sb-ext:process-kill process 9))))

(defun main ()
  (start (second sb-ext:*posix-argv*)
         (third sb-ext:*posix-argv*)))
