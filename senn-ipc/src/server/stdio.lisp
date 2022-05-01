(defpackage :senn-ipc.server.stdio
  (:use :cl)
  (:export :start-server))
(in-package :senn-ipc.server.stdio)

(defun start-server (handle-fn)
  (loop for line = (read-line *standard-input* nil nil) while line do
    (let ((resp (funcall handle-fn line)))
      (write-line resp *standard-output*)
      (force-output *standard-output*))))
