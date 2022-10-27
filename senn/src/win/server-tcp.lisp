(defpackage :senn.win.server
  (:use :cl)
  (:export :with-ime
           :handle-request))
(in-package :senn.win.server)

(defstruct ime stream)

(defmacro with-ime ((ime host port) &body body)
  `(usocket:with-client-socket (socket stream ,host ,port)
     (let ((,ime (make-ime :stream stream)))
       ,@body)))

(defun handle-request (ime req)
  (let ((stream (ime-stream ime)))
    (log:info "-> [~A]" req)
    (write-line req stream)
    (force-output stream)
    (loop for resp = (read-line stream nil nil nil)
          when (string/= resp "")
            return (progn
                     (log:info "<- [~A]" resp)
                     (format nil "~A~%" resp)))))
