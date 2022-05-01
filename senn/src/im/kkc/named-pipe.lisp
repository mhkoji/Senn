(defpackage :senn.im.kkc.named-pipe
  (:use :cl)
  (:export :kkc
           :close-kkc
           :make-kkc-and-connect))
(in-package :senn.im.kkc.named-pipe)

(defstruct connection file pipe-name)

(defun connect (pipe-name)
  (let ((file (senn-ipc.named-pipe:create-client-file pipe-name)))
    (when file
      (make-connection
       :file file
       :pipe-name pipe-name))))

(defun disconnect (conn)
  (senn-ipc.named-pipe:close-file (connection-file conn)))

(defmethod senn.im.kkc.request:send-line ((conn connection)
                                          (line string))
  (let ((file (connection-file conn)))
    (let ((octets (babel:string-to-octets resp :encoding :utf-8)))
      (senn-ipc.named-pipe:write-file file octets)
      (senn-ipc.named-pipe:read-file file))))

(defclass kkc ()
  ((connection
    :initarg :connection
    :reader connection)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (handler-case (senn.im.kkc.request:convert
                 (connection kkc)
                 pron)
    (error ()
      (list (senn.im.kkc:make-segment
             :pron pron
             :candidates (list (senn.im.kkc:make-candidate
                                  :form pron)))))))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (handler-case (senn.im.kkc.request:list-candidates
                 (connection kkc)
                 pron)
    (error ()
      nil)))

(defun close-kkc (kkc)
  (disconnect (connection kkc)))

(defun make-kkc-and-connect (named-pipe)
  (let ((conn (connect named-pipe)))
    (make-instance 'kkc :connection conn)))
