(defpackage :senn-ipc.server.log
  (:use :cl)
  (:shadow :warn)
  (:export :info :warn))
(in-package :senn-ipc.server.log)

(defmacro info (&rest args)
  `(progn
     (format *error-output* ,@args)
     (format *error-output* "~%")
     (values)))


(defmacro warn (&rest args)
  `(progn
     (format *error-output* ,@args)
     (format *error-output* "~%")
     (values)))
