(defpackage :senn.lib.ibus
  (:use :cl)
  (:export :make-ime
           :close-ime
           :handle-request))
(in-package :senn.lib.ibus)

(defun handle-request (ime octets)
  (let ((line (babel:octets-to-string octets :encoding :utf-8)))
    (let ((output (senn.ibus.server:handle-request ime line)))
      (babel:string-to-octets output :encoding :utf-8))))
