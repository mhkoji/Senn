(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :main))
(in-package :senn.bin.win-server)

(defstruct server
  kkc
  (ime-hash (make-hash-table :test #'equal))
  (ime-count 0))

(defun make-ime (server)
  (with-accessors ((kkc server-kkc)
                   (ime-hash server-ime-hash)
                   (ime-count server-ime-count)) server
    (let ((ime-id (format nil "ime-~A" (incf ime-count)))
          (ime (senn.win.stateful-ime:make-ime :kkc kkc)))
      (setf (gethash ime-id ime-hash) ime)
      (jsown:to-json (jsown:new-js ("id" ime-id))))))

(defun close-ime (server ime-id)
  (with-accessors ((ime-hash server-ime-hash)) server
    (remhash ime-id ime-hash)
    (jsown:to-json "OK")))

(defun handle-request (server ime-id request)
  (with-accessors ((ime-hash server-ime-hash)) server
    (let ((ime (gethash ime-id ime-hash)))
      (assert ime)
      (senn.win.server:handle-request ime request))))

(defun handle (server line)
  (let ((jsown (jsown:parse line)))
    (let ((method (jsown:val jsown "method")))
      (cond ((string= method "make-ime")
             (make-ime
              server))
            ((string= method "close-ime")
             (close-ime
              server
              (jsown:val (jsown:val jsown "params") "id")))
            ((string= method "handle-request")
             (handle-request
              server
              (jsown:val (jsown:val jsown "params") "id")
              (jsown:val (jsown:val jsown "params") "request")))
            (t
             (jsown:to-json "unknown"))))))

(defun run (engine-path)
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (unwind-protect
         (senn-ipc.server.stdio:start-server
          (let ((server (make-server :kkc kkc)))
            (lambda (line)
              (handle server line))))
      (senn.im.kkc.engine:close-kkc kkc))))

(defun main ()
  (run (merge-pathnames ".senn/kkc-engine.exe"
                        (user-homedir-pathname))))
