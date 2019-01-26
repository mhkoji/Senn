(defpackage :senn.win.named-pipe
  (:use :cl))
(in-package :senn.win.named-pipe)

(defun create-named-pipe ()
  (win32:create-named-pipe
   "\\\\.\\Pipe\\senn"
   win32:+pipe-access-duplex+
   (logior win32:+pipe-type-byte+
           win32:+pipe-wait+)
   1
   0
   0
   100
   (cffi:null-pointer)))


(defun listen (pipe)
  (win32:connect-named-pipe pipe (cffi:null-pointer))
  (cffi:with-foreign-object (buf :char 256)
    (cffi:with-foreign-object (raed-size 'win32:dword)
      (win32:read-file p buf 256 read-size (cffi:null-pointer))
      (loop for i below 256 collect (cffi:mem-ref buf :char i)))))
