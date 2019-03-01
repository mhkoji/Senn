(defpackage :hachee.ipc.named-pipe
  (:use :cl)
  (:export :create
           :connect
           :disconnect

           :read-file
           :write-file))
(in-package :hachee.ipc.named-pipe)

(defun create (pipe-name)
  (win32:create-named-pipe
   pipe-name
   win32:+pipe-access-duplex+
   (logior win32:+pipe-type-byte+
           win32:+pipe-wait+)
   1
   0
   0
   100
   (cffi:null-pointer)))

(defun connect (pipe)
  (win32:connect-named-pipe pipe (cffi:null-pointer)))

(defun disconnect (pipe)
  (win32:disconnect-named-pipe pipe))


;;; Windows File API

(defun read-file (file)
  (let ((buf-size 4096))
    (cffi:with-foreign-object (buf :unsigned-char buf-size)
      (cffi:with-foreign-object (bytes-read-ptr 'win32:dword)
        (win32:read-file file
                         buf
                         buf-size
                         bytes-read-ptr
                         (cffi:null-pointer))
        (let ((bytes-read (cffi:mem-ref bytes-read-ptr 'win32:dword)))
          (let ((octets-in-list
                 (loop for i from 0 below bytes-read
                       collect (cffi:mem-ref buf :unsigned-char i))))
            (make-array bytes-read
                        :element-type '(unsigned-byte 8)
                        :initial-contents octets-in-list
                        :adjustable nil)))))))

(defun write-file (file octets)
  (cffi:with-foreign-object (buf :unsigned-char (length octets))
    (loop for i below (length octets)
          do (setf (cffi:mem-aref buf :unsigned-char i) (aref octets i)))
    (cffi:with-foreign-object (size 'win32:dword)
      (win32:write-file file buf (length octets) size (cffi:null-pointer)))))
