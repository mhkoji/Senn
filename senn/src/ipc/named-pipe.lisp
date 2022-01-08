(defpackage :senn.ipc.named-pipe
  (:use :cl)
  (:export :create
           :connect
           :disconnect-and-close

           :read-file
           :write-file))
(in-package :senn.ipc.named-pipe)

(defun h= (handle1 handle2)
  (= (cffi:pointer-address handle1)
     (cffi:pointer-address handle2)))


(defun create (pipe-name)
  (let ((pipe (win32:create-named-pipe
               pipe-name
               win32:+pipe-access-duplex+
               (logior win32:+pipe-type-byte+
                       win32:+pipe-wait+)
               win32:+pipe-unlimited-instances+
               0
               0
               100
               (cffi:null-pointer))))
    (if (h= pipe win32:+invalid-handle-value+)
        nil
        pipe)))


(defun connect (pipe)
  (win32:connect-named-pipe pipe (cffi:null-pointer)))

(defun disconnect-and-close (pipe)
  (win32:disconnect-named-pipe pipe)
  (win32:close-handle pipe))


;;; Windows File API

(defun read-file (file)
  (let ((buf-size 4096))
    (cffi:with-foreign-object (buf :unsigned-char buf-size)
      (cffi:with-foreign-object (bytes-read-ptr 'win32:dword)
        (let ((return-value (win32:read-file file
                                             buf
                                             buf-size
                                             bytes-read-ptr
                                             (cffi:null-pointer))))
          (if (and (numberp return-value)
                   (= return-value 0))
              nil
              ;; succeeds
              (let ((bytes-read
                     (cffi:mem-ref bytes-read-ptr 'win32:dword)))
                (let ((octet-list
                       (loop for i from 0 below bytes-read
                          collect (cffi:mem-ref buf :unsigned-char i))))
                  (make-array bytes-read
                              :element-type '(unsigned-byte 8)
                              :initial-contents octet-list
                              :adjustable nil)))))))))

(defun write-file (file octets)
  (cffi:with-foreign-object (buf :unsigned-char (length octets))
    (loop for i below (length octets)
          do (setf (cffi:mem-aref buf :unsigned-char i) (aref octets i)))
    (cffi:with-foreign-object (size 'win32:dword)
      (win32:write-file file buf (length octets) size (cffi:null-pointer)))))
