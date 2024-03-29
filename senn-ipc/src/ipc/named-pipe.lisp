(defpackage :senn-ipc.named-pipe
  (:use :cl)
  (:export :create-server-pipe
           :connect
           :disconnect-and-close

           :create-client-file
           :close-file

           :read-file
           :write-file))
(in-package :senn-ipc.named-pipe)

(defun h= (handle1 handle2)
  (= (cffi:pointer-address handle1)
     (cffi:pointer-address handle2)))

;;; server
(defun create-server-pipe (pipe-name)
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


;;; client
(defun create-client-file (pipe-name)
  (let ((file (win32:create-file
               pipe-name
               (logior win32:+generic-read+
                       win32:+generic-write+)
               0
               (cffi:null-pointer)
               win32:+open-existing+
               win32:+file-attribute-normal+
               (cffi:null-pointer))))
    (if (h= file win32:+invalid-handle-value+)
        nil
        file)))

(defun close-file (file)
  (win32:close-handle file))


;;; Windows File API
;; https://docs.microsoft.com/en-us/windows/win32/ipc/multithreaded-pipe-server
(defun read-file (file)
  (let ((buf-size 4096))
    (cffi:with-foreign-object (buf :unsigned-char buf-size)
      (cffi:with-foreign-object (bytes-read-ptr 'win32:dword)
        (let ((success-p (win32:read-file file
                                          buf
                                          buf-size
                                          bytes-read-ptr
                                          (cffi:null-pointer))))
          (let ((bytes-read (cffi:mem-ref bytes-read-ptr 'win32:dword)))
            (if (or (not success-p) (= bytes-read 0))
                nil
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
