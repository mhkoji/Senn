(defpackage :hachee.kkc.persist
  (:use :cl)
  (:export :save-object
           :load-object
           :do-save-into-zip
           :load-from-zip))
(in-package :hachee.kkc.persist)

(defgeneric save-object (obj stream))
(defgeneric load-object (type stream))

(defmacro do-save-into-zip ((add-entry pathname) &body body)
  `(zip:with-output-to-zipfile (writer ,pathname)
     (labels ((add-file (name data-string)
                (flexi-streams:with-input-from-sequence
                    (data-stream (flexi-streams:string-to-octets
                                  data-string
                                  :external-format :utf-8))
                  (zip:write-zipentry writer name data-stream
                                      :file-write-date
                                      (get-universal-time)))))
       (let ((added-entry-types nil))
         (labels ((,add-entry (filename obj &optional (type (class-of obj)))
                    (let ((data-string (with-output-to-string (s)
                                         (save-object obj s))))
                      (add-file filename data-string))
                    (push (list filename type) added-entry-types)))
           (progn ,@body)
           (add-file "types.txt" (with-output-to-string (s)
                                   (print added-entry-types s))))))))

(defun load-from-zip (pathname)
  (zip:with-zipfile (zip pathname)
    (labels ((read-from-file (filename read-from-stream-fn)
               (let ((entry (zip:get-zipfile-entry filename zip)))
                 (when entry
                   (let ((octets (zip:zipfile-entry-contents entry)))
                     (let ((string (babel:octets-to-string
                                    octets
                                    :encoding :utf-8)))
                       (with-input-from-string (s string)
                         (funcall read-from-stream-fn s)))))))
             (load-from-file (type filename)
               (read-from-file filename (lambda (s) (load-object type s)))))
      (loop for (filename type) in (read-from-file "types.txt" #'read)
            collect (list filename (load-from-file type filename))))))
