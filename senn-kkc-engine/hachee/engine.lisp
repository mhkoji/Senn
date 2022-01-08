(defpackage :senn-kkc-engine.hachee
  (:use :cl)
  (:export :main))
(in-package :senn-kkc-engine.hachee)

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (funcall (read-from-string "asdf:system-source-directory")
                    :hachee-kkc)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc:build-kkc-simple corpus-pathnames)))

(defvar *kkc*
  (create-system-kkc))

(defun read-jsown ()
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      (jsown:parse line))))

(defun send-jsown (jsown)
  (write-line (jsown:to-json jsown) *standard-output*)
  (force-output *standard-output*))

(defun main ()
  (loop for jsown = (read-jsown) while jsown do
    (let ((j-op (jsown:val jsown "op"))
          (j-args (jsown:val jsown "args")))
      (ecase (alexandria:make-keyword j-op)
        (:convert
         (let ((entries (hachee.kkc.convert:execute
                         *kkc*
                         (jsown:val j-args "pron"))))
           (send-jsown
            (mapcar (lambda (e)
                      (jsown:new-js
                        ("form" (hachee.kkc.convert:entry-form e))
                        ("pron" (hachee.kkc.convert:entry-pron e))))
                    entries))))
        (:lookup
         (let ((items (hachee.kkc.lookup:execute
                       *kkc*
                       (jsown:val j-args "pron"))))
           (send-jsown
            (mapcar (lambda (item)
                      (jsown:new-js
                        ("form" (hachee.kkc.lookup:item-form item))))
                    items))))))))
