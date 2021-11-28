(defpackage :senn.bin.kkc-engine
  (:use :cl)
  (:export :main))
(in-package :senn.bin.kkc-engine)

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

(defun main ()
  (loop for line = (read-line *standard-input* nil nil) while line do
    (destructuring-bind (cmd pron)
        (cl-ppcre:split " " line)
      (ecase (alexandria:make-keyword cmd)
        (:convert
         (let ((entries (hachee.kkc.convert:execute *kkc* pron)))
           (princ (cons 0 ;; logp
                        (mapcar (lambda (e)
                                  (list
                                   (hachee.kkc.convert:entry-form e)
                                   (hachee.kkc.convert:entry-pron e)
                                   (hachee.kkc.convert:entry-origin e)))
                                entries)))
           (terpri)
           (force-output)))
        (:list_candidate
         (let ((items (hachee.kkc.lookup:execute *kkc* pron)))
           (princ (mapcar (lambda (item)
                            (list 0 ;; logp
                                  (hachee.kkc.lookup:item-form item)
                                  (hachee.kkc.lookup:item-origin item)))
                          items))
           (terpri)
           (force-output)))))))
