(defpackage :senn.im.kkc.request
  (:use :cl)
  (:export :send-line
           :convert
           :list-candidates))
(in-package :senn.im.kkc.request)

(defgeneric send-line (agent line))

(defun send-json (agent hash)
  (handler-case
      (let ((line
             (yason:with-output-to-string* ()
               (yason:encode hash))))
        (yason:parse (send-line agent line)))
    (error () nil)))

(defun obj->candidate (hash)
  (let ((form (gethash "form" hash)))
    (senn.im.kkc:make-candidate :form form)))

(defun obj->seg (hash)
  (let ((pron (gethash "pron" hash))
        (obj-cands (gethash "candidates" hash)))
    (senn.im.kkc:make-segment
     :pron pron
     :candidates (mapcar #'obj->candidate obj-cands))))

(defun convert (agent pron)
  (let ((array
         (send-json
          agent
          (alexandria:plist-hash-table
           (list
            "op" "CONVERT"
            "args" (alexandria:plist-hash-table
                    (list "pron" pron)
                    :test #'equal))
           :test #'equal))))
    (if array
        (mapcar #'obj->seg array)
        (list (senn.im.kkc:make-segment
               :pron pron
               :candidates (list (senn.im.kkc:make-candidate
                                  :form pron)))))))

(defun list-candidates (agent pron)
  (let ((array
         (send-json
          agent
          (alexandria:plist-hash-table
           (list
            "op" "LIST_CANDIDATES"
            "args" (alexandria:plist-hash-table
                    (list
                     "pron" pron)
                    :test #'equal))
           :test #'equal))))
    (mapcar #'obj->candidate array)))
