(defpackage :senn-kkc-engine.hachee
  (:use :cl)
  (:export :run))
(in-package :senn-kkc-engine.hachee)

(defun read-jsown (in-stream)
  (let ((line (read-line in-stream nil nil)))
    (when line
      (jsown:parse line))))

(defun send-jsown (out-stream jsown)
  (write-line (jsown:to-json jsown) out-stream)
  (force-output out-stream))

(defun run (kkc in-stream out-stream)
  (let ((entries nil))
    (loop for jsown = (read-jsown in-stream) while jsown do
      (let ((j-op (jsown:val jsown "op"))
            (j-args (jsown:val jsown "args")))
        (ecase (alexandria:make-keyword j-op)
          (:convert
           (let ((pron (jsown:val j-args "pron")))
             (setf entries (hachee.kkc.convert:execute kkc pron))
             (let ((jsown (mapcar
                           (lambda (e)
                             (jsown:new-js
                               ("form" (hachee.kkc.convert:entry-form e))
                               ("pron" (hachee.kkc.convert:entry-pron e))))
                           entries)))
               (send-jsown out-stream jsown))))
          (:list_candidates
           (let ((jsown
                  (let ((index (jsown:val j-args "index")))
                    (when (and (<= 0 index) (< index (length entries)))
                      (let* ((pron (hachee.kkc.convert:entry-pron
                                    (elt entries index)))
                             (items (hachee.kkc.lookup:execute kkc pron)))
                        (mapcar
                         (lambda (item)
                           (jsown:new-js
                             ("form" (hachee.kkc.lookup:item-form item))))
                         items))))))
             (send-jsown out-stream jsown))))))))
