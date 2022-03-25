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

(defun string->candidate-jsown (str)
  (jsown:new-js
    ("form" str)))

(defun convert-entry->segment-jsown (e)
  (jsown:new-js
    ("pron" (hachee.kkc.convert:entry-pron e))
    ("candidates" (list (string->candidate-jsown
                         (hachee.kkc.convert:entry-form e))))))

(defun run (kkc in-stream out-stream)
  (loop for jsown = (read-jsown in-stream) while jsown do
    (let ((j-op (jsown:val jsown "op"))
          (j-args (jsown:val jsown "args")))
      (ecase (alexandria:make-keyword j-op)
        (:convert
         (let* ((pron (jsown:val j-args "pron"))
                (entries (hachee.kkc.convert:execute kkc pron)))
           (let ((jsown (mapcar #'convert-entry->segment-jsown
                                entries)))
             (send-jsown out-stream jsown))))
        (:list_candidates
         (let* ((pron (jsown:val j-args "pron"))
                (items (hachee.kkc.lookup:execute kkc pron)))
           (let ((jsown (mapcar (lambda (item)
                                  (string->candidate-jsown
                                   (hachee.kkc.lookup:item-form item)))
                                items)))
             (send-jsown out-stream jsown))))))))
