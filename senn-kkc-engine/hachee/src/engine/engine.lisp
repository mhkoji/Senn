(defpackage :senn-kkc-engine.hachee.engine
  (:use :cl)
  (:export :run))
(in-package :senn-kkc-engine.hachee.engine)

(defstruct candidate
  form)

(defstruct segment
  pron candidates)

(defun convert (kkc pron)
  (let ((entries (hachee.kkc.convert:execute kkc pron)))
    (mapcar (lambda (e)
              (make-segment
               :pron (hachee.kkc.convert:entry-pron e)
               :candidates (list (make-candidate
                                  :form (hachee.kkc.convert:entry-form e)))))
            entries)))

(defun list-candidates (kkc pron)
  (let ((items (hachee.kkc.lookup:execute kkc pron)))
    (mapcar (lambda (item)
              (make-candidate
               :form (hachee.kkc.lookup:item-form item)))
            items)))

;;;

(defun candidate->jsown (cand)
  (jsown:new-js
    ("form" (candidate-form cand))))

(defun segment->jsown (seg)
  (jsown:new-js
    ("pron" (segment-pron seg))
    ("candidates" (mapcar #'candidate->jsown (segment-candidates seg)))))

(defun read-jsown (in-stream)
  (let ((line (read-line in-stream nil nil)))
    (when line
      (jsown:parse line))))

(defun send-jsown (out-stream jsown)
  (write-line (jsown:to-json jsown) out-stream)
  (force-output out-stream))

(defun run (kkc in-stream out-stream)
  (loop for jsown = (read-jsown in-stream) while jsown do
    (let ((j-op (jsown:val jsown "op"))
          (j-args (jsown:val jsown "args")))
      (ecase (alexandria:make-keyword j-op)
        (:convert
         (let ((pron (jsown:val j-args "pron")))
           (let ((segs (convert kkc pron)))
             (let ((jsown (mapcar #'segment->jsown segs)))
               (send-jsown out-stream jsown)))))
        (:list_candidates
         (let ((pron (jsown:val j-args "pron")))
           (let ((cands (list-candidates kkc pron)))
             (let ((jsown (mapcar #'candidate->jsown cands)))
               (send-jsown out-stream jsown)))))))))
