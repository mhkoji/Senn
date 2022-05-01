(defpackage :senn-kkc-engine.hachee.engine
  (:use :cl)
  (:export :handle))
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

(defun handle (line kkc)
  (let ((jsown (jsown:parse line)))
    (let ((j-op (jsown:val jsown "op"))
          (j-args (jsown:val jsown "args")))
      (ecase (alexandria:make-keyword j-op)
        (:convert
         (let ((pron (jsown:val j-args "pron")))
           (let ((segs (convert kkc pron)))
             (let ((jsown (mapcar #'segment->jsown segs)))
               (jsown:to-json jsown)))))
        (:list_candidates
         (let ((pron (jsown:val j-args "pron")))
           (let ((cands (list-candidates kkc pron)))
             (let ((jsown (mapcar #'candidate->jsown cands)))
               (jsown:to-json out-stream jsown)))))))))
