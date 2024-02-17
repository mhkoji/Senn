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

(defun candidate->obj (cand)
  (alexandria:plist-hash-table
   (list
    "form" (candidate-form cand))
   :test #'equal))

(defun segment->obj (seg)
  (alexandria:plist-hash-table
   (list
    "pron" (segment-pron seg)
    "candidates" (or (mapcar #'candidate->obj
                             (segment-candidates seg))
                     #()))
   :test #'equal))

(defun handle (line kkc)
  (let ((obj (yason:parse line)))
    (let ((op (gethash "op" obj))
          (args (gethash "args" obj)))
      (ecase (alexandria:make-keyword op)
        (:convert
         (let ((pron (gethash "pron" args)))
           (let ((segs (convert kkc pron)))
             (let ((obj (or (mapcar #'segment->obj segs) #())))
               (with-output-to-string (stream)
                 (yason:with-output (stream)
                   (yason:encode obj)))))))
        (:list_candidates
         (let ((pron (gethash "pron" args)))
           (let ((cands (list-candidates kkc pron)))
             (let ((obj (or (mapcar #'candidate->obj cands) #())))
               (with-output-to-string (stream)
                 (yason:with-output (stream)
                   (yason:encode obj)))))))))))
