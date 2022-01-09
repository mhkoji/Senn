(defpackage :senn.im.kkc.request
  (:use :cl)
  (:export :send-line
           :convert
           :lookup))
(in-package :senn.im.kkc.request)

(defgeneric send-line (agent jsown))

(defun send-json (agent jsown)
  (let ((line (jsown:to-json jsown)))
    (jsown:parse (send-line agent line))))

(defun convert (agent pron)
  (let ((j-segs (send-json
                 agent
                 (jsown:new-js
                   ("op" :convert)
                   ("args" (jsown:new-js
                             ("pron" pron)))))))
    (mapcar (lambda (j-seg)
              (let ((form (jsown:val j-seg "form"))
                    (pron (jsown:val j-seg "pron")))
                (senn.im.segment:make-segment
                 :pron pron
                 :candidates (list (senn.im.segment:make-candidate
                                    :form form))
                 :current-index 0
                 :has-more-candidates-p t)))
            j-segs)))

(defun lookup (agent pron)
  (let ((j-cands (send-json
                  agent
                  (jsown:new-js
                    ("op" :lookup)
                    ("args" (jsown:new-js
                              ("pron" pron)))))))
    (mapcar (lambda (j-cand)
              (let ((form (jsown:val j-cand "form")))
                (senn.im.segment:make-candidate
                 :form form)))
            j-cands)))
