(defpackage :senn-kkc.request
  (:use :cl)
  (:export :send-line
           :convert
           :list-candidates))
(in-package :senn-kkc.request)

(defgeneric send-line (agent jsown))

(defun send-json (agent jsown)
  (let ((line (jsown:to-json jsown)))
    (jsown:parse (send-line agent line))))

(defun jsown->candidate (jsown)
  (let ((form (jsown:val jsown "form")))
    (senn-kkc:make-candidate :form form)))

(defun convert (agent pron)
  (let ((j-segs (send-json
                 agent
                 (jsown:new-js
                   ("op" :convert)
                   ("args" (jsown:new-js
                             ("pron" pron)))))))
    (mapcar (lambda (j-seg)
              (let ((pron (jsown:val j-seg "pron"))
                    (j-cands (jsown:val j-seg "candidates")))
                (senn-kkc:make-segment
                 :pron pron
                 :candidates (mapcar #'jsown->candidate j-cands))))
            j-segs)))

(defun list-candidates (agent pron)
  (let ((j-cands (send-json
                  agent
                  (jsown:new-js
                    ("op" :list_candidates)
                    ("args" (jsown:new-js
                              ("pron" pron)))))))
    (mapcar #'jsown->candidate j-cands)))
