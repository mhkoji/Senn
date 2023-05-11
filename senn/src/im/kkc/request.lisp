(defpackage :senn.im.kkc.request
  (:use :cl)
  (:export :send-line
           :convert
           :list-candidates))
(in-package :senn.im.kkc.request)

(defgeneric send-line (agent jsown))

(defun send-json (agent jsown)
  (let ((line (jsown:to-json jsown)))
    (jsown:parse (send-line agent line))))

(defun jsown->candidate (jsown)
  (let ((form (jsown:val jsown "form")))
    (senn.im.kkc:make-candidate :form form)))

(defun jsown->seg (jsown)
  (let ((pron (jsown:val jsown "pron"))
        (j-cands (jsown:val jsown "candidates")))
    (senn.im.kkc:make-segment
     :pron pron
     :candidates (mapcar #'jsown->candidate j-cands))))

(defun convert (agent pron)
  (let ((jsown (send-json
                agent
                (jsown:new-js
                  ("op" :convert)
                  ("args" (jsown:new-js
                            ("pron" pron)))))))
    (if jsown
        (mapcar #'jsown->seg jsown)
        (list (senn.im.kkc:make-segment
               :pron pron
               :candidates (list (senn.im.kkc:make-candidate
                                  :form pron)))))))

(defun list-candidates (agent pron)
  (let ((jsown (send-json
                agent
                (jsown:new-js
                  ("op" :list_candidates)
                  ("args" (jsown:new-js
                            ("pron" pron)))))))
    (mapcar #'jsown->candidate jsown)))
