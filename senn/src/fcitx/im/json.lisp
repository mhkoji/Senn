(defpackage :senn.fcitx.im.json
  (:use :cl :senn.fcitx.im.view)
  (:export :from-view))
(in-package :senn.fcitx.im.json)

(defgeneric from-view (view))

(defmethod from-view ((view editing))
  (let ((json (jsown:to-json
               (jsown:new-js
                 ("cursor-pos"
                  (editing-cursor-pos view))
                 ("input"
                  (editing-input view))
                 ("predictions"
                  (editing-predictions view))
                 ("prediction-index"
                  (editing-prediction-index view))
                 ("committed-input"
                  (editing-committed-input view))))))
    (format nil "EDITING ~A" json)))

(defmethod from-view ((view converting))
  (let ((json (jsown:to-json
               (jsown:new-js
                 ("forms"
                  (converting-forms view))
                 ("cursor-form-index"
                  (converting-cursor-form-index view))
                 ("cursor-form"
                  (let ((cursor-form (converting-cursor-form view)))
                    (jsown:new-js
                      ("candidates"
                       (converting-cursor-form-candidates
                        cursor-form))
                      ("candidate-index"
                       (converting-cursor-form-candidate-index
                        cursor-form)))))))))
    (format nil "CONVERTING ~A" json)))
