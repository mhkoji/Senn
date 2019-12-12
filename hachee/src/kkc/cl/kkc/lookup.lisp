(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :execute
           :item-form
           :item-origin))
(in-package :hachee.kkc.lookup)

(defstruct item form origin)

(defun lookup-word-forms (dict pron)
  (mapcar #'hachee.kkc.word:word-form
          (hachee.kkc.word.dictionary:lookup dict pron)))

(defun lookup-char-forms (dict pron)
  (mapcar #'hachee.kkc.word:char-form
          (hachee.kkc.word.dictionary:lookup dict pron)))

(defun execute (pronunciation &key score-fn word-dicts char-dicts)
  (let ((result-items nil))
    (labels ((push-items (forms origin)
               (dolist (form forms)
                 (let ((item (make-item :form form :origin origin)))
                   (pushnew item result-items
                            :test #'string=
                            :key #'item-form)))))
      (loop for (origin dict) in word-dicts do
        (push-items (lookup-word-forms dict pronunciation) origin))
      (when score-fn
        (labels ((item->score (item)
                   (funcall score-fn (hachee.kkc.word:make-word
                                      :form (item-form item)
                                      :pron pronunciation))))
          (setq result-items (sort result-items #'< :key #'item->score))))
      (loop for (origin dict) in char-dicts do
        (push-items (lookup-char-forms dict pronunciation) origin)))
    ;; ch_1, ..., ch_s, w_1, ..., w_t,
    ;;     where score(w_1) < score(w_2) < ... < score(w_t)
    ;; => w_t, ..., w_1, ch_s, ..., ch_1
    ;; We don't care about the order of the chars ch_1, ..., ch_s.
    (nreverse result-items)))
