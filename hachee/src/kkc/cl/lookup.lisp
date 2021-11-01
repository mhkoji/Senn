(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :item-unit
           :item-form
           :item-origin

           :lookup-score-fn
           :lookup-word-dictionary
           :lookup-char-dictionary
           :execute))
(in-package :hachee.kkc.lookup)

(defstruct item unit origin)

(defun item-form (item)
  (hachee.kkc.dictionary:unit-form (item-unit item)))

(defun execute-internal (pronunciation &key score-fn word-dict char-dict)
  (let ((result-items nil))
    (labels ((push-items (dictionary-entries)
               (dolist (dictionary-entry dictionary-entries)
                 (let ((item (make-item
                              :unit (hachee.kkc.dictionary:entry-unit
                                     dictionary-entry)
                              :origin (hachee.kkc.dictionary:entry-origin
                                       dictionary-entry))))
                   (pushnew item result-items
                            :test #'string=
                            :key #'item-form)))))
      (push-items (hachee.kkc.dictionary:lookup word-dict pronunciation))
      (when score-fn
        (setq result-items (sort result-items #'< :key score-fn)))
      (push-items (hachee.kkc.dictionary:lookup char-dict pronunciation)))
    ;; ch_1, ..., ch_s, w_1, ..., w_t,
    ;;     where score(w_1) < score(w_2) < ... < score(w_t)
    ;; => w_t, ..., w_1, ch_s, ..., ch_1
    ;; We don't care about the order of the chars ch_1, ..., ch_s.
    (nreverse result-items)))

(defgeneric lookup-score-fn (mixin prev next))
(defgeneric lookup-word-dictionary (mixin))
(defgeneric lookup-char-dictionary (mixin))

(defun execute (lookup pronunciation &key prev next)
  (execute-internal pronunciation
   :score-fn (when (and next prev)
               (lookup-score-fn lookup prev next))
   :word-dict (lookup-word-dictionary lookup)
   :char-dict (lookup-char-dictionary lookup)))
