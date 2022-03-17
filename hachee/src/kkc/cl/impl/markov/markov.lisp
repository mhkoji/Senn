(defpackage :hachee.kkc.impl.markov
  (:use :cl)
  (:export :make-markov
           :make-kkc))
(in-package :hachee.kkc.impl.markov)

(defstruct markov
  cost-1gram
  cost-2gram)

(defun markov-transition-cost (markov prev curr)
  (or (gethash (list prev curr)
               (markov-cost-2gram markov))
      (aref (markov-cost-1gram markov) curr)))

(defun markov-sentence-cost (markov tokens)
  (let ((cost 0)
        (prev hachee.kkc.impl.markov.int-str:+BT+))
    (dolist (curr tokens)
      (incf cost (markov-transition-cost markov prev curr))
      (setf prev curr))
    (+ cost (markov-transition-cost
             markov prev hachee.kkc.impl.markov.int-str:+BT+))))

(defstruct kkc
  word-markov
  in-dict
  char-markov
  char-int-str
  char-cost-0gram)

;;;

(defstruct convert-entry
  form
  pron
  cost
  token
  origin)

(defmethod hachee.kkc.convert:entry-form ((e convert-entry))
  (convert-entry-form e))

(defmethod hachee.kkc.convert:entry-pron ((e convert-entry))
  (convert-entry-pron e))

(defmethod hachee.kkc.convert:entry-origin ((e convert-entry))
  (convert-entry-origin e))

(defmethod hachee.kkc.convert:convert-begin-entry ((kkc kkc))
  (make-convert-entry :token hachee.kkc.impl.markov.int-str:+BT+
                      :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (make-convert-entry :cost 0
                      :token hachee.kkc.impl.markov.int-str:+BT+
                      :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc))
  (let ((word-markov (kkc-word-markov kkc)))
    (lambda (curr-entry prev-entry)
      (let ((cost (+
                   ;; 表記の遷移コスト
                   (markov-transition-cost word-markov
                                           (convert-entry-token prev-entry)
                                           (convert-entry-token curr-entry))
                   ;; 現在の表記が与えられた時の読みのコスト
                   (convert-entry-cost curr-entry))))
        (- cost)))))

(defun list-convert-entries (pron in-dict unknown-word-cost-fn)
  (let ((entries nil))
    (dolist (dict-entry (hachee.kkc.impl.markov.in-dict:list-entries
                         in-dict pron))
      (push (make-convert-entry
             :form (hachee.kkc.impl.markov.in-dict:entry-form dict-entry)
             :pron pron
             :cost (hachee.kkc.impl.markov.in-dict:entry-cost dict-entry)
             :token (hachee.kkc.impl.markov.in-dict:entry-token dict-entry)
             :origin hachee.kkc.origin:+vocabulary+)
            entries))
    ;; Add unknown word entry
    (when (< (length pron) 8) ;; Length up to 8
      (push (make-convert-entry
             :form (hachee.ja:hiragana->katakana pron)
             :pron pron
             :cost (funcall unknown-word-cost-fn pron)
             :token hachee.kkc.impl.markov.int-str:+UT+
             :origin hachee.kkc.origin:+out-of-dictionary+)
            entries))
    entries))

(defun char-tokens (pron char-int-str)
  (loop for ch across pron
        collect (hachee.kkc.impl.markov.int-str:to-int
                 char-int-str
                 (string ch))))

(defun unknown-word-cost (pron char-markov char-int-str char-cost-0gram)
  (let ((char-tokens (char-tokens pron char-int-str)))
    (let ((UT-count (count hachee.kkc.impl.markov.int-str:+UT+
                           char-tokens
                           :test #'=)))
      (+ (markov-sentence-cost char-markov char-tokens)
         (* UT-count char-cost-0gram)))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((in-dict (kkc-in-dict kkc))
        (char-markov (kkc-char-markov kkc))
        (char-int-str (kkc-char-int-str kkc))
        (char-cost-0gram (kkc-char-cost-0gram kkc)))
    (labels ((run-unknown-word-cost (pron)
               (unknown-word-cost
                pron char-markov char-int-str char-cost-0gram)))
      (lambda (pron)
        (list-convert-entries pron in-dict #'run-unknown-word-cost)))))

;;;

(defstruct lookup-item form origin)

(defmethod hachee.kkc.lookup:item-form ((item lookup-item))
  (lookup-item-form item))

(defmethod hachee.kkc.lookup:item-origin ((item lookup-item))
  (lookup-item-origin item))

(defun list-lookup-items (pron in-dict)
  (let ((entries nil))
    (dolist (dict-entry (hachee.kkc.impl.markov.in-dict:list-entries
                         in-dict pron))
      (push (make-lookup-item
             :form (hachee.kkc.impl.markov.in-dict:entry-form dict-entry)
             :origin hachee.kkc.origin:+vocabulary+)
            entries))
    entries))

(defmethod hachee.kkc.lookup:execute ((kkc kkc) (pronunciation string)
                                      &key prev next)
  (declare (ignore prev next))
  (list-lookup-items pronunciation (kkc-in-dict kkc)))
