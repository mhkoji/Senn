(defpackage :hachee.kkc.impl.markov
  (:use :cl)
  (:export :make-markov
           :make-kkc
           :char-based-cost
           :cost->probability
           :probability->cost
           :kkc-apply-user-dict))
(in-package :hachee.kkc.impl.markov)

(defvar *log-probability-to-cost-multiple* #x10000)

(defun probability->cost (prob)
  (- (* (log prob) *log-probability-to-cost-multiple*)))

(defun cost->probability (cost)
  (exp (/ (- cost) *log-probability-to-cost-multiple*)))

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
  in-dict-prob
  ex-dict
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

(defun list-convert-entries (pron in-dict ex-dict char-based-cost-fn)
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
    (when (and (null entries)
               (< (length pron) 8)) ;; Length up to 8
      (push (make-convert-entry
             :form (hachee.ja:hiragana->katakana pron)
             :pron pron
             :cost (funcall char-based-cost-fn pron)
             :token hachee.kkc.impl.markov.int-str:+UT+
             :origin hachee.kkc.origin:+out-of-dictionary+)
            entries))
    (dolist (dict-entry (hachee.kkc.impl.markov.ex-dict:list-entries
                         ex-dict pron))
      (push (make-convert-entry
             :form (hachee.kkc.impl.markov.ex-dict:entry-form dict-entry)
             :pron pron
             :cost (hachee.kkc.impl.markov.ex-dict:entry-cost dict-entry)
             :token hachee.kkc.impl.markov.int-str:+UT+
             :origin hachee.kkc.origin:+extended-dictionary+)
            entries))
    entries))

(defun char-tokens (pron char-int-str)
  (loop for ch across pron
        collect (hachee.kkc.impl.markov.int-str:to-int
                 char-int-str
                 (string ch))))

(defun char-based-cost (pron char-int-str char-markov char-cost-0gram)
  (let ((char-tokens (char-tokens pron char-int-str)))
    (let ((UT-count (count hachee.kkc.impl.markov.int-str:+UT+
                           char-tokens
                           :test #'=)))
      (+ (markov-sentence-cost char-markov char-tokens)
         (* UT-count char-cost-0gram)))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((in-dict (kkc-in-dict kkc))
        (ex-dict (kkc-ex-dict kkc))
        (char-markov (kkc-char-markov kkc))
        (char-int-str (kkc-char-int-str kkc))
        (char-cost-0gram (kkc-char-cost-0gram kkc)))
    (labels ((run-char-based-cost (pron)
               (char-based-cost
                pron char-int-str char-markov char-cost-0gram)))
      (lambda (pron)
        (list-convert-entries
         pron in-dict ex-dict #'run-char-based-cost)))))

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

;;;

(defun user-dict->ex-dict (user-dict in-dict-prob char-based-cost-fn)
  (let ((hash (make-hash-table :test #'equal))
        (each-added-probability
         (/ in-dict-prob
            (hachee.kkc.user-dict:dict-size user-dict))))
    (loop for entry in (hachee.kkc.user-dict:dict-entries user-dict)
          for form = (hachee.kkc.user-dict:entry-form entry)
          for pron = (hachee.kkc.user-dict:entry-pron entry) do
      (progn
        (let* ((cost (funcall char-based-cost-fn pron))
               (new-cost (probability->cost
                          (+ (cost->probability cost)
                             each-added-probability))))
          (push (hachee.kkc.impl.markov.ex-dict:make-entry
                 :form form :cost new-cost)
                (gethash pron hash)))))
    (hachee.kkc.impl.markov.ex-dict:make-ex-dict :hash hash)))

(defun kkc-apply-user-dict (kkc path)
  (let ((user-dict (hachee.kkc.user-dict:read-file path)))
    (when user-dict
      (let ((in-dict-prob (kkc-in-dict-prob kkc))
            (char-markov (kkc-char-markov kkc))
            (char-int-str (kkc-char-int-str kkc))
            (char-cost-0gram (kkc-char-cost-0gram kkc)))
        (labels ((run-char-based-cost (pron)
                   (char-based-cost
                    pron char-int-str char-markov char-cost-0gram)))
          (let ((ex-dict (user-dict->ex-dict user-dict
                                             in-dict-prob
                                             #'run-char-based-cost)))
            (setf (kkc-ex-dict kkc) ex-dict)))))))
