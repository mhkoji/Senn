(defpackage :hachee.kkc.impl.markov
  (:use :cl)
  (:export :make-markov
           :make-kkc
           :char-based-cost
           :cost->probability
           :probability->cost
           :kkc-apply-user-dict)
  (:local-nicknames (:int-str :hachee.kkc.impl.markov.int-str))
  (:local-nicknames (:in-dict :hachee.kkc.impl.markov.in-dict))
  (:local-nicknames (:ex-dict :hachee.kkc.impl.markov.ex-dict)))
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
        (prev int-str:+BT+))
    (dolist (curr tokens)
      (incf cost (markov-transition-cost markov prev curr))
      (setf prev curr))
    (+ cost (markov-transition-cost markov prev int-str:+BT+))))

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
  (make-convert-entry :token int-str:+BT+
                      :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (make-convert-entry :cost 0
                      :token int-str:+BT+
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
    (dolist (dict-entry (in-dict:list-entries in-dict pron))
      (push (make-convert-entry
             :form (in-dict:entry-form dict-entry)
             :pron pron
             :cost (in-dict:entry-cost dict-entry)
             :token (in-dict:entry-token dict-entry)
             :origin hachee.kkc.origin:+vocabulary+)
            entries))
    (dolist (dict-entry (ex-dict:list-entries ex-dict pron))
      (push (make-convert-entry
             :form (ex-dict:entry-form dict-entry)
             :pron pron
             :cost (ex-dict:entry-cost dict-entry)
             :token int-str:+UT+
             :origin hachee.kkc.origin:+extended-dictionary+)
            entries))
    (when (< (length pron) 8) ;; Length up to 8
      (let ((form (hachee.ja:hiragana->katakana pron)))
        (push (make-convert-entry
               :form form
               :pron pron
               :cost (funcall char-based-cost-fn form)
               :token int-str:+UT+
               :origin hachee.kkc.origin:+out-of-dictionary+)
              entries)))
    entries))

(defun char-tokens (string char-int-str)
  (loop for ch across string
        collect (int-str:to-int char-int-str (string ch))))

(defun char-based-cost (string char-int-str char-markov char-cost-0gram)
  (let ((char-tokens (char-tokens string char-int-str)))
    (let ((UT-count (count int-str:+UT+ char-tokens :test #'=)))
      (+ (markov-sentence-cost char-markov char-tokens)
         (* UT-count char-cost-0gram)))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((in-dict (kkc-in-dict kkc))
        (ex-dict (kkc-ex-dict kkc))
        (char-markov (kkc-char-markov kkc))
        (char-int-str (kkc-char-int-str kkc))
        (char-cost-0gram (kkc-char-cost-0gram kkc)))
    (labels ((run-char-based-cost (string)
               (char-based-cost string
                                char-int-str
                                char-markov
                                char-cost-0gram)))
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
    (dolist (dict-entry (in-dict:list-entries in-dict pron))
      (push (make-lookup-item
             :form (in-dict:entry-form dict-entry)
             :origin hachee.kkc.origin:+vocabulary+)
            entries))
    entries))

(defmethod hachee.kkc.lookup:execute ((kkc kkc) (pronunciation string)
                                      &key prev next)
  (declare (ignore prev next))
  (list-lookup-items pronunciation (kkc-in-dict kkc)))

;;;

(defun user-dict-entries-in-dict-excluded (user-dict in-dict)
  (labels ((in-dict-contains-p (user-dict-entry)
             (let ((pron (hachee.kkc.user-dict:entry-pron user-dict-entry))
                   (form (hachee.kkc.user-dict:entry-form user-dict-entry)))
               (let ((in-dict-entries (in-dict:list-entries in-dict pron)))
                 (find form in-dict-entries
                       :test #'string=
                       :key #'in-dict:entry-form)))))
    (remove-if #'in-dict-contains-p
               (hachee.kkc.user-dict:dict-entries user-dict))))

(defun user-dict->ex-dict (user-dict in-dict in-dict-prob char-based-cost-fn)
  (let ((hash (make-hash-table :test #'equal)))
    (let* ((user-dict-entries (user-dict-entries-in-dict-excluded
                               user-dict in-dict))
           (each-added-probability (/ in-dict-prob
                                      (length user-dict-entries))))
      (loop for entry in user-dict-entries
            for form = (hachee.kkc.user-dict:entry-form entry)
            for pron = (hachee.kkc.user-dict:entry-pron entry) do
        (progn
          (let* ((cost (funcall char-based-cost-fn form))
                 (new-cost (probability->cost
                            (+ (cost->probability cost)
                               each-added-probability))))
            (push (ex-dict:make-entry :form form :cost new-cost)
                  (gethash pron hash))))))
    (ex-dict:make-ex-dict :hash hash)))

(defun kkc-apply-user-dict (kkc path)
  (let ((user-dict (hachee.kkc.user-dict:read-file path)))
    (when user-dict
      (let ((char-markov (kkc-char-markov kkc))
            (char-int-str (kkc-char-int-str kkc))
            (char-cost-0gram (kkc-char-cost-0gram kkc)))
        (labels ((run-char-based-cost (string)
                   (char-based-cost
                    string char-int-str char-markov char-cost-0gram)))
          (let ((ex-dict (user-dict->ex-dict user-dict
                                             (kkc-in-dict kkc)
                                             (kkc-in-dict-prob kkc)
                                             #'run-char-based-cost)))
            (setf (kkc-ex-dict kkc) ex-dict)))))))
