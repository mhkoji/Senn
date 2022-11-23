(defpackage :hachee.kkc.impl.markov
  (:use :cl)
  (:export :make-markov
           :build-task
           :build-kkc
           :kkc-set-ex-dict)
  (:local-nicknames (:int-str :hachee.kkc.impl.markov.int-str))
  (:local-nicknames (:in-dict :hachee.kkc.impl.markov.in-dict))
  (:local-nicknames (:ex-dict :hachee.kkc.impl.markov.ex-dict)))
(in-package :hachee.kkc.impl.markov)

(defstruct markov
  cost-1gram
  cost-2gram)

(defun markov-transition-cost (markov prev curr)
  (or (gethash (list prev curr)
               (markov-cost-2gram markov))
      (aref (markov-cost-1gram markov) curr)))

(defun markov-1gram-cost (markov token)
  (aref (markov-cost-1gram markov) token))

(defmacro do-as-sentence (((prev curr) tokens) &body body)
  `(labels ((run (,prev ,curr)
              ,@body))
     (let ((prev int-str:+BT+))
       (dolist (curr ,tokens)
         (run prev curr)
         (setq prev curr))
       (run prev int-str:+BT+))))

(defun char-tokens (string char-int-str)
  (loop for ch across string
        collect (int-str:to-int char-int-str (string ch))))

(defun char-based-cost (string char-int-str char-markov char-cost-0gram)
  (let ((char-tokens (char-tokens string char-int-str))
        (cost 0))
    (do-as-sentence ((prev curr) char-tokens)
      (let ((transition-cost
             (markov-transition-cost char-markov prev curr))
            (ut-cost
             (if (= curr int-str:+UT+) char-cost-0gram 0)))
        (incf cost (+ transition-cost ut-cost))))
    cost))

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
  (let ((origin (convert-entry-origin e)))
    (case origin
      (:in-dict hachee.kkc.origin:+vocabulary+)
      (:ex-dict hachee.kkc.origin:+extended-dictionary+)
      (:unk     hachee.kkc.origin:+out-of-dictionary+)
      (t        origin))))


(defstruct task
  markov
  in-dict
  cb1 cb2 c11 c21)

(defstruct kkc
  word-markov
  in-dict
  in-dict-prob
  ex-dict
  char-markov
  char-int-str
  char-cost-0gram
  task)

(defmethod hachee.kkc.convert:convert-begin-entry ((kkc kkc))
  (make-convert-entry :token int-str:+BT+
                      :origin :in-dict))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (make-convert-entry :cost 0
                      :token int-str:+BT+
                      :origin :in-dict))

(defun entry-generation-cost (word-markov curr-entry prev-entry)
  "Compute the generation cost of curr-entry given prev-entry using word-markov"
  (let ((curr-token (convert-entry-token curr-entry))
        (prev-token (convert-entry-token prev-entry)))
    (+
     ;; 表記の遷移コスト
     (markov-transition-cost word-markov prev-token curr-token)
     ;; 現在の表記が与えられた時の読みのコスト
     (convert-entry-cost curr-entry))))

(defun task-interpolation (cb word-markov-cost task-markov-cost)
  (- (+ cb word-markov-cost)
     (hachee.kkc.impl.markov.cost:logadd (- task-markov-cost
                                            cb
                                            word-markov-cost))))

;; mysterious threashold
(defvar *threshold*
  (1- #x7fffffff))

(defun entry-generation-cost-with-task (task-markov
                                        cb1 cb2 c11 c21
                                        char-based-cost-fn
                                        word-markov
                                        curr-entry prev-entry)
  (+
   ;; 表記の遷移コスト
   (let* ((curr-origin (convert-entry-origin curr-entry))
          (curr-token (convert-entry-token curr-entry))
          (prev-token (convert-entry-token prev-entry))
          (prev-token-or-UT
            (if (eql (convert-entry-origin prev-entry) :task-in-dict)
                int-str:+UT+
                prev-token)))
     (cond ((= curr-token int-str:+BT+)
            ;; curr-entry is end-entry if curr-token is int-str:+BT+
            (let ((wm-cost (markov-transition-cost word-markov
                                                   prev-token-or-UT
                                                   curr-token)))
              (if (= (markov-1gram-cost task-markov prev-token) *threshold*)
                  (task-interpolation
                   cb1 wm-cost (markov-transition-cost
                                task-markov prev-token curr-token))
                  (task-interpolation
                   cb2 wm-cost (+ (markov-1gram-cost task-markov
                                                     curr-token)
                                  (- c11) c21)))))
           ((eql curr-origin :in-dict)
            (let ((wm-cost (markov-transition-cost word-markov
                                                   prev-token-or-UT
                                                   curr-token)))
              (if (= (markov-1gram-cost task-markov prev-token) *threshold*)
                  (task-interpolation
                   cb2 wm-cost (+ (markov-1gram-cost task-markov
                                                     curr-token)
                                  (- c11) c21))
                  (task-interpolation
                   cb1 wm-cost (markov-transition-cost
                                task-markov prev-token curr-token)))))
           ((eql curr-origin :task-in-dict)
            (let ((wm-cost (+ (markov-transition-cost word-markov
                                                      prev-token-or-UT
                                                      int-str:+UT+)
                              (funcall char-based-cost-fn
                                       (convert-entry-form curr-entry)))))
              (if (= (markov-1gram-cost task-markov prev-token) *threshold*)
                  (task-interpolation
                   cb1 wm-cost (markov-transition-cost
                                task-markov prev-token curr-token))
                  (task-interpolation
                   cb2 wm-cost (+ (markov-1gram-cost task-markov
                                                     curr-token)
                                  (- c11) c21)))))
           ((or (eql curr-origin :ex-dict)
                (eql curr-origin :unk))
            (let ((wm-cost (markov-transition-cost word-markov
                                                   prev-token-or-UT
                                                   int-str:+UT+)))
              (if (= (markov-1gram-cost task-markov prev-token) *threshold*)
                  (task-interpolation
                   cb1 wm-cost (markov-transition-cost
                                task-markov prev-token curr-token))
                  (task-interpolation
                   cb2 wm-cost (+ (markov-1gram-cost task-markov
                                                     int-str:+UT+)
                                  (- c11) c21)))))
           (t
            (markov-transition-cost
             word-markov prev-token curr-token))))
   ;; 現在の表記が与えられた時の読みのコスト
   (convert-entry-cost curr-entry)))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc))
  (let ((task (kkc-task kkc))
        (word-markov (kkc-word-markov kkc)))
    (if task
        (let ((char-markov (kkc-char-markov kkc))
              (char-int-str (kkc-char-int-str kkc))
              (char-cost-0gram (kkc-char-cost-0gram kkc)))
          (labels ((run-char-based-cost (string)
                     (char-based-cost string
                                      char-int-str
                                      char-markov
                                      char-cost-0gram)))
            (lambda (curr-entry prev-entry)
              (- (entry-generation-cost-with-task
                  (task-markov task)
                  (task-cb1 task) (task-cb2 task)
                  (task-c11 task) (task-c21 task)
                  #'run-char-based-cost
                  word-markov
                  curr-entry prev-entry)))))
        (lambda (curr-entry prev-entry)
          (- (entry-generation-cost word-markov curr-entry prev-entry))))))

(defun list-convert-entries (pron in-dict ex-dict char-based-cost-fn
                             task-in-dict)
  (let ((entries nil))
    (dolist (dict-entry (in-dict:list-entries in-dict pron))
      (push (make-convert-entry
             :form (in-dict:entry-form dict-entry)
             :pron pron
             :cost (in-dict:entry-cost dict-entry)
             :token (in-dict:entry-token dict-entry)
             :origin :in-dict)
            entries))
    (dolist (dict-entry (ex-dict:list-entries ex-dict pron))
      (push (make-convert-entry
             :form (ex-dict:entry-form dict-entry)
             :pron pron
             :cost (ex-dict:entry-cost dict-entry)
             :token int-str:+UT+
             :origin :ex-dict)
            entries))
    (when (< (length pron) 8) ;; Length up to 8
      (let ((form (hachee.ja:hiragana->katakana pron)))
        (push (make-convert-entry
               :form form
               :pron pron
               :cost (funcall char-based-cost-fn form)
               :token int-str:+UT+
               :origin :unk)
              entries)))
    (when task-in-dict
      (dolist (dict-entry (in-dict:list-entries task-in-dict pron))
        (push (make-convert-entry
               :form (in-dict:entry-form dict-entry)
               :pron pron
               :cost (in-dict:entry-cost dict-entry)
               :token (in-dict:entry-token dict-entry)
               :origin :task-in-dict)
              entries)))
    entries))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((in-dict (kkc-in-dict kkc))
        (ex-dict (kkc-ex-dict kkc))
        (char-markov (kkc-char-markov kkc))
        (char-int-str (kkc-char-int-str kkc))
        (char-cost-0gram (kkc-char-cost-0gram kkc))
        (task-in-dict (when (kkc-task kkc)
                        (task-in-dict (kkc-task kkc)))))
    (labels ((run-char-based-cost (string)
               (char-based-cost string
                                char-int-str
                                char-markov
                                char-cost-0gram)))
      (lambda (pron)
        (list-convert-entries
         pron in-dict ex-dict #'run-char-based-cost task-in-dict)))))

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

(defun kkc-set-ex-dict (kkc ex-dict-source)
  (let ((char-markov (kkc-char-markov kkc))
        (char-int-str (kkc-char-int-str kkc))
        (char-cost-0gram (kkc-char-cost-0gram kkc)))
    (labels ((run-char-based-cost (string)
               (char-based-cost
                string char-int-str char-markov char-cost-0gram)))
      (let ((ex-dict (hachee.kkc.impl.markov.ex-dict-builder:build
                      ex-dict-source
                      (kkc-in-dict kkc)
                      (kkc-in-dict-prob kkc)
                      #'run-char-based-cost)))
        (setf (kkc-ex-dict kkc) ex-dict)))))

;;;

(defvar *empty-ex-dict*
  (ex-dict:make-ex-dict :hash (make-hash-table)))

(defun in-dict-prob (in-dict char-int-str char-markov char-cost-0gram)
  (let ((sum-prob 0))
    (in-dict:do-entries (entry in-dict)
      (let* ((form (in-dict:entry-form entry))
             (cost (char-based-cost
                    form char-int-str char-markov char-cost-0gram))
             (prob (hachee.kkc.impl.markov.cost:->probability cost)))
        (incf sum-prob prob)))
    sum-prob))

(defun char-cost-0gram (char-int-str)
  (let ((pron-alphabet-size 6878)
        (char-int-str-size  (int-str:int-str-size char-int-str)))
    ;; 2 for UT and BT
    (let ((unk-char-size (- pron-alphabet-size (- char-int-str-size 2))))
      (assert (< 0 unk-char-size))
      (hachee.kkc.impl.markov.cost:<-probability (/ 1 unk-char-size)))))

(defun build-kkc (&key word-markov
                       char-int-str
                       char-markov
                       in-dict
                       task)
  (let* ((char-cost-0gram (char-cost-0gram char-int-str))
         (in-dict-prob    (in-dict-prob in-dict
                                        char-int-str
                                        char-markov
                                        char-cost-0gram)))
    (make-kkc :word-markov  word-markov
              :in-dict      in-dict
              :in-dict-prob in-dict-prob
              :ex-dict      *empty-ex-dict*
              :char-markov  char-markov
              :char-int-str char-int-str
              :char-cost-0gram char-cost-0gram
              :task         task)))

(defun build-task (&key markov in-dict coeffs)
  (destructuring-bind (l10 l11 l12 l20 l21) coeffs
    (declare (ignore l12))
    (make-task
     :markov markov
     :in-dict in-dict
     :cb1 (hachee.kkc.impl.markov.cost:<-probability l10)
     :cb2 (hachee.kkc.impl.markov.cost:<-probability l20)
     :c11 (hachee.kkc.impl.markov.cost:<-probability l11)
     :c21 (hachee.kkc.impl.markov.cost:<-probability l21))))
