(defpackage :hachee.kkc
  (:use :cl)
  (:import-from :alexandria
                :if-let
                :when-let
                :ensure-list
                :lastcar
                :with-gensyms)
  (:import-from :anaphora :it :aif :awhen)
  (:import-from :metabang-bind :bind))
(in-package :hachee.kkc)

(defvar *external-format* :euc-jp)
(defvar *step* 1)

(defvar *BOS* "<BOS>")
(defvar *EOS* "<EOS>")

;; utilities
(defun split (line &optional (regex " "))
  (cl-ppcre:split regex line))

(defun num-line-chars (line)
  (let ((units (cl-ppcre:split " |-|=" line)))
    (let ((words (mapcar #'first
                         (mapcar (lambda (unit)
                                   (cl-ppcre:split "/" unit))
                                 units))))
      (reduce #'+ (mapcar #'length words)))))

(defun square-distance (x y)
  (reduce #'+ (mapcar (lambda (a) (* a a)) (mapcar #'- x y))))

(defmacro with-each-line ((line filename &optional step) &body body)
  (with-gensyms (in line-count step2)
    `(let ((,step2 (or ,step *step*)))
       (with-open-file (,in ,filename
                            :external-format *external-format*)
         (format t "Reading: ~A by step ~A~%" ,filename, step2)
         (loop for ,line = (read-line ,in nil nil)
               for ,line-count from 1
           while ,line
             when (= (mod ,line-count ,step2) 0)
               do (progn ,@body))))))

(defun list-corpora (corpus-dir num &optional (suffix "word"))
  (loop for i from 1 to num collect
    (let ((filename (format nil "~2,'0d.~A" i suffix)))
      (merge-pathnames filename corpus-dir))))

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defun append1 (list obj)
  (append list (list obj)))

(defun transition-list (list size)
  (let ((length (length list)))
    (loop for begin from 0 to (1- length)
          for end from (+ begin size)
       while (<= end length)
         collect (subseq list begin end))))

(let ((table (make-hash-table :test #'equal))
      (regex (cl-ppcre:create-scanner
              "あ|い|う|え|お|か|き|く|け|こ|さ|し|す|せ|そ|た|ち|つ|て|と|な|に|ぬ|ね|の|は|ひ|ふ|へ|ほ|ま|み|む|め|も|や|ゆ|よ|ら|り|る|れ|ろ|わ|お|ん|ぁ|ぃ|ぅ|ぇ|ぉ|っ|ゃ|ゅ|ょ")))
  (loop for hiragana across "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわおんぁぃぅぇぉっゃゅょ"
        for katakana across "アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワオンァィゥェォッャュョ"
    do (setf (gethash hiragana table) katakana))
  (defun hiragana->katakana (hiragana-sequence)
    (flet ((convert (target-string start end
                     match-start match-end reg-starts reg-ends)
             (declare (ignore start end match-end reg-starts reg-ends))
             (let ((hiragana (char target-string match-start)))
               (format nil "~A" (gethash hiragana table)))))
      (cl-ppcre:regex-replace-all regex hiragana-sequence #'convert))))

(defun conditional-probability (hash sequence)
  (setq sequence (ensure-list sequence))
  (let ((history (butlast sequence)))
    (when-let ((numer (gethash sequence hash))
               (denom (gethash history  hash)))
      (/ numer (coerce denom 'double-float)))))


(defclass vocabulary ()
  ((unk :initform "UT"
        :initarg :unk
        :accessor vocabulary-unk)
   (size :initform 0
         :accessor vocabulary-size)
   (to-int-map :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-int-map)
   (to-str-map :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-str-map)))

(defun tokenize (vocab word)
  (with-accessors ((unk vocabulary-unk)
                   (map vocabulary-to-int-map)) vocab
    (aif (gethash word map)
         (values it t)
         (values (gethash unk map) nil))))

(defun unknown-word-p (vocab word)
  (not (gethash word (vocabulary-to-int-map vocab))))

(defun tokenize-all (vocab words)
  (let ((tokens nil) (unknown-words nil))
    (dolist (word words)
      (bind (((:values tok found) (tokenize vocab word)))
        (push tok tokens)
        (when (not found) (push word unknown-words))))
    (values (nreverse tokens) (nreverse unknown-words))))

(defun stringize (vocab token)
  (or (gethash token (vocabulary-to-str-map vocab))
      (error "stringize: unknown token ~A" token)))

(proclaim '(inline vocabulary-add))
(defun vocabulary-add (vocab string)
  (with-accessors ((size vocabulary-size)
                   (to-int-map vocabulary-to-int-map)
                   (to-str-map vocabulary-to-str-map)) vocab
    (when (not (gethash string to-int-map))
      (setf (gethash size to-str-map) string)
      (setf (gethash string to-int-map) size)
      (incf size))))

(defun vocabulary-integers (vocabulary)
  (alexandria:hash-table-values (vocabulary-to-int-map vocabulary)))

(defmethod initialize-instance :after ((vocab vocabulary) &key file)
  (vocabulary-add vocab (vocabulary-unk vocab))
  (vocabulary-add vocab *BOS*)
  (vocabulary-add vocab *EOS*)
  (when file
    (with-each-line (line file 1)
      (vocabulary-add vocab line))))

(defun make-word-vocab (corpora
                        &key (overlap 2)
                             (vocab (make-instance 'vocabulary)))
  (let ((appear (make-hash-table :test #'equal)))
    (loop for corpus in corpora do
      (let ((cur-appear (make-hash-table :test #'equal)))
        (with-each-line (line corpus)
          (dolist (word (split line))
            (setf (gethash word cur-appear) t)))
        (loop for word being the hash-key of cur-appear
           do (when (<= overlap (inchash word appear))
                (vocabulary-add vocab word)))))
    vocab))


;;; n-gram model
(defclass model ()
  ((gram :initform 2
         :initarg :gram
         :accessor model-gram)
   (weights :initform (list 0.2 0.8)
            :initarg :weights
            :accessor model-weights)
   (frequency :initform (make-hash-table :test #'equal)
              :initarg :frequency
              :accessor model-frequency)
   (vocabulary :initform nil
               :initarg :vocabulary
               :accessor model-vocabulary)))

(defun count-n-grams (hash gram list BT)
  (loop for i from 0 to gram do
    (when (<= 2 i) ; 2-gram 以上から先頭に履歴が必要
      (push BT list))
    (dolist (transition (transition-list list i))
      (inchash transition hash)))
      ;; 先頭の履歴を追加
  (loop for i from 1 to (1- gram) do
;  (loop for i from 2 to (1- gram) do ;; *BOS* = *EOS*の時はこっちを使う
    (let ((transition (make-list i :initial-element BT)))
      (inchash transition hash)))
  hash)

(defgeneric transition-count (model words))
(defgeneric transition-probability (model words))

(defmethod transition-count ((model model) (words list))
  (with-accessors ((gram model-gram)
                   (frequency model-frequency)
                   (vocabulary model-vocabulary)) model
    (let ((tokens (tokenize-all vocabulary (append1 words *EOS*))))
      (count-n-grams frequency gram tokens (tokenize vocabulary *BOS*)))))

(defmethod transition-probability ((model model) (words list))
  (with-accessors ((weights model-weights)
                   (frequency model-frequency)
                   (vocabulary model-vocabulary)) model
    (let ((probabilities
           (maplist (lambda (subseq)
                      (or (conditional-probability frequency subseq) 0))
                    (tokenize-all vocabulary words))))
      (log (apply #'+ (mapcar #'* weights (nreverse probabilities)))))))

(defun probability (model string)
  (with-accessors ((gram model-gram)) model
    (let* ((bos (make-list (1- gram) :initial-element *BOS*))
           (eos (list *EOS*))
           (words (nconc bos (split string) eos)))
      (loop for begin from 0 to (- (length words) gram)
            for transition = (subseq words begin (+ begin gram))
        sum (transition-probability model transition)))))

(defun cross-entropy (model test-corpus)
  (let ((logp 0) (char-count 0))
    (with-each-line (line test-corpus)
      (decf logp (probability model line))
      (incf char-count (1+ (num-line-chars line))))
    (format t "Chars:         ~A~%" char-count)
    (format t "Cross-entropy: ~A~%" (/ logp char-count (log 2)))))

(defun train-model (model corpora &key (exclusion -1))
  (setf exclusion (ensure-list exclusion))
  (prog1 model
    (loop for corpus in corpora for index from 0
      unless (member index exclusion) do
         (with-each-line (line corpus)
           (transition-count model (split line))))))

(defun eval-language-model (corpus-dir
                            &key (suffix "word")
                                 vocab-config
                                 model-config
                                 (model-class 'model))
  (setq corpus-dir (cl-fad:pathname-as-directory corpus-dir))
  (let ((corpora (list-corpora corpus-dir 10 suffix)))
    (let ((vocab (apply #'make-instance 'vocabulary vocab-config)))
      (when (not (getf vocab-config :file))
        (make-word-vocab (butlast corpora) :vocab vocab))
      (let ((model (train-model (apply #'make-instance model-class
                                       :vocabulary vocab model-config)
                                (butlast corpora))))
        (ignore-errors (cross-entropy model (lastcar corpora)))
        model))))

(defstruct classifier to-words-map to-class-map)

(defun to-class (classifier word-or-list)
  (let ((map (classifier-to-class-map classifier)))
    (if (listp word-or-list)
        (mapcar (lambda (word) (gethash word map)) word-or-list)
        (gethash word-or-list map))))

(defun read-classifier (file vocabulary)
  (let ((to-words-map (make-hash-table))
        (to-class-map (make-hash-table)))
    (with-each-line (line file)
      (bind (((class% word) (split line)))
        (let ((class (parse-integer class%))
              (token (tokenize vocabulary word)))
          (push token (gethash class to-words-map))
          (setf (gethash token to-class-map) class))))
    (make-classifier :to-words-map to-words-map
                     :to-class-map to-class-map)))

(defclass class-model (model)
  ((classifier :initform nil
               :initarg :classifier
               :accessor class-model-classifier)
   (word-frequency :initform (make-hash-table :test #'equal)
                   :initarg :word-frequency
                   :accessor class-model-word-frequency)))

(defmethod initialize-instance :after ((model class-model) &key class-file)
  (when class-file
    (setf (class-model-classifier model)
          (read-classifier class-file (model-vocabulary model)))))

(defmethod transition-count ((model class-model) (words list))
  (with-accessors ((gram model-gram)
                   (frequency model-frequency)
                   (vocabulary model-vocabulary)) model
    (let ((tokens (tokenize-all vocabulary (append1 words *EOS*))))
      (dolist (token tokens)
        (inchash token (class-model-word-frequency model)))
      (with-accessors ((classifier class-model-classifier)) model
        (count-n-grams frequency gram 
                       (to-class classifier tokens)
                       (to-class classifier (tokenize vocabulary *BOS*)))))))

(defmethod transition-probability ((model class-model) (words list))
  (with-accessors ((weights model-weights)
                   (frequency model-frequency)
                   (vocabulary model-vocabulary)) model
    (with-accessors ((classifier class-model-classifier)
                     (word-frequency class-model-word-frequency)) model
      (let ((tokens (tokenize-all vocabulary words)))
        (let ((class-sequence (to-class classifier tokens)))
          (let ((probabilities
                 (maplist (lambda (subseq)
                            (or (conditional-probability frequency subseq) 0))
                          class-sequence))
                (generation-probability
                 (/ (gethash (lastcar tokens) word-frequency)
                    (gethash (last class-sequence) frequency))))
            (log (* (apply #'+ (mapcar #'* weights (nreverse probabilities)))
                    generation-probability))))))))


;;; 仮名漢字変換 (表記・読みペア言語モデルでの変換)
(defclass word-pron-pair-kkc ()
  ((dictionary :initform nil
               :initarg :dictionary
               :accessor kkc-dictionary)
   (model :initform nil
          :initarg :model)))

(defclass word-kkc ()
  ((dictionary :initform nil
               :initarg :dictionary
               :accessor kkc-dictionary)
   (language-model :initform nil
                   :initarg :language-model
                   :accessor word-kkc-language-model)
   (kana-kanji-model :initform nil
                     :initarg :kana-kanji-model
                     :accessor word-kkc-kana-kanji-model)))


(defun lookup-kkc-dictionary (kkc pron)
  (gethash pron (kkc-dictionary kkc)))

(defun make-word-pron-pair-dictionary (corpora)
  (let ((dict (make-hash-table :test #'equal)))
    (dolist (corpus corpora dict)
      (with-each-line (line corpus)
        (dolist (unit (split line))
          (when-let ((pron% (cadr (split unit "/"))))
            (let ((pron (remove #\- pron%)))
              (pushnew unit (gethash pron dict) :test #'equal))))))))

(defmethod transition-probability ((kkc word-pron-pair-kkc) (units list))
  (with-slots (model) kkc
    (+ (transition-probability model units)
       (let ((predicted-unit (lastcar units)))
         (if (unknown-word-p (model-vocabulary model) predicted-unit)
             ;; 未知語の場合
             -100000
             0)))))

(defmethod transition-probability ((kkc word-kkc) (units list))
  (with-slots (language-model kana-kanji-model) kkc
    (let ((words (mapcar (lambda (unit) (car (split unit "/")))
                         units)))
      (+ (transition-probability language-model words)
         (let ((predicted-unit (lastcar units)))
           (bind (((word pron) (split predicted-unit "/")))
             (if-let ((word-freq (gethash word kana-kanji-model))
                      (pron-freq (gethash pron kana-kanji-model)))
               (log (/ word-freq pron-freq))
               ;; 未知語の場合
               -100000)))))))

(defun find-optimal-result (kkc sub-results current-unit)
  (let ((optimal-result nil)
        (optimal-cost (- #xffffffff)))
    (dolist (sub-result sub-results optimal-result)
      (bind (((prev-unit cost-so-far units-so-far) sub-result))
        (let ((transition (list prev-unit current-unit)))
          (let ((new-cost (+ (transition-probability kkc transition)
                             cost-so-far))
                (new-units (cons current-unit units-so-far)))
            (when (< optimal-cost new-cost)
              (setq optimal-cost new-cost)
              (setq optimal-result
                    (list current-unit new-cost new-units)))))))))

(defun convert (kkc pronunciation)
  (let ((length (length pronunciation))
        (results (make-hash-table)))
    ;; 初期化
    (push (list *BOS* 0 (list *BOS*)) (gethash 0 results))
    ;; DP
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (let ((sub-pron (subseq pronunciation start end))
              (sub-results (gethash start results)))
          (let ((units (or (lookup-kkc-dictionary kkc sub-pron)
                           (list (format nil "~A/~A"
                                         (hiragana->katakana sub-pron)
                                         sub-pron)))))
            (dolist (unit units)
              (push (find-optimal-result kkc sub-results unit)
                    (gethash end results)))))))
    (let ((result (find-optimal-result kkc (gethash length results) *EOS*)))
      (push result (gethash (1+ length) results))
      (let ((units (cdr (butlast (reverse (third result))))))
        (values (format nil "~{~A~^ ~}" units) results)))))
