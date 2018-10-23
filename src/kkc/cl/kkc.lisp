(defpackage :hachee.kkc
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.vocabulary
                :to-int :to-int-or-unk)
  (:export :convert
           :make-kkc
           :build-vocabulary
           :build-dictionary
           :build-language-model))
(in-package :hachee.kkc)

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

(defun build-dictionary (pathnames)
  (let ((dict (hachee.kkc.dictionary:make-dictionary)))
    (dolist (pathname pathnames dict)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (let ((pron (cadr (cl-ppcre:split "/" word-pron-str))))
            (when pron
              (hachee.kkc.dictionary:add dict
                                         (remove #\- pron)
                                         word-pron-str))))))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.kkc.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (hachee.kkc.vocabulary:add-str vocab word-pron-str))))
    vocab))

(defun build-language-model (pathnames &key vocabulary)
  (let ((model (make-instance 'hachee.language-model.n-gram:model)))
    (let ((sentences nil))
      (dolist (pathname pathnames)
        (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
          (let ((word-pron-list (hachee.kkc.file:sentence-units sentence)))
            (push (hachee.language-model:make-sentence
                   :tokens (mapcar (curry #'to-int-or-unk vocabulary)
                                   word-pron-list))
                  sentences))))
      (hachee.language-model.n-gram:train model sentences
       :BOS (to-int vocabulary hachee.kkc.vocabulary:+BOS+)
       :EOS (to-int vocabulary hachee.kkc.vocabulary:+EOS+)))
    model))


(defstruct kkc converter dictionary)

(defun find-optimal-result (converter prev-results curr-str)
  (let ((optimal-result nil)
        (optimal-cost (- #xffffffff)))
    (dolist (prev-result prev-results optimal-result)
      (destructuring-bind (prev-str cost-so-far strs-so-far) prev-result
        (let ((new-cost (+ cost-so-far
                           (hachee.kkc.converters.converter:probability
                            converter
                            curr-str (list prev-str)))))
          (when (< optimal-cost new-cost)
            (setq optimal-cost new-cost)
            (setq optimal-result (list curr-str
                                       new-cost
                                       (cons curr-str strs-so-far)))))))))

(defun convert (kkc pronunciation)
  (let ((converter (kkc-converter kkc))
        (dictionary (kkc-dictionary kkc))
        (BOS hachee.kkc.vocabulary:+BOS+)
        (EOS hachee.kkc.vocabulary:+EOS+)
        (length (length pronunciation))
        (results (make-hash-table)))
    ;; 初期化
    (push (list BOS 0 (list BOS)) (gethash 0 results))
    ;; DP
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (let ((sub-pron (subseq pronunciation start end)))
          (let ((curr-strs
                 (or (hachee.kkc.dictionary:lookup dictionary sub-pron)
                     (list (format nil "~A/~A"
                                   (hiragana->katakana sub-pron)
                                   sub-pron))))
                (prev-results (gethash start results)))
            (dolist (curr-str curr-strs)
              (push (find-optimal-result converter prev-results curr-str)
                    (gethash end results)))))))
    (let ((result (find-optimal-result converter
                                       (gethash length results)
                                       EOS)))
      (push result (gethash (1+ length) results))
      (let ((strs (cdr (butlast (reverse (third result))))))
        (values (format nil "~{~A~^ ~}" strs) results)))))
