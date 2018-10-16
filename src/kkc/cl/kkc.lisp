(defpackage :hachee.kkc
  (:use :cl)
  (:export :kkc
           :convert
           :transition-log-probability
           :kkc-dictionary
           :kkc-vocabulary
           :build-word-pron-vocabulary
           :build-word-pron-dictionary))
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

(defclass kkc ()
  ((dictionary :initform nil
               :initarg :dictionary
               :reader kkc-dictionary)
   (vocabulary :initform nil
               :initarg :vocabulary
               :reader kkc-vocabulary)))

(defgeneric transition-log-probability (kkc
                                        word-pron-string
                                        history-word-pron-string-list))
(defun lookup-dictionary (kkc pron)
  (gethash pron (kkc-dictionary kkc)))

(defun find-optimal-result (kkc prev-results curr-str)
  (let ((optimal-result nil)
        (optimal-cost (- #xffffffff)))
    (dolist (prev-result prev-results optimal-result)
      (destructuring-bind (prev-str cost-so-far strs-so-far) prev-result
        (let ((new-cost (+ cost-so-far (transition-log-probability
                                        kkc
                                        curr-str
                                        (list prev-str)))))
          (when (< optimal-cost new-cost)
            (setq optimal-cost new-cost)
            (setq optimal-result (list curr-str
                                       new-cost
                                       (cons curr-str strs-so-far)))))))))

(defun convert (kkc pronunciation)
  (let ((BOS hachee.kkc.vocabulary:+BOS+)
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
                 (or (lookup-dictionary kkc sub-pron)
                     (list (format nil "~A/~A"
                                   (hiragana->katakana sub-pron)
                                   sub-pron))))
                (prev-results (gethash start results)))
            (dolist (curr-str curr-strs)
              (push (find-optimal-result kkc prev-results curr-str)
                    (gethash end results)))))))
    (let ((result (find-optimal-result kkc (gethash length results) EOS)))
      (push result (gethash (1+ length) results))
      (let ((strs (cdr (butlast (reverse (third result))))))
        (values (format nil "~{~A~^ ~}" strs) results)))))


(defun build-word-pron-dictionary (pathnames)
  (let ((dict (make-hash-table :test #'equal)))
    (dolist (pathname pathnames dict)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (let ((pron (cadr (cl-ppcre:split "/" word-pron-str))))
            (when pron
              (setq pron (remove #\- pron))
              (pushnew word-pron-str (gethash pron dict)
                       :test #'equal))))))))


(defun build-word-pron-vocabulary (pathnames)
  (let ((vocab (hachee.kkc.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (hachee.kkc.vocabulary:add-str vocab word-pron-str))))
    vocab))
