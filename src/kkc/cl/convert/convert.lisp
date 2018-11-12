(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :execute))
(in-package :hachee.kkc.convert)

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


(defun find-optimal-result (cost-fn prev-results curr-str)
  (let ((optimal-result nil)
        (optimal-cost (- #xffffffff)))
    (dolist (prev-result prev-results optimal-result)
      (destructuring-bind (prev-str cost-so-far strs-so-far) prev-result
        (let ((new-cost (+ cost-so-far
                           (funcall cost-fn curr-str (list prev-str)))))
          (when (< optimal-cost new-cost)
            (setq optimal-cost new-cost)
            (setq optimal-result (list curr-str
                                       new-cost
                                       (cons curr-str strs-so-far)))))))))


(defun execute (pronunciation &key cost-fn dictionary)
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
                 (or (hachee.kkc.dictionary:lookup dictionary sub-pron)
                     (list (format nil "~A/~A"
                                   (hiragana->katakana sub-pron)
                                   sub-pron))))
                (prev-results (gethash start results)))
            (dolist (curr-str curr-strs)
              (push (find-optimal-result cost-fn prev-results curr-str)
                    (gethash end results)))))))
    (let ((result (find-optimal-result cost-fn
                                       (gethash length results)
                                       EOS)))
      (push result (gethash (1+ length) results))
      (let ((strs (cdr (butlast (reverse (third result))))))
        (values (format nil "~{~A~^ ~}" strs) results)))))
