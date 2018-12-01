(defpackage :hachee.kkc.convert
  (:use :cl :hachee.kkc.word)
  (:export :execute))
(in-package :hachee.kkc.convert)

(defun calculate-score (score-fn curr-word prev-word)
  (funcall score-fn curr-word prev-word))

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


(defstruct node word score-so-far prev-node)

(defun find-optimal-result (score-fn prev-nodes curr-word)
  (let ((optimal-node nil)
        (optimal-score (- #xffffffff)))
    (dolist (prev-node prev-nodes)
      (let ((new-score-so-far
             (+ (node-score-so-far prev-node)
                (calculate-score score-fn curr-word (node-word prev-node)))))
        (when (< optimal-score new-score-so-far)
          (setq optimal-score new-score-so-far)
          (setq optimal-node (make-node :word curr-word
                                        :prev-node prev-node
                                        :score-so-far new-score-so-far)))))
    optimal-node))

(defun backtrack (node acc)
  (if (null (node-prev-node node))
      acc
      (backtrack (node-prev-node node)
                 (cons (node-word node) acc))))


(defvar *BOS-node*
  (make-node :word hachee.kkc.word.vocabulary:+BOS+
             :prev-node nil
             :score-so-far 0))

(defun execute (pronunciation &key score-fn dictionary)
  (let ((length (length pronunciation))
        (results (make-hash-table)))
    ;; 初期化
    (push *BOS-node* (gethash 0 results))
    ;; DP
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (let ((sub-pron (subseq pronunciation start end)))
          (let ((prev-nodes
                 (gethash start results))
                (curr-words
                 (or (hachee.kkc.word.dictionary:lookup dictionary sub-pron)
                     (list (make-word
                            :pron sub-pron
                            :form (hiragana->katakana sub-pron))))))
            (dolist (curr-word curr-words)
              (push (find-optimal-result score-fn prev-nodes curr-word)
                    (gethash end results)))))))
    (let ((last-node (find-optimal-result
                      score-fn
                      (gethash length results)
                      hachee.kkc.word.vocabulary:+EOS+)))
      ;; skip EOS
      (backtrack (node-prev-node last-node) nil))))
