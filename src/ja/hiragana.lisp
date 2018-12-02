(in-package :hachee.ja)

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
