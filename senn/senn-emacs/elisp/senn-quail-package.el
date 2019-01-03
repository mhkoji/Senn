;; SENNをleimとして使うためのquailのパッケージ定義。
;; quail/japaneseを使う
(require 'quail)
(require 'senn)

(setq quail-japanese-use-double-n t)

;; 現在の入力文字列を、SENNを使って仮名漢字交じり列に変換する。
(defun quail-japanese-kanji-senn ()
  (interactive)
  (when (= (char-before (overlay-end quail-conv-overlay)) ?n)
    ;; 最後の文字が`n'の場合。`ん'に変換する。
    (goto-char (1- (overlay-end quail-conv-overlay)))
    (insert ?ん)
    (delete-char 1))
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
         (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
        (let ((result (senn-region from (+ from len))))
          (move-overlay quail-conv-overlay from (point))
          (setq quail-conversion-str (buffer-substring from (point)))
          (if (= (+ from result) (point))
              (setq quail-converting nil))
          (setq quail-translating nil))
      (set-marker from nil))))


(quail-define-package
 "japanese-senn" "Japanese" "[SENN]" nil
 "Japanese input method for using SENN Kana Kanji Converter."
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation
 '(("\C-t"  . quail-japanese-toggle-kana)
   (" "      . quail-japanese-kanji-senn)
   ("\C-m"   . quail-no-conversion)
   ([return] . quail-no-conversion)))

(let ((transliteration-rules
       (append
        quail-japanese-transliteration-rules
        '(("A" "Ａ") ("B" "Ｂ") ("C" "Ｃ") ("D" "Ｄ") ("E" "Ｅ")
          ("F" "Ｆ") ("G" "Ｇ") ("H" "Ｈ") ("I" "Ｉ") ("J" "Ｊ")
          ("K" "Ｋ") ("L" "Ｌ") ("M" "Ｍ") ("N" "Ｎ") ("O" "Ｏ")
          ("P" "Ｐ") ("Q" "Ｑ") ("R" "Ｒ") ("S" "Ｓ") ("T" "Ｔ")
          ("U" "Ｕ") ("V" "Ｖ") ("W" "Ｗ") ("X" "Ｘ") ("Y" "Ｙ")
          ("Z" "Ｚ"))
        '(("la" "ぁ") ("li" "ぃ") ("lu" "ぅ") ("le" "ぇ") ("lo" "ぉ")))))
  (dolist (elt transliteration-rules)
    (quail-defrule (car elt) (nth 1 elt))))
