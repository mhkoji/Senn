(defun hiragana->katakana (hiragana-string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'japanese-katakana
			 (vector-to-list hiragana-string)))))

(defun string->hankaku (string)
  (apply #'concat
	 (mapcar #'(lambda (ch)
		     (if (stringp ch) ch (char-to-string ch)))
		 (mapcar #'japanese-hankaku (vector-to-list string)))))

(defun string->zenkaku (string)
  (apply #'concat
	 (mapcar #'(lambda (ch)
		     (if (stringp ch) ch (char-to-string ch)))
		 (mapcar #'japanese-zenkaku
			 (vector-to-list string)))))

(defun string-downcase (string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'downcase
			 (vector-to-list string)))))

(defun string-upcase (string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'upcase
			 (vector-to-list string)))))

(defun string-capitalize (string)
  (if (string-equal string "")
      ""
    (concat (string-upcase (subseq string 0 1))
	    (string-downcase (subseq string 1)))))

(defun string-zenkaku-downcase (string)
  (string->zenkaku (string-downcase string)))

(defun string-zenkaku-upcase (string)
  (string->zenkaku (string-upcase string)))

(defun string-zenkaku-capitalize (string)
  (string->zenkaku (string-capitalize string)))

(provide 'hachee-util)
