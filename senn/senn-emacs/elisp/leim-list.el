(register-input-method
  "japanese-senn" "Japanese" 'quail-use-package
  "[SENN]" "Japanese input method using SENN Kana Kanji Converter"
  ; quail-use-pacakgeの中で読み込まれるライブラリ。
  ; その後、japanese-sennのquailパッケージをロード。
  ; そうして、japanese-sennがアクティブになる。
  "quail/japanese"
  (expand-file-name "./senn-quail-package.el" senn-elisp-dir))

;; インプットメソッドをSENNにする。
(set-language-info "Japanese" 'input-method "japanese-senn")
