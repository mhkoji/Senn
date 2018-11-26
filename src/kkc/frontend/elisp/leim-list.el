(register-input-method
  "japanese-hachee" "Japanese" 'quail-use-package
  "[HACHEE]" "Japanese input method using HACHEE Kana Kanji Converter"
  ; quail-use-pacakgeの中で読み込まれるライブラリ。
  ; その後、japanese-hacheeのquailパッケージをロード。
  ; そうして、japanese-hacheeがアクティブになる。
  "quail/japanese"
  (expand-file-name "./hachee-quail-package.el" hachee-elisp-dir))

;; インプットメソッドをHACHEEにする。
(set-language-info "Japanese" 'input-method "japanese-hachee")
