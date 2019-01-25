;; Commentary:
;;
;; かな漢字変換エンジンSENNをemacsから使うためのプログラム
;; leimとして使う。自前のquailパッケージを用意している。

(require 'cl)
(require 'hi-lock)

(require 'senn-data)

(defvar senn-input-method-title "S"
  "String denoting SENN input method is working, which is shown on mode line.")

(defvar senn-server-bin-list
  (list (expand-file-name "../bin/server" senn-elisp-dir)
        (expand-file-name "../bin/server.ros" senn-elisp-dir))
  "senn-server の PATH")

(defvar senn-working-buffer " *senn*")

(defvar senn-server-process nil
  "senn-serverのプロセス。")

(defvar senn-accept-timeout 50)

(defvar senn-overlay-list nil
  "使用するoverlayはこれで一括で管理する。")

(defvar senn-option-selector-chars "asdfghjkl;"
  "候補を選ぶときの文字。")

(defvar senn-preedit-begin-mark "|")

(defvar senn-preedit-delim-mark "|")

(defvar senn-origin-string-alist
  '((IV . "S") (TK . "T") (EX . "E")))

(defvar senn-origin-overlay-alist
  '((IV . hi-yellow) (TK . hi-blue) (EX . hi-red)))

(defvar senn-option-output-size
  (length senn-option-selector-chars)
  "いくつの候補をミニバッファに表示するか")

(defvar senn-message-select-count 2
  "senn-next-prev-counterがこの数字以上になると、ミニバッファに候補のリストxが表示される。")

(defvar senn-conversion-data nil
  "出力のためにいろいろな情報を集めるための変数。")


(defvar senn-keymap
  (let ((map (make-sparse-keymap))
        (len (length senn-option-selector-chars))
        (i 0))
    (while (< i len)
      (define-key map
        (char-to-string
         (aref senn-option-selector-chars i))
        'senn-select-from-chars)
      (setq i (1+ i)))
    (define-key map " " 'senn-next-option)
    (define-key map "\C-n" 'senn-next-option)
    (define-key map "\C-p" 'senn-prev-option)
    (define-key map "\C-f" 'senn-next-select)
    (define-key map "\C-b" 'senn-prev-select)
    (define-key map "\C-i" 'senn-make-word-shorter)
    (define-key map "\C-o" 'senn-make-word-longer)
    (define-key map "\C-a" 'senn-move-beginning-of-selects)
    (define-key map "\C-e" 'senn-move-end-of-selects)
    (define-key map "H"    'senn-to-hiragana)
    (define-key map "K"    'senn-to-katakana)
    (define-key map "\C-h" 'senn-to-hiragana)
    (define-key map "\C-k" 'senn-to-katakana)
    (define-key map "\C-w" 'senn-move-top-of-options)
    (define-key map "\C-[" 'senn-move-bottom-of-options)
    (define-key map "" 'senn-cancel)
    (define-key map "" 'senn-accept)
    map))

;; これで、ベクタがリストになる
;; Emacs Lispのリファレンスマニュアルにも書いてある正当なやり方
(defun senn-vector->list (vector)
  (append vector nil))

(defun senn-process-sentinel (proc stat)
  (message "%s" stat))

(defun senn-process-running-p (proc)
  (and (processp proc) (eq (process-status proc) 'run)))

(defun senn-kill-server ()
  (when (senn-process-running-p senn-server-process)
    (kill-process senn-server-process)))

;; serverを起動し、プロセスを返す。
(defun senn-invoke-server ()
  (loop for bin in senn-server-bin-list
        when (file-exists-p bin)
        for proc = (start-process "senn-kkc" senn-working-buffer bin)
        when (senn-process-running-p proc) return proc))

;; serverが走ってなかったら起動。
(defun senn-ensure-server-invoked ()
  (unless (senn-process-running-p senn-server-process)
    (setq senn-server-process (senn-invoke-server))
    (set-process-sentinel senn-server-process 'senn-process-sentinel)))

(defun senn-send-recv-command (command)
  (senn-ensure-server-invoked)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer senn-working-buffer)
          (erase-buffer)
          (process-send-string senn-server-process command)
          (while (= (buffer-size) 0)
            (accept-process-output nil 0 senn-accept-timeout))
          ; 文字列として返す
          (buffer-string))
      (set-buffer old-buffer))))

(defun senn-api-call (op arg-plist)
  (let ((json (json-encode `((op . ,op) (args . ,arg-plist)))))
    (json-read-from-string
     (senn-send-recv-command (concat json "\n")))))

(defun senn-api-convert (string 1st-boundary-index)
  (senn-api-call "convert"
                   `(("text" . ,string)
                     ("1st-boundary-index" . ,1st-boundary-index))))

(defun senn-api-lookup-words (string)
  (senn-api-call "lookup"
                   `(("text" . ,string))))

(defun senn-api-quit ()
  (senn-api-call "quit" nil))

(defun senn-assoc-get (key alist)
  (cdr (assoc key alist)))

(defun senn-word->option (word-alist)
  (make-senn-option :form (senn-assoc-get 'form word-alist)
                      :origin 'IV
                      :logP 0))


(defun senn-word->select (word-alist)
  (make-senn-select :pron (senn-assoc-get 'pron word-alist)
                    :options (vector (senn-word->option word-alist))
                    :option-count 1
                    :curr-option-idx 0
                    :more-options-p t
                    :selectors nil
                    :np-count 0))

(defun senn-convert (pron)
  (let ((words (senn-api-convert pron nil)))
    (let ((selects (mapcar #'senn-word->select words)))
      (make-senn-conversion :pron pron
                            :mode 'kana-kanji
                            :logP 0
                            :selects (apply #'vector selects)
                            :select-count (length selects)
                            :curr-select-idx 0))))

(defun senn-reconvert-with-boundary (conversion diff)
  (let ((selects (senn-conv-selects conversion))
        (curr-select-idx (senn-conv-curr-select-idx conversion)))
    (let ((selects1
           (subseq selects 0 curr-select-idx))
          (selects2
           (mapcar #'senn-word->select
                   (senn-api-convert
                    ;; 現在のselectから残りの読み全部
                    (loop for select across (subseq selects curr-select-idx)
                          concat (senn-select-pron select))
                    (funcall diff (length
                                   (senn-select-pron
                                    (aref selects curr-select-idx))))))))
      (let ((new-selects (vconcat selects1 selects2)))
        (make-senn-conversion
         :pron (senn-conv-pron conversion)
         :mode 'kana-kanji
         :logP 0
         :selects new-selects
         :select-count (length new-selects)
         :curr-select-idx curr-select-idx)))))

(defun senn-lookup-options (pron)
  (mapcar #'senn-word->option (senn-api-lookup-words pron)))


;;; オーバーレイに関する関数。
(defun senn-delete-all-overlays ()
  (dolist (overlay senn-overlay-list)
    (delete-overlay overlay))
  (setq senn-overlay-list nil))


;; overlayをつくって設定した後、プットしてsenn-overlay-listにプッシュする。
(defun senn-put-and-register-overlay (begin end prop value)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay prop value)
    (push overlay senn-overlay-list)))


;;; 現在のフレーズの変換候補をエコーエリアに表示できるようにするための関数。
(defun senn-origin->string (org)
  (let ((pair (assoc org senn-origin-string-alist)))
    (if pair (concat " (" (cdr pair) ")") "")))

(defun senn-get-option-selector-char (index)
  (char-to-string (aref senn-option-selector-chars index)))

(defun senn-option->message (option selector-char focusedp)
  (let ((form (senn-option-form option))
        (origin (senn-origin->string (senn-option-origin option))))
    (if focusedp
        (let ((msg (format " %s: [%s]%s " selector-char form origin)))
          (put-text-property (position ?\[ msg) (1+ (position ?\] msg))
                             'face 'highlight msg)
          msg)
      (format " %s: %s%s " selector-char form origin))))

(defun senn-collect-option-messages (curr-option-idx selector)
  (let ((region (senn-selector-region selector))
        (options (senn-selector-options selector)))
    (let ((index-from-region-begin (- curr-option-idx (car region))))
      (loop for option in options
            for i from 0
            for focusedp      = (= i index-from-region-begin)
            for selector-char = (senn-get-option-selector-char i)
            collect (senn-option->message
                     option selector-char focusedp)))))


;; フレーズに従って、エコーエリアに表示させる候補文字列を構成する関数。
(defun senn-select->minibuf-string (select)
  (let ((selectors (senn-select-selectors select)))
    (when selectors
      (let ((curr-option-idx (senn-select-curr-option-idx select)))
        (let ((selector (senn-find-selector curr-option-idx selectors)))
          (apply 'concat (senn-collect-option-messages
                          curr-option-idx selector)))))))

;; エコーエリアに現在の変換候補を表示。
(defun senn-message-select (select)
  (let ((message-string (senn-select->minibuf-string select)))
    (when message-string
      (let ((message-log-max nil)) ;; *Message*バッファにログを残さない
        (message message-string)))))


;;; キーが押されたら呼ばれるコマンド
(defun senn-move-top-of-options ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (senn-with-current-select curr-select senn-conversion-data
    (setf (senn-select-curr-option-idx curr-select) 0)
    (senn-message-select curr-select)))

(defun senn-move-bottom-of-options ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (senn-with-current-select curr-select senn-conversion-data
    (setf (senn-select-curr-option-idx curr-select)
          (1- (senn-select-option-count curr-select)))
    (senn-message-select curr-select)))


(defun senn-adjoin-options (option other-options)
  (let ((form (senn-option-form option)))
    (apply #'vector
           (cons option
                 (remove-if ;候補に重複があれば、取り除く
                  #'(lambda (o) (string= form (senn-option-form o)))
                  other-options)))))

(defun senn-ensure-more-options-looked-up (select)
  (when (senn-select-more-options-p select)
    (let ((pron (senn-select-pron select))
          (option (senn-get-curr-option select)))
      (let ((new-options (senn-adjoin-options
                          option
                          (senn-lookup-options pron))))
        (setf (senn-select-options select) new-options)
        (setf (senn-select-option-count select) (length new-options))
        ;; 今増やしたので、これ以上増やせない
        (setf (senn-select-more-options-p select) nil)
        (setf (senn-select-selectors select)
              (senn-build-selectors (senn-vector->list new-options)
                                      senn-option-output-size))
        (setf (senn-select-np-count select) 0)))))

(defun senn-next-option ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (senn-with-current-select curr-select senn-conversion-data
    (senn-ensure-more-options-looked-up curr-select)
    (let ((count (senn-select-option-count curr-select))
          (index (senn-select-curr-option-idx curr-select)))
      (setf (senn-select-curr-option-idx curr-select)
            (if (or (= index (1- count))
                    (< index 0))
                0
                (1+ index)))
      (incf (senn-select-np-count curr-select))
      (when (<= senn-message-select-count
                (senn-select-np-count curr-select))
        (senn-message-select curr-select)))))

(defun senn-prev-option ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (senn-with-current-select curr-select senn-conversion-data
    (senn-ensure-more-options-looked-up curr-select)
    (let ((count (senn-select-option-count curr-select))
          (index (senn-select-curr-option-idx curr-select)))
      (setf (senn-select-curr-option-idx curr-select)
            (if (<= index 0)
                (1- count)
                (1- index)))
      (incf (senn-select-np-count curr-select))
      (when (<= senn-message-select-count
                (senn-select-np-count curr-select))
        (senn-message-select curr-select)))))

(defun senn-move-beginning-of-selects ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (setf (senn-conv-curr-select-idx senn-conversion-data) 0))

(defun senn-move-end-of-selects ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (setf (senn-conv-curr-select-idx senn-conversion-data)
        (1- (senn-conv-select-count senn-conversion-data))))

;; wordを短くできるか？
(defun senn-can-be-shorter-p (conversion)
  (senn-with-current-select curr-select conversion
    (< 1 (length (senn-select-pron curr-select)))))

;; wordを短くする。
(defun senn-make-word-shorter ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (when (senn-can-be-shorter-p senn-conversion-data)
    (setq senn-conversion-data (senn-reconvert-with-boundary
                                senn-conversion-data #'1-))))

;; wordを長くできるか？
(defun senn-can-be-longer-p (conversion)
  (let ((select-count (senn-conv-select-count conversion))
        (curr-select-idx (senn-conv-curr-select-idx conversion)))
    (< (1+ curr-select-idx) select-count)))

;; wordを長くする。
(defun senn-make-word-longer ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (when (senn-can-be-longer-p senn-conversion-data)
    (setq senn-conversion-data (senn-reconvert-with-boundary
                                senn-conversion-data #'1+))))

    ;; a,s,dなどが押されたときに呼ばれる。
;; 現在のフレーズの候補のインデックスを選ばれたものにする。
;; まちがって範囲外のものが押された可能性もあるので、そこら辺も考慮する。
(defun senn-select-from-chars ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (let ((selector-char last-input-event))
    (senn-with-current-select curr-select senn-conversion-data
      ;; まず、selectorがあるかを確認
      (when (senn-select-selectors curr-select)
        (let ((region (senn-selector-region
                       (senn-find-selector
                        (senn-select-curr-option-idx curr-select)
                        (senn-select-selectors curr-select)))))
          (let ((from (car region))
                (to   (cdr region)))
            (let ((new-option-idx
                   (+ from (position selector-char
                                     senn-option-selector-chars))))
              ;; はみ出してないか
              (when (and (<= from new-option-idx) (<= new-option-idx to))
                (setf (senn-select-curr-option-idx curr-select)
                      new-option-idx)))))
        (senn-message-select curr-select)))))

(defun senn-next-select ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (let ((index (senn-conv-curr-select-idx senn-conversion-data))
        (count (senn-conv-select-count senn-conversion-data)))
    (when (< index (1- count))
      (incf (senn-conv-curr-select-idx senn-conversion-data)))))

(defun senn-prev-select ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'kana-kanji)
  (let ((index (senn-conv-curr-select-idx senn-conversion-data)))
    (when (< 0 index)
      (decf (senn-conv-curr-select-idx senn-conversion-data)))))

(defun senn-to-hiragana ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'hiragana))

(defun senn-to-katakana ()
  (interactive)
  (setf (senn-conversion-mode senn-conversion-data) 'katakana))

(defun senn-accept ()
  (interactive)
  (setq senn-converting nil))


(defun senn-insert-select (select from delim focusedp)
  (let ((curr-option (senn-get-curr-option select))
        (option-form (senn-get-option-form select)))
    (insert (concat delim option-form))
    (cond (focusedp
           (senn-put-and-register-overlay from (point) 'face 'highlight))
          ; 普通の変換候補か
;          (curr-option
;           ;; 候補のoriginによってオーバーレイをつける。
;           (let* ((dict (senn-option-dict curr-option))
;                  (overlay (cdr (assoc origin senn-origin-overlay-alist))))
;             (when overlay
;               (senn-put-and-register-overlay from (point)
;                                                'face overlay)))))))
          )))


(defun senn-hiragana->katakana (hiragana-string)
  (apply #'concat
         (mapcar #'char-to-string
                 (mapcar #'japanese-katakana
                         (senn-vector->list hiragana-string)))))

;; conversion-dataを表示させる間数。
;; 変換の終わりのポインタ値を返す。
(defun senn-display-conversion (begin end delim-p)
  (goto-char begin)
  (delete-region begin end)
  (senn-delete-all-overlays)
  (let ((mode (senn-conversion-mode senn-conversion-data)))
    (cond ((eq mode 'hiragana)
           (insert (senn-conversion-pron senn-conversion-data))
           (when delim-p
             ;; アンダーラインだけ表示
             (senn-put-and-register-overlay begin (point)
                                            'face 'underline)))
          ((eq mode 'katakana)
           (insert (senn-hiragana->katakana
                    (senn-conversion-pron senn-conversion-data)))
           (when delim-p
             (senn-put-and-register-overlay begin (point)
                                            'face 'underline)))
          (t ;; 通常の場合
           (let ((delim
                  (if delim-p senn-preedit-delim-mark ""))
                 (begin-mark
                  (if delim-p senn-preedit-begin-mark ""))
                 (curr-select-idx
                  (senn-conversion-curr-select-idx senn-conversion-data))
                 (selects
                  (senn-conversion-selects senn-conversion-data)))
             (loop for select across selects
                   for i from 0
                   ;; begin-mark, delimのハイライトはしない
                   for from      = (1+ (point))
                   for separator = (if (= i 0) begin-mark delim)
                   for focusedp  = (= i curr-select-idx)
                   do (senn-insert-select select from separator focusedp)
                   finally (senn-with-current-select curr-select
                                                     senn-conversion-data
                             (senn-put-and-register-overlay
                              begin (point) 'face 'underline)))))))
  (point))


;;; senn-region

;; ローマ字入力してスペースを押すと呼ばれる。
;; 変換し終わった後、結局何文字になったのかを返さないと
;; 呼び出し元から怒られる。

;; 基本的に、
;; １、キーが押される。
;; ２、キーに対応する関数が呼ばれる。
;; ３、関数が、senn-conversion-dataの値を変える。
;; ４、senn-conversion-dataの値に従って、senn-display-conversionがselectを表示する。
;; の、ループ。
;; 変換が確定した後は、オーバーレイを全部消しておわり。
(defun senn-region (from to)
  (interactive "r")
  (let ((input (buffer-substring from to))
        (begin from) (end to))
    (setq senn-conversion-data (senn-convert input))
    (setq senn-converting      t)
    (setq end (senn-display-conversion begin end t))
    (unwind-protect
        (let ((current-input-method-title senn-input-method-title)
              ; これをnilにしないとinput-method-functionが
              ; quail-input-methodのままでまずい。
              (input-method-function nil))
          (while senn-converting
            (let* ((overriding-terminal-local-map senn-keymap)
                   (keyseq (read-key-sequence nil))
                   (cmd (lookup-key senn-keymap keyseq)))
              (unless (commandp cmd)
                ; 変なキーが来た場合
                ; 現在の変換をアクセプトして、変なキーを次の入力文字にする。
                (setq unread-input-method-events
                      (append (string-to-list (this-single-command-raw-keys))
                              unread-input-method-events))
                (setq cmd 'senn-accept))
              (call-interactively cmd) ;; コマンド実行
              (setq end (let ((delim-p (not (eq cmd 'senn-accept))))
                          (senn-display-conversion begin end delim-p)))))
          (- end begin)) ;これを返さないといけない
      (senn-delete-all-overlays))))


(defadvice toggle-input-method (before senn-init-processes)
  "SENNが使うプロセスを起動"
  (let ((lang (get-language-info "Japanese" 'input-method)))
    (when (equal lang "japanese-senn")
      (senn-ensure-server-invoked))))

(defadvice save-buffers-kill-emacs (before senn-kill-all-processes)
  (senn-kill-server))


(ad-activate 'toggle-input-method)
(ad-activate 'save-buffers-kill-emacs)


(provide 'senn)
