;; Commentary:
;;
;; かな漢字変換エンジンHACHEEをemacsから使うためのプログラム
;; leimとして使う。自前のquailパッケージを用意している。

(require 'cl)
(require 'hi-lock)

(require 'hachee-util)
(require 'hachee-data)

(defvar hachee-input-method-title "H"
  "String denoting HACHEE input method is working, which is shown on mode line.")

(defvar hachee-server-command-list
  (list (expand-file-name "../../bin/server.ros" hachee-elisp-dir))
  "hachee-server の PATH")

(defvar hachee-server-data-dir "~/.hachee/slm"
  "hachee-server のデータがあるところ。")

(defvar hachee-working-buffer " *hachee*")

(defvar hachee-server-process nil
  "hachee-serverのプロセス。")

(defvar hachee-accept-timeout 50)

(defvar hachee-overlay-list nil
  "使用するoverlayはこれで一括で管理する。")

(defvar hachee-option-selector-chars "asdfghjkl;"
  "候補を選ぶときの文字。")

(defvar hachee-preedit-begin-mark "|")

(defvar hachee-preedit-delim-mark "|")

(defvar hachee-origin-string-alist
  '((IV . "S") (TK . "T") (EX . "E")))

(defvar hachee-origin-overlay-alist
  '((IV . hi-yellow) (TK . hi-blue) (EX . hi-red)))

(defvar hachee-option-output-size
  (length hachee-option-selector-chars)
  "いくつの候補をミニバッファに表示するか")

(defvar hachee-message-select-count 2
  "hachee-next-prev-counterがこの数字以上になると、ミニバッファに候補のリストxが表示される。")

(defvar hachee-conversion-data nil
  "出力のためにいろいろな情報を集めるための変数。")


(defvar hachee-keymap
  (let ((map (make-sparse-keymap))
        (len (length hachee-option-selector-chars))
        (i 0))
    (while (< i len)
      (define-key map
        (char-to-string
         (aref hachee-option-selector-chars i))
        'hachee-select-from-chars)
      (setq i (1+ i)))
    (define-key map " " 'hachee-next-option)
    (define-key map "\C-n" 'hachee-next-option)
    (define-key map "\C-p" 'hachee-prev-option)
    (define-key map "\C-f" 'hachee-next-select)
    (define-key map "\C-b" 'hachee-prev-select)
    (define-key map "\C-i" 'hachee-shorter)
    (define-key map "\C-o" 'hachee-longer)
    (define-key map "\C-a" 'hachee-move-beginning-of-selects)
    (define-key map "\C-e" 'hachee-move-end-of-selects)
    (define-key map "H"    'hachee-to-hiragana)
    (define-key map "K"    'hachee-to-katakana)
    (define-key map "\C-h" 'hachee-to-hiragana)
    (define-key map "\C-k" 'hachee-to-katakana)
    (define-key map "\C-w" 'hachee-move-top-of-options)
    (define-key map "\C-[" 'hachee-move-bottom-of-options)
    (define-key map "" 'hachee-cancel)
    (define-key map "" 'hachee-accept)
    map))

;; これで、ベクタがリストになる
;; Emacs Lispのリファレンスマニュアルにも書いてある正当なやり方
(defun hachee-vector->list (vector)
  (append vector nil))

(defun hachee-process-sentinel (proc stat)
  (message "%s" stat))

(defun hachee-process-running-p (proc)
  (and (processp proc) (eq (process-status proc) 'run)))

(defun hachee-kill-server ()
  (when (hachee-process-running-p hachee-server-process)
    (kill-process hachee-server-process)))

;; serverを起動し、プロセスを返す。
(defun hachee-invoke-server ()
  (loop for cmd in hachee-server-command-list
        for proc = (start-process
                    "hachee-kkc"
                    hachee-working-buffer
                    cmd
                    hachee-server-data-dir)
        when (hachee-process-running-p proc) return proc))

;; serverが走ってなかったら起動。
(defun hachee-ensure-server-invoked ()
  (unless (hachee-process-running-p hachee-server-process)
    (setq hachee-server-process (hachee-invoke-server))
    (set-process-sentinel hachee-server-process 'hachee-process-sentinel)))

(defun hachee-send-recv-command (command)
  (hachee-ensure-server-invoked)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer hachee-working-buffer)
          (erase-buffer)
          (process-send-string hachee-server-process command)
          (while (= (buffer-size) 0)
            (accept-process-output nil 0 hachee-accept-timeout))
          ; 文字列として返す
          (buffer-string))
      (set-buffer old-buffer))))

(defun hachee-api-call (op arg-plist)
  (let ((json (json-encode `((op . ,op) (args . ,arg-plist)))))
    (json-read-from-string
     (hachee-send-recv-command (concat json "\n")))))

(defun hachee-api-convert (string)
  (hachee-api-call "convert" `(("text" . ,string))))

(defun hachee-api-lookup-words (string)
  (hachee-api-call "lookup" `(("text" . ,string))))

(defun hachee-api-quit ()
  (hachee-api-call "quit" nil))

(defun hachee-assoc-get (key alist)
  (cdr (assoc key alist)))

(defun hachee-convert (pron)
  (labels ((word->select (word)
             (let ((option (make-hachee-option
                            :form (hachee-assoc-get 'form word)
                            :origin 'IV
                            :logP 0)))
               (make-hachee-select :pron (hachee-assoc-get 'pron word)
                                   :options (vector option)
                                   :option-count 1
                                   :curr-option-idx 0
                                   :more-options-p t))))
    (let ((words (hachee-api-convert pron)))
      (make-hachee-conversion :pron pron
                              :logP 0
                              :selects (apply #'vector
                                              (mapcar #'word->select words))
                              :select-count (length words)
                              :curr-select-idx 0))))

(defun hachee-lookup-options (pron)
  (mapcar #'(lambda (word)
              (make-hachee-option
               :form (hachee-assoc-get 'form word)
               :origin 'IV
               :logP 0))
          (hachee-api-lookup-words pron)))

;;; オーバーレイに関する関数。
(defun hachee-delete-all-overlays ()
  (dolist (overlay hachee-overlay-list)
    (delete-overlay overlay))
  (setq hachee-overlay-list nil))


;; overlayをつくって設定した後、プットしてhachee-overlay-listにプッシュする。
(defun hachee-put-and-register-overlay (begin end prop value)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay prop value)
    (push overlay hachee-overlay-list)))


;;; 現在のフレーズの変換候補をエコーエリアに表示できるようにするための関数。
(defun hachee-origin->string (org)
  (let ((pair (assoc org hachee-origin-string-alist)))
    (if pair (concat " (" (cdr pair) ")") "")))

(defun hachee-get-option-selector-char (index)
  (char-to-string (aref hachee-option-selector-chars index)))

(defun hachee-option->message (option selector-char focusedp)
  (let ((form (hachee-option-form option))
        (origin (hachee-origin->string (hachee-option-origin option))))
    (if focusedp
        (let ((msg (format " %s: [%s]%s " selector-char form origin)))
        (put-text-property (position ?\[ msg) (1+ (position ?\] msg))
                           'face 'highlight msg)
        msg)
    (format " %s: %s%s " selector-char form origin))))

(defun hachee-collect-option-messages (curr-option-idx selector)
  (let ((region (hachee-selector-region selector))
        (options (hachee-selector-options selector)))
    (let ((index-from-region-begin (- curr-option-idx (car region))))
      (loop for option in options
            for i from 0
            for focusedp      = (= i index-from-region-begin)
            for selector-char = (hachee-get-option-selector-char i)
            collect (hachee-option->message
                      option selector-char focusedp)))))


;; フレーズに従って、エコーエリアに表示させる候補文字列を構成する関数。
(defun hachee-select->minibuf-string (select)
  (let ((selectors (hachee-select-selectors select)))
    (when selectors
      (let ((curr-option-idx (hachee-select-curr-option-idx select)))
        (apply 'concat (hachee-collect-option-messages
                        curr-option-idx
                        (hachee-find-selector curr-option-idx
                                              selectors)))))))

;; エコーエリアに現在の変換候補を表示。
(defun hachee-message-select (select)
  (let ((message-string (hachee-select->minibuf-string select)))
    (when message-string
      (let ((message-log-max nil)) ;; *Message*バッファにログを残さない
        (message message-string)))))


;;; キーが押されたら呼ばれるコマンド

(defun hachee-move-top-of-options ()
  (interactive)
  (hachee-with-current-select curr-select hachee-conversion-data
    (setf (hachee-select-curr-option-idx curr-select) 0)
    (hachee-message-select curr-select)))

(defun hachee-move-bottom-of-options ()
  (interactive)
  (hachee-with-current-select curr-select hachee-conversion-data
    (setf (hachee-select-curr-option-idx curr-select)
          (1- (hachee-select-option-count curr-select)))
    (hachee-message-select curr-select)))


(defun hachee-adjoin-options (option other-options)
  (let ((form (hachee-option-form option)))
    (apply #'vector
           (cons option
                 (remove-if ;候補に重複があれば、取り除く
                  #'(lambda (o) (string= form (hachee-option-form o)))
                  other-options)))))

(defun hachee-ensure-more-options-looked-up (select)
  (when (hachee-select-more-options-p select)
    (let ((pron (hachee-select-pron select))
          (option (hachee-get-curr-option select)))
      (let ((new-options (hachee-adjoin-options
                          option
                          (hachee-lookup-options pron))))
        (setf (hachee-select-options select) new-options)
        (setf (hachee-select-option-count select) (length new-options))
        ;; 今増やしたので、これ以上増やせない
        (setf (hachee-select-more-options-p select) nil)
        (setf (hachee-select-selectors select)
              (hachee-build-selectors (hachee-vector->list new-options)
                                      hachee-option-output-size))
        (setf (hachee-select-np-count select) 0)))))

(defun hachee-next-option ()
  (interactive)
  (hachee-with-current-select curr-select hachee-conversion-data
    (hachee-ensure-more-options-looked-up curr-select)
    (let ((count (hachee-select-option-count curr-select))
          (index (hachee-select-curr-option-idx curr-select)))
      (setf (hachee-select-curr-option-idx curr-select)
            (if (or (= index (1- count))
                    (< index 0))
                0
                (1+ index)))
      (incf (hachee-select-np-count curr-select))
      (when (<= hachee-message-select-count
                (hachee-select-np-count curr-select))
        (hachee-message-select curr-select)))))

(defun hachee-prev-option ()
  (interactive)
  (hachee-with-current-select curr-select hachee-conversion-data
    (hachee-ensure-more-options-looked-up curr-select)
    (let ((count (hachee-select-option-count curr-select))
          (index (hachee-select-curr-option-idx curr-select)))
      (setf (hachee-select-curr-option-idx curr-select)
            (if (<= index 0)
                (1- count)
                (1- index)))
      (incf (hachee-select-np-count curr-select))
      (when (<= hachee-message-select-count
                (hachee-select-np-count curr-select))
        (hachee-message-select curr-select)))))

(defun hachee-move-beginning-of-selects ()
  (interactive)
  (setf (hachee-conv-curr-select-idx hachee-conversion-data) 0))

(defun hachee-move-end-of-selects ()
  (interactive)
  (setf (hachee-conv-curr-select-idx hachee-conversion-data)
        (1- (hachee-conv-select-count hachee-conversion-data))))

;; フレーズを短くできるか？
(defun hachee-can-be-shorter-p (conversion)
  (hachee-with-current-select curr-select conversion
    (< 1 (length (hachee-select-pron curr-select)))))

;; フレーズを短くする。
(defun hachee-make-select-shorter ()
  (interactive)
  (when (hachee-can-be-shorter-p hachee-conversion-data)
    (setq hachee-conversion-data
          (hachee-reconvert-conversion hachee-conversion-data 'shorter))))


;; フレーズを長くできるか？
(defun hachee-can-be-longer-p (conversion)
  (let ((select-count (hachee-conv-select-count conversion))
        (curr-select-idx  (hachee-conv-curr-select-idx  conversion)))
    (< (1+ curr-select-idx) select-count)))

;; フレーズを長くする。
(defun hachee-make-select-longer ()
  (interactive)
  (when (hachee-can-be-longer-p hachee-conversion-data)
    (setq hachee-conversion-data
          (hachee-reconvert-conversion hachee-conversion-data 'longer))))


;; a,s,dなどが押されたときに呼ばれる。
;; 現在のフレーズの候補のインデックスを選ばれたものにする。
;; まちがって範囲外のものが押された可能性もあるので、そこら辺も考慮する。
(defun hachee-select-from-chars ()
  (interactive)
  (let ((selector-char last-input-event))
    (hachee-with-current-select curr-select hachee-conversion-data
      ;; まず、selectorがあるかを確認
      (when (hachee-select-selectors curr-select)
        (let ((region (hachee-selector-region
                       (hachee-find-selector
                        (hachee-select-curr-option-idx curr-select)
                        (hachee-select-selectors curr-select)))))
          (let ((from (car region))
                (to   (cdr region)))
            (let ((new-option-idx
                   (+ from (position selector-char
                                     hachee-option-selector-chars))))
              (when (<= from new-option-idx to) ;; はみ出してないか
                (setf (hachee-select-curr-option-idx curr-select)
                      new-option-idx)))))
        (hachee-message-select curr-select)))))

(defun hachee-next-select ()
  (interactive)
  (let ((index (hachee-conv-curr-select-idx hachee-conversion-data))
        (count (hachee-conv-select-count hachee-conversion-data)))
    (when (< index (1- count))
      (incf (hachee-conv-curr-select-idx hachee-conversion-data)))))

(defun hachee-prev-select ()
  (interactive)
  (let ((index (hachee-conv-curr-select-idx hachee-conversion-data)))
    (when (< 0 index)
      (decf (hachee-conv-curr-select-idx hachee-conversion-data)))))

(defun hachee-accept ()
  (interactive)
  (setq hachee-converting nil))


;; コマンドを実行する。
(defun hachee-do-command (cmd)
  (when (not (eq cmd 'hachee-accept))
    ;; curr-select-idx = -1で確定もあり得るので
    ;; hachee-acceptはここと別にしないといけない
    (let ((selects (hachee-conv-selects hachee-conversion-data))
          (index (hachee-conv-curr-select-idx hachee-conversion-data)))
      (when (< index 0)
        (setf (hachee-conv-curr-select-idx hachee-conversion-data) 0)
        (setf index 0))))
  (call-interactively cmd))


(defun hachee-insert-select (select from delim focusedp)
  (let ((curr-option (hachee-get-curr-option select))
        (option-form (hachee-get-option-form select)))
    (insert (concat delim option-form))
    (cond (focusedp
           (hachee-put-and-register-overlay from (point) 'face 'highlight))
          ; 普通の変換候補か
;          (curr-option
;           ;; 候補のoriginによってオーバーレイをつける。
;           (let* ((dict (hachee-option-dict curr-option))
;                  (overlay (cdr (assoc origin hachee-origin-overlay-alist))))
;             (when overlay
;               (hachee-put-and-register-overlay from (point)
;                                                'face overlay)))))))
          )))

;; フレーズを表示させる間数。
;; 変換の終わりのポインタ値を返す。
(defun hachee-display-selects (begin end delim-p)
  (goto-char begin)
  (delete-region begin end)
  (hachee-delete-all-overlays)
  (let ((curr-select-idx
         (hachee-conv-curr-select-idx hachee-conversion-data)))
    (cond ((= curr-select-idx -1)
           ;; ひらがなの場合
           (insert (hachee-conv-input hachee-conversion-data))
           (when delim-p
             ;; アンダーラインだけ表示
             (hachee-put-and-register-overlay begin (point)
                                              'face 'underline)))
          (t ;; 通常の場合
           (loop
            with delim      = (if delim-p hachee-preedit-delim-mark "")
            with begin-mark = (if delim-p hachee-preedit-begin-mark "")
            for select across (hachee-conv-selects hachee-conversion-data)
            for i from 0
            ;; begin-mark, delimのハイライトはしない
            for from      = (1+ (point))
            for separator = (if (= i 0) begin-mark delim)
            for focusedp  = (= i curr-select-idx)
            do      (hachee-insert-select select from separator focusedp)
            finally (hachee-with-current-select curr-select
                                                hachee-conversion-data
                      (hachee-put-and-register-overlay begin (point)
                                                       'face 'underline))))))
  (point))


;;; hachee-region

;; ローマ字入力してスペースを押すと呼ばれる。
;; 変換し終わった後、結局何文字になったのかを返さないと
;; 呼び出し元から怒られる。

;; 基本的に、
;; １、キーが押される。
;; ２、キーに対応する関数が呼ばれる。
;; ３、関数が、hachee-conversion-dataの値を変える。
;; ４、hachee-conversion-dataの値に従って、hachee-display-selectsがselectを表示する。
;; の、ループ。
;; 変換が確定した後は、オーバーレイを全部消しておわり。
(defun hachee-region (from to)
  (interactive "r")
  (let ((input (buffer-substring from to))
        (begin from) (end to))
    (setq hachee-conversion-data (hachee-convert input))
    (setq hachee-converting      t)
    (setq end (hachee-display-selects begin end t))
    (unwind-protect
        (let ((current-input-method-title hachee-input-method-title)
              ; これをnilにしないとinput-method-functionが
              ; quail-input-methodのままでまずい。
              (input-method-function nil))
          (while hachee-converting
            (let* ((overriding-terminal-local-map hachee-keymap)
                   (keyseq (read-key-sequence nil))
                   (cmd (lookup-key hachee-keymap keyseq)))
              (unless (commandp cmd)
                ; 変なキーが来た場合
                ; 現在の変換をアクセプトして、変なキーを次の入力文字にする。
                (setq unread-input-method-events
                      (append (string-to-list (this-single-command-raw-keys))
                              unread-input-method-events))
                (setq cmd 'hachee-accept))
              ; コマンド実行
              (hachee-do-command cmd)
              (if (eq cmd 'hachee-accept)
                 (setq end (hachee-display-selects begin end nil))
                (setq end (hachee-display-selects begin end t)))))
          (- end begin)) ;これを返さないといけない
      (hachee-delete-all-overlays))))


(defadvice toggle-input-method (before hachee-init-processes)
  "HACHEEが使うプロセスを起動"
  (let ((lang (get-language-info "Japanese" 'input-method)))
    (when (equal lang "japanese-hachee")
      (hachee-ensure-server-invoked))))

(defadvice save-buffers-kill-emacs (before hachee-kill-all-processes)
  (hachee-kill-server))


(ad-activate 'toggle-input-method)
(ad-activate 'save-buffers-kill-emacs)


(provide 'hachee)
