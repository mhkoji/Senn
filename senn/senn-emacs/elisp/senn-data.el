;;; データ構造の定義

;; 変換候補の一つ分。
(defstruct senn-option
  form
  origin
  logP)

;; pronに対する変換候補の束
(defstruct senn-select
  pron
  options
  option-count
  curr-option-idx
  more-options-p
  selectors
  ;;next, prevの総数
  np-count
  )

(defun senn-get-curr-option (select)
  (let ((options (senn-select-options select))
        (curr-index (senn-select-curr-option-idx select)))
    (if (<= 0 curr-index)
        (aref options curr-index)
      nil)))

(defun senn-get-option-form (select)
  (let ((option (senn-get-curr-option select)))
    (if option
        ;; 通常の場合
        (senn-option-form option)
      (case (senn-select-curr-option-idx option)
        ;; ひらがなの場合
        (-1 (senn-select-pron option))
        ;; カタカナの場合
        (-2 (hiragana->katakana (senn-select-pron option)))))))

;; 変換に関わるすべての変数を保持する構造。
;; 日本語列は単語に区切られて、senn-selectという形になっている。
;; 全senn-selectは配列として、senn-selectsがもっている。
;; curr-select-idxが編集しているsenn-selectsのインデックス。
;; つまり、curr-select-idxで示されるsenn-selectで
;; 次や前の候補が得られたり、ハイライトが表示されている。
;; -1のときは、全てひらがな表示。バックスぺースキーが押されたときなど。
(defstruct senn-conversion
  pron
  logP
  selects
  select-count
  curr-select-idx)
(defalias 'senn-conv-pron            'senn-conversion-pron)
(defalias 'senn-conv-logP            'senn-conversion-logP)
(defalias 'senn-conv-selects         'senn-conversion-selects)
(defalias 'senn-conv-select-count    'senn-conversion-select-count)
(defalias 'senn-conv-curr-select-idx 'senn-conversion-curr-select-idx)

(defmacro senn-with-selects (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (senn-conv-selects ,conv)))
	 ,@body))))

(defmacro senn-with-current-select (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (aref (senn-conv-selects ,g)
                         (senn-conv-curr-select-idx ,g))))
	 ,@body))))

;; selectorは、ミニバッファに候補を見せるための構造。
;; まず、全候補をリストにして、何個かに分ける。
;; 何個で分けるかは、senn-option-output-sizeとフレーム幅で決まる。
;; そして、等分されたリストに、
;; 最初の候補のインデックスと最後の候補のインデックスのコンスを付け加える。
;; これでselectorをつくる。
(defstruct senn-selector
  region
  options)

(defun senn-calc-width (option)
  (let ((selector-char-width 1)
        (form-width (string-width (senn-option-form option)))
        (origin-width (string-width (senn-origin->string
                                     (senn-option-origin option)))))
    ;; ` a: [漢字](IV) 'を参考に幅を計算
    (+ 1  selector-char-width 1 1 1 form-width 1 origin-width 1)))

(defun senn-build-selectors (options max-size)
  (let ((frame-width (frame-width))
        (total-width (senn-calc-width (car options)))
        (total-size  1)
        (tmp (list (car options)))
        (from 0)
        (ret nil))
    (loop for option in (cdr options)
          for index from 1
          do (let ((cur-width (senn-calc-width option)))
               (cond ((and (<= (1+ total-size) max-size)
                           (< (+ cur-width total-width) frame-width))
                      (push option tmp)
                      (incf total-size)
                      (incf total-width cur-width))
                     (t
                      (let ((selector (make-senn-selector
                                       :region (cons from
                                                     (+ from
                                                        (1- total-size)))
                                       :options (nreverse tmp))))
                        (push selector ret)
                        (setq tmp (list option))
                        (setq total-width cur-width)
                        (setq total-size  1)
                        (setq from index)))))
          finally (when tmp
                    (let ((selector (make-senn-selector
                                     :region (cons from
                                                   (+ from
                                                      (1- total-size)))
                                     :options (nreverse tmp))))
                      (push selector ret))))
    (nreverse ret)))

;; selectorのリストの中から、indexを挟むselectorを取り出す。
(defun senn-find-selector (index selectors)
  (find-if #'(lambda (selector)
               (let ((region (senn-selector-region selector)))
                 (let ((from (car region))
                       (to   (cdr region)))
                   (and (<= from index)
                        (<= index to)))))
           selectors))

(provide 'senn-data)
