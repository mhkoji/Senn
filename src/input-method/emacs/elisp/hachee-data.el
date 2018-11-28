;;; データ構造の定義

;; 変換候補の一つ分。
(defstruct hachee-option
  form
  origin
  logP)

;; pronに対する変換候補の束
(defstruct hachee-select
  pron
  options
  option-count
  curr-option-idx
  more-options-p
  selectors
  ;;next, prevの総数
  np-count
  )

(defun hachee-get-curr-option (select)
  (let ((options (hachee-select-options select))
        (curr-index (hachee-select-curr-option-idx select)))
    (if (<= 0 curr-index)
        (aref options curr-index)
      nil)))

(defun hachee-get-option-form (select)
  (let ((option (hachee-get-curr-option select)))
    (if option
        ;; 通常の場合
        (hachee-option-form option)
      (case (hachee-select-curr-option-idx option)
        ;; ひらがなの場合
        (-1 (hachee-select-pron option))
        ;; カタカナの場合
        (-2 (hiragana->katakana (hachee-select-pron option)))))))

;; 変換に関わるすべての変数を保持する構造。
;; 日本語列は単語に区切られて、hachee-selectという形になっている。
;; 全hachee-selectは配列として、hachee-selectsがもっている。
;; curr-select-idxが編集しているhachee-selectsのインデックス。
;; つまり、curr-select-idxで示されるhachee-selectで
;; 次や前の候補が得られたり、ハイライトが表示されている。
;; -1のときは、全てひらがな表示。バックスぺースキーが押されたときなど。
(defstruct hachee-conversion
  pron
  logP
  selects
  select-count
  curr-select-idx)
(defalias 'hachee-conv-pron            'hachee-conversion-pron)
(defalias 'hachee-conv-logP            'hachee-conversion-logP)
(defalias 'hachee-conv-selects         'hachee-conversion-selects)
(defalias 'hachee-conv-select-count    'hachee-conversion-select-count)
(defalias 'hachee-conv-curr-select-idx 'hachee-conversion-curr-select-idx)

(defmacro hachee-with-selects (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (hachee-conv-selects ,conv)))
	 ,@body))))

(defmacro hachee-with-current-select (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (aref (hachee-conv-selects ,g)
                         (hachee-conv-curr-select-idx ,g))))
	 ,@body))))

;; selectorは、ミニバッファに候補を見せるための構造。
;; まず、全候補をリストにして、何個かに分ける。
;; 何個で分けるかは、hachee-option-output-sizeとフレーム幅で決まる。
;; そして、等分されたリストに、
;; 最初の候補のインデックスと最後の候補のインデックスのコンスを付け加える。
;; これでselectorをつくる。
(defstruct hachee-selector
  region
  options)

(defun hachee-calc-width (option)
  (let ((selector-char-width 1)
        (form-width (string-width (hachee-option-form option)))
        (origin-width (string-width (hachee-origin->string
                                     (hachee-option-origin option)))))
    ;; ` a: [漢字](IV) 'を参考に幅を計算
    (+ 1  selector-char-width 1 1 1 form-width 1 origin-width 1)))

(defun hachee-build-selectors (options max-size)
  (let ((frame-width (frame-width))
        (total-width (hachee-calc-width (car options)))
        (total-size  1)
        (tmp (list (car options)))
        (from 0)
        (ret nil))
    (loop for option in (cdr options)
          for index from 1
          do (let ((cur-width (hachee-calc-width option)))
               (cond ((and (<= (1+ total-size) max-size)
                           (< (+ cur-width total-width) frame-width))
                      (push option tmp)
                      (incf total-size)
                      (incf total-width cur-width))
                     (t
                      (let ((selector (make-hachee-selector
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
                    (let ((selector (make-hachee-selector
                                     :region (cons from
                                                   (+ from
                                                      (1- total-size)))
                                     :options (nreverse tmp))))
                      (push selector ret))))
    (nreverse ret)))

;; selectorのリストの中から、indexを挟むselectorを取り出す。
(defun hachee-find-selector (index selectors)
  (find-if #'(lambda (selector)
               (let ((region (hachee-selector-region selector)))
                 (let ((from (car region))
                       (to   (cdr region)))
                   (<= from index to))))
           selectors))

(provide 'hachee-data)
