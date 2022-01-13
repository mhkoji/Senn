(defpackage :senn.im.converting
  (:use :cl)
  (:export :segment-pron
           :segment-current-index
           :segment-shows-katakana-p
           :segment-has-more-candidates-p
           :segment-forms
           :segment-cursor-pos-form

           :state
           :state-pronunciation
           :state-segments
           :state-current-segment
           :state-current-input
           :state-current-segment-index
           :state-move-curret-segment
           :current-segment
           :current-segment-move!
           :current-segment-katakana!
           :current-segment-candidates-move!
           :current-segment-candidates-set!
           :current-input
           :convert)
  (:import-from :senn.im.kkc
                :candidate
                :candidate-form))
(in-package :senn.im.converting)

(defstruct segment
  pron
  candidates
  current-index
  has-more-candidates-p
  shows-katakana-p)

(defun segment-forms (s)
  (mapcar #'candidate-form (segment-candidates s)))

(defun segment-cursor-pos-form (s)
  (if (segment-shows-katakana-p s)
      (senn.ja:hiragana->katakana (segment-pron s))
      (candidate-form (elt (segment-candidates s)
                           (segment-current-index s)))))

(defun segment-cursor-pos-move! (segment diff)
  (with-accessors ((current-index segment-current-index)) segment
    (setf current-index (mod (+ current-index diff)
                             (length (segment-candidates segment))))))

(defun segment-cursor-pos-set! (segment index)
  (with-accessors ((current-index segment-current-index)) segment
    (when (< -1 index (length (segment-candidates segment)))
      (setf current-index index))))

(defun append-candidates (current-candidates new-candidates)
  (labels ((exists-in-current-candidates-p (cand)
             (find-if (lambda (c)
                        (string= (candidate-form c)
                                 (candidate-form cand)))
                      current-candidates)))
    ;; Don't change the sort order of current-candidates.
    (append current-candidates
            (remove-if #'exists-in-current-candidates-p new-candidates))))

(defun segment-ensure-candidates-appended! (segment kkc)
  (when (segment-has-more-candidates-p segment)
    (let ((current-candidates (segment-candidates segment))
          (candidates (senn.im.kkc:lookup kkc (segment-pron segment))))
      (setf (segment-candidates segment)
            (append-candidates current-candidates candidates))
      (setf (segment-has-more-candidates-p segment) nil))))

(defstruct state
  segments
  pronunciation
  (current-segment-index 0))

(defun current-segment (state)
  (elt (state-segments state) (state-current-segment-index state)))

(defun current-segment-move! (state diff)
  (let ((new-index (+ (state-current-segment-index state) diff)))
    (when (<= 0 new-index (1- (length (state-segments state))))
      (setf (state-current-segment-index state) new-index))))

(defun current-segment-candidates-move! (state diff kkc)
  (with-accessors ((segment current-segment)) state
    (segment-ensure-candidates-appended! segment kkc)
    (segment-cursor-pos-move! segment diff)
    (setf (segment-shows-katakana-p segment) nil)))

(defun current-segment-candidates-set! (state index)
  (with-accessors ((segment current-segment)) state
    (segment-cursor-pos-set! segment index)))

(defun current-segment-katakana! (state)
  (with-accessors ((segment current-segment)) state
    (setf (segment-shows-katakana-p segment) t)))

(defun current-input (state)
  (format nil "~{~A~}"
          (mapcar #'segment-cursor-pos-form (state-segments state))))

(defun convert (kkc pron)
  (let ((kkc-segs (senn.im.kkc:convert kkc pron)))
    (make-state
     :segments
     (mapcar (lambda (kkc-seg)
               (make-segment
                :pron (senn.im.kkc:segment-pron kkc-seg)
                :candidates
                (list (senn.im.kkc:make-candidate
                       :form (senn.im.kkc:segment-form kkc-seg)))
                :current-index 0
                :has-more-candidates-p t))
             kkc-segs)
     :current-segment-index 0
     :pronunciation pron)))
