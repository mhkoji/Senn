(defpackage :senn.win.im
  (:use :cl)
  (:export :ime
           :ime-kkc
           :ime-predictor
           :make-editing
           :editing
           :editing-buffer
           :editing-predictions
           :make-converting
           :converting
           :converting-segments
           :converting-pronunciation
           :converting-current-segment-index
           :convert))
(in-package :senn.win.im)

(defclass ime () ())
(defgeneric ime-kkc (ime))
(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))

;; input state

(defstruct editing
  (buffer (senn.im.buffer:make-buffer))
  predictions)

(defstruct segment
  pron
  candidates
  current-index
  has-more-candidates-p)

(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))

(defun segment-cursor-pos-form (s)
  (senn.im.kkc:candidate-form (elt (segment-candidates s)
                                   (segment-current-index s))))

(defun current-segment-move! (converting diff)
  (let ((new-index (+ (converting-current-segment-index converting) diff)))
    (when (<= 0 new-index (1- (length (converting-segments converting))))
      (setf (converting-current-segment-index converting) new-index))))

(defun segment-cursor-pos-move! (segment diff)
  (with-accessors ((current-index segment-current-index)) segment
    (setf current-index (mod (+ current-index diff)
                             (length (segment-forms segment))))))

(defun append-candidates (current-candidates new-candidates)
  (labels ((exists-in-current-candidates-p (cand)
             (find-if (lambda (c)
                        (string= (candidate-form c)
                                 (candidate-form cand)))
                      current-candidates)))
    ;; Don't change the sort order of current-candidates.
    (append current-candidates
            (remove-if #'exists-in-current-candidates-p new-candidates))))

(defun convert (kkc pron)
  (let ((kkc-segs (senn.im.kkc:convert kkc pron)))
    (make-converting
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

(defun ssegment-append-candidates! (segment ime)
  (when (segment-has-more-candidates-p segment)
    (let ((current-candidates (segment-candidates segment))
          (candidates (senn.im.kkc:lookup (ime-kkc ime)
                                          (segment-pron segment))))
      (setf (segment-candidates segment)
            (append-candidates current-candidates candidates)))
    (setf (segment-has-more-candidates-p segment) nil)))
