(defpackage :senn.im.inputting
  (:use :cl)
  (:export :ime
           :ime-predictor
           :ime-max-candidate-count

           :state
           :state-buffer
           :state-buffer-empty-p
           :state-buffer-string
           :state-buffer-get-pron
           :state-buffer-cursor-pos-move!
           :state-predictions
           :make-state

           :insert-char!
           :insert-char-direct!
           :delete-char!))
(in-package :senn.im.inputting)

(defun buffer-empty-p (buffer)
  (string= (senn.im.buffer:buffer-string buffer) ""))

(defstruct state
  (buffer (senn.im.buffer:make-buffer))
  predictions)

(defun state-buffer-empty-p (state)
  (buffer-empty-p (state-buffer state)))

(defun state-buffer-string (state)
  (senn.im.buffer:buffer-string (state-buffer state)))

(defun state-buffer-get-pron (state)
  (let ((pron (state-buffer-string state)))
    (if (char= (alexandria:last-elt pron) #\n)
        ;; It is convenient to add an additional #\n to make "ん" if the pron ends with a single "n".
        (senn.im.buffer:buffer-string
         (senn.im.buffer:insert-char (state-buffer state) #\n))
        pron)))

(defun take-first (list n)
  (if (< n (length list))
      (subseq list 0 n)
      list))

(defclass ime () ())
(defgeneric ime-predictor (ime))
(defgeneric ime-max-candidate-count (ime))

(defun ime-list-predictions (ime string)
  (if (string= string "")
      nil
      (with-accessors ((predictor ime-predictor)
                       (max-count ime-max-candidate-count)) ime
        (let ((cands (senn.im.predict:execute predictor string)))
          (if max-count
              (take-first cands max-count)
              cands)))))

(defun state-predictions-update! (state ime)
  (when ime
    (setf (state-predictions state)
          (ime-list-predictions ime (state-buffer-string state)))))

(defun state-buffer-cursor-pos-move! (state diff)
  (setf (state-buffer state)
        (senn.im.buffer:move-cursor-pos (state-buffer state) diff)))

(defun insert-char! (state char &optional ime)
  (setf (state-buffer state)
        (senn.im.buffer:insert-char (state-buffer state) char))
  (state-predictions-update! state ime))

(defun insert-char-direct! (state char &optional ime)
  (setf (state-buffer state)
        (senn.im.buffer:insert-char-direct (state-buffer state) char))
  (state-predictions-update! state ime))

(defun delete-char! (state &optional ime)
  (when (not (state-buffer-empty-p state))
    (setf (state-buffer state)
          (senn.im.buffer:delete-char (state-buffer state)))
    (state-predictions-update! state ime)
    t))
