(defpackage :senn.im.inputting
  (:use :cl)
  (:export :state
           :state-buffer
           :state-buffer-empty-p
           :state-buffer-string
           :state-buffer-get-pron
           :state-buffer-cursor-pos-move!
           :state-predictions
           :make-state

           :insert-char!
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

(defun state-predictions-update! (state predictor max-count)
  (when predictor
    (setf (state-predictions state)
          (let ((string (state-buffer-string state)))
            (if (string= string "")
                nil
                (let ((cands (senn.im.predict:execute predictor string)))
                  (if max-count
                      (take-first cands max-count)
                      cands)))))))

(defun state-buffer-cursor-pos-move! (state diff)
  (setf (state-buffer state)
        (senn.im.buffer:move-cursor-pos (state-buffer state) diff)))

(defun insert-char! (state char &optional predictor max-count)
  (setf (state-buffer state)
        (senn.im.buffer:insert-char (state-buffer state) char))
  (state-predictions-update! state predictor max-count))

(defun delete-char! (state &optional predictor max-count)
  (when (not (state-buffer-empty-p state))
    (setf (state-buffer state)
          (senn.im.buffer:delete-char (state-buffer state)))
    (state-predictions-update! state predictor max-count)
    t))
