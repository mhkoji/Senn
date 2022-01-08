(defpackage :senn.im.buffer
  (:use :cl)
  (:export :make-buffer
           :buffer-cursor-pos
           :buffer-string
           :insert-char
           :delete-char
           :move-cursor-pos))
(in-package :senn.im.buffer)

(defstruct buffer
  (string "")
  (cursor-pos 0))

(defun replace-n-at-end (str)
  (let ((len (length str)))
    (if (and (< 0 len)
             (char= (char str (1- len)) #\n))
        (concatenate 'string
                     (subseq str 0 (1- len))
                     "ん")
        str)))

(defun insert-char (buffer char)
  (let ((string (buffer-string buffer))
        (pos (buffer-cursor-pos buffer)))
    (labels ((try-insertion (diff-from-pos)
               (if (< diff-from-pos 0)
                   (make-buffer :string (concatenate 'string
                                                     (replace-n-at-end
                                                      (subseq string 0 pos))
                                                     (string char)
                                                     (subseq string pos))
                                :cursor-pos (1+ pos))
                   (let ((substr-ending-at-pos
                          (concatenate 'string
                           (subseq string (- pos diff-from-pos) pos)
                           (string char))))
                     (let ((hiragana
                            (senn.ja:romaji->hiragana
                             substr-ending-at-pos)))
                       (if hiragana
                           (make-buffer
                            :string
                            (concatenate 'string
                             (subseq string 0 (- pos diff-from-pos))
                             hiragana
                             (subseq string pos))
                            :cursor-pos (+ (- pos diff-from-pos)
                                           (length hiragana)))
                           (try-insertion (1- diff-from-pos))))))))
      (try-insertion (min pos 4)))))


(defun delete-char (buffer)
  (let ((string (buffer-string buffer))
        (pos (buffer-cursor-pos buffer)))
    (if (< 0 pos)
        (make-buffer :string (concatenate
                              'string
                              (subseq string 0 (1- pos))
                              (subseq string pos))
                     :cursor-pos (1- pos))
        buffer)))


(defun move-cursor-pos (buffer diff)
  (let ((string (buffer-string buffer))
        (pos (buffer-cursor-pos buffer)))
    (let ((new-pos (+ diff pos)))
      (if (<= 0 new-pos (length string))
          (make-buffer :string string :cursor-pos new-pos)
          buffer))))


(let ((buf (insert-char (make-buffer :string "あk"
                                     :cursor-pos 2)
                        #\a)))
  (assert (string= (buffer-string buf) "あか"))
  (assert (= (buffer-cursor-pos buf) 2)))

(let ((buf (insert-char (make-buffer :string "kr"
                                     :cursor-pos 1)
                        #\a)))
  (assert (string= (buffer-string buf) "かr"))
  (assert (= (buffer-cursor-pos buf) 1)))
