(defpackage :hachee.input-method.fcitx.state
  (:use :cl)
  (:export :make-state
           :state-type
           :state-buffer
           :state-cursor-pos-in-utf-8
           :transit-by-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.fcitx.state)

(defun buffer-insert-char (buffer pos char)
  (labels ((try-insertion (diff-from-pos)
             (if (< diff-from-pos 0)
                 (list (concatenate 'string
                                    (subseq buffer 0 pos)
                                    (string char)
                                    (subseq buffer pos))
                       (1+ pos))
                 (let ((substr-ending-at-pos
                        (concatenate 'string
                                     (subseq buffer
                                             (- pos diff-from-pos) pos)
                                     (string char))))
                   (if-let ((hiragana (hachee.ja:romaji->hiragana
                                       substr-ending-at-pos)))
                     (list (concatenate 'string
                                        (subseq buffer
                                                0 (- pos diff-from-pos))
                                        hiragana
                                        (subseq buffer pos))
                           (+ (- pos diff-from-pos)
                              (length hiragana)))
                     (try-insertion (1- diff-from-pos)))))))
    (try-insertion (min pos 4))))


(defstruct state
  type
  (buffer "")
  (cursor-pos 0))

(defun state-cursor-pos-in-utf-8 (state)
  (length (sb-ext:string-to-octets (subseq (state-buffer state)
                                           0
                                           (state-cursor-pos state))
                                   :external-format :utf-8)))


(defun transit-by-input (state code)
  (when (= code 65293) ;; Enter key
    (setf (state-type state) :committed)
    (return-from transit-by-input state))

  (when (eql (state-type state) :committed)
    (setq state (make-state)))

  (case code
    (65361 ;; Left key
     (when (< 0 (state-cursor-pos state))
       (decf (state-cursor-pos state)))
     state)
    (65363 ;; Right key
     (when (< (state-cursor-pos state)
              (length (state-buffer state)))
       (incf (state-cursor-pos state)))
     state)
    (t
     (destructuring-bind (new-buffer new-pos)
         (buffer-insert-char (state-buffer state)
                             (state-cursor-pos state)
                             (code-char code))
       (setf (state-buffer state) new-buffer)
       (setf (state-cursor-pos state) new-pos))
     state)))


(destructuring-bind (buf pos) (buffer-insert-char "あk" 2 #\a)
  (assert (string= buf "あか"))
  (assert (= pos 2)))

(destructuring-bind (buf pos) (buffer-insert-char "kk" 1 #\a)
  (assert (string= buf "かk"))
  (assert (= pos 1)))
