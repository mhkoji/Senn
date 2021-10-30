(defpackage :senn.win.keys
  (:use :cl)
  (:export :key
           :make-key
           :key-code
           :alphabet-p
           :number-p))
(in-package :senn.win.keys)

(defstruct key
  code)

(defmacro defkey (name code)
  `(progn
     (defun ,name (k)
       (= (key-code k) ,code))
     (export ',name)))

(defkey backspace-p 8)
(defkey enter-p 13)
(defkey space-p 32)

(defkey left-p 37)
(defkey up-p 38)
(defkey right-p 39)
(defkey down-p 40)

(defkey oem-3-p #xC0)

(defkey oem-1-p #xBA)
(defkey oem-plus-p #xBB)
(defkey oem-comma-p #xBC)
(defkey oem-minus-p #xBD)
(defkey oem-period-p #xBE)
(defkey oem-2-p #xBF)

(defkey oem-4-p #xDB)
(defkey oem-5-p #xDC)
(defkey oem-6-p #xDD)
(defkey oem-7-p #xDE)

(defkey oem-102-p #xE2)

(defun alphabet-p (k)
  (<= (char-code #\A) (key-code k) (char-code #\Z)))

(defun number-p (k)
  (<= (char-code #\0) (key-code k) (char-code #\9)))
