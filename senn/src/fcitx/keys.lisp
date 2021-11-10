(defpackage :senn.fcitx.keys
  (:use :cl)
  (:export :make-key
           :key
           :key-sym
           :key-state
           :char-p))
(in-package :senn.fcitx.keys)

(defstruct key sym state)

(defmacro defkey (name sym)
  `(progn
     (defun ,name (k)
       (= (key-sym k) ,sym))
     (export ',name)))

(defkey space-p 32)
(defkey backspace-p 65288)
(defkey enter-p 65293)
(defkey left-p  65361)
(defkey up-p 65362)
(defkey right-p 65363)
(defkey down-p 65364)
(defkey f7-p 65476)
(defkey tab-p 65289)


(defun char-p (k)
  (<= (char-code #\!) (key-sym k) (char-code #\~)))
