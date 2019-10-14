(defpackage :senn.fcitx.transit.keys
  (:use :cl)
  (:export :make-key
           :key
           :key-sym
           :key-state))
(in-package :senn.fcitx.transit.keys)

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
(defkey down-p 65362)
(defkey right-p 65363)
(defkey up-p 65364)
