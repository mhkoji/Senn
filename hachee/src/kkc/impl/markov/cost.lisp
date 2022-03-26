(defpackage :hachee.kkc.impl.markov.cost
  (:use :cl)
  (:export :<-probability
           :->probability))
(in-package :hachee.kkc.impl.markov.cost)

(defvar *log-probability-to-cost-multiple* #x10000)

(defun <-probability (prob)
  (- (* (log prob) *log-probability-to-cost-multiple*)))

(defun ->probability (cost)
  (exp (/ (- cost) *log-probability-to-cost-multiple*)))
